// Copyright (c) 2024 Stephane Raux. Distributed under the 0BSD license.

use anstyle::AnsiColor;
use clap::{
    parser::ValueSource, ArgAction, ArgMatches, CommandFactory, FromArgMatches, Parser, Subcommand,
};
use freedesktop_desktop_entry::DesktopEntry;
use futures::StreamExt;
use iced::{
    keyboard::{key::Named, Key},
    widget::{scrollable::AbsoluteOffset, text_input, Column, MouseArea, Row, TextInput},
    Element, Font, Length,
};
use itertools::Itertools;
use notify_rust::Notification;
use resvg::{tiny_skia, usvg};
use serde::Deserialize;
use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::{hash_map, HashMap},
    convert::identity,
    env::VarError,
    ffi::OsStr,
    fmt::{self, Display},
    fs,
    future::ready,
    io,
    path::{Path, PathBuf},
    process::Command,
    str::FromStr,
    sync::OnceLock,
    time::Instant,
};
use thiserror::Error;
use tokio::{sync::mpsc, task::spawn_blocking};
use tokio_stream::wrappers::ReceiverStream;
use tracing_subscriber::EnvFilter;
use walkdir::WalkDir;

const APP_NAME: &str = env!("CARGO_PKG_NAME");
const DEFAULT_ICON_SIZE: u32 = 16;
const DEFAULT_MAX_DISTANCE: f64 = 0.4;
const DEFAULT_RESIZABLE: bool = false;
const APP_LIST_SPACING: f32 = 5.0;

static FONT: OnceLock<String> = OnceLock::new();

#[derive(Debug)]
struct App {
    all_candidates: Vec<Candidate>,
    suggestions: Vec<Suggestion>,
    search_pattern: String,
    search_pattern_id: iced::widget::text_input::Id,
    icons: HashMap<String, Option<iced::widget::image::Handle>>,
    request_icon: Option<mpsc::Sender<String>>,
    selection: usize,
    settings: AppSettings,
    app_list_viewport: Option<iced::widget::scrollable::Viewport>,
    app_list_scroll_id: iced::widget::scrollable::Id,
}

impl App {
    fn update_results(&mut self) {
        let target = self.search_pattern.trim().to_lowercase();
        let matches = if target.is_empty() {
            self.all_candidates
                .iter()
                .enumerate()
                .map(|(index, _)| Suggestion {
                    index,
                    matched_keyword: None,
                    distance: StrDistance(0.0),
                })
                .collect::<Vec<_>>()
        } else {
            let mut matches = self
                .all_candidates
                .iter()
                .enumerate()
                .map(|(index, c)| {
                    let closest_keyword = c
                        .keywords
                        .iter()
                        .map(|k| (k, StrDistance::new(&k.to_lowercase(), &target)))
                        .min_by_key(|&(_, d)| d);
                    let name_distance = StrDistance::new(&c.name.to_lowercase(), &target);
                    let (distance, matched_keyword) = match closest_keyword {
                        Some((k, d)) if d < name_distance => (d, Some(k.clone())),
                        _ => (name_distance, None),
                    };
                    Suggestion {
                        index,
                        matched_keyword,
                        distance,
                    }
                })
                .filter(|suggestion| suggestion.distance <= self.settings.max_distance)
                .collect::<Vec<_>>();
            matches.sort_by_key(|m| m.distance);
            matches
        };
        self.suggestions = matches;
        self.selection = 0;
    }

    fn new(app_settings: AppSettings) -> (Self, iced::Task<AppMessage>) {
        let (send_candidate, receive_candidate) = mpsc::channel(100);
        drop(spawn_blocking(|| find_all_candidates(send_candidate)));
        let (send_icon_name, receive_icon_name) = mpsc::channel(100);
        let (send_icon, receive_icon) = mpsc::channel(100);
        drop(spawn_blocking({
            let theme = app_settings.icon_theme.clone();
            move || {
                find_all_icons(
                    theme.as_deref(),
                    app_settings.icon_size,
                    receive_icon_name,
                    send_icon,
                )
            }
        }));
        let search_pattern_id = iced::widget::text_input::Id::unique();
        (
            App {
                all_candidates: Vec::new(),
                suggestions: Vec::new(),
                search_pattern: String::new(),
                search_pattern_id: search_pattern_id.clone(),
                icons: Default::default(),
                request_icon: Some(send_icon_name),
                selection: 0,
                settings: app_settings,
                app_list_viewport: None,
                app_list_scroll_id: iced::widget::scrollable::Id::unique(),
            },
            iced::Task::batch([
                iced::Task::run(
                    ReceiverStream::new(receive_candidate)
                        .map(AppMessage::Candidate)
                        .chain(futures::stream::once(ready(
                            AppMessage::AllCandidatesFetched,
                        ))),
                    identity,
                ),
                iced::Task::run(ReceiverStream::new(receive_icon), |(name, icon)| {
                    AppMessage::Icon { name, icon }
                }),
                iced::widget::text_input::focus(search_pattern_id),
            ]),
        )
    }

    fn title(&self) -> String {
        APP_NAME.into()
    }

    fn update(&mut self, message: AppMessage) -> iced::Task<AppMessage> {
        match message {
            AppMessage::SearchPatternUpdated(s) => {
                self.search_pattern = s;
                self.update_results();
                iced::Task::none()
            }
            AppMessage::Quit => iced::window::get_latest().and_then(iced::window::close),
            AppMessage::Candidate(candidate) => {
                let icon = candidate.icon.clone();
                match self
                    .all_candidates
                    .iter_mut()
                    .find(|c| c.id == candidate.id)
                {
                    Some(c) if c.priority < candidate.priority => *c = candidate,
                    Some(_) => return iced::Task::none(),
                    None => {
                        self.all_candidates.push(candidate);
                    }
                }
                self.update_results();
                let Some((request_icon, icon)) = self.request_icon.clone().zip(icon) else {
                    return iced::Task::none();
                };
                let hash_map::Entry::Vacant(entry) = self.icons.entry(icon) else {
                    return iced::Task::none();
                };
                let name = entry.key().clone();
                entry.insert(None);
                iced::Task::perform(async move { request_icon.send(name).await }, |_| {
                    AppMessage::Ignore
                })
            }
            AppMessage::AllCandidatesFetched => {
                self.request_icon = None;
                iced::Task::none()
            }
            AppMessage::Icon { name, icon } => {
                self.icons.insert(name, Some(icon));
                iced::Task::none()
            }
            AppMessage::Ignore => iced::Task::none(),
            AppMessage::PrevSuggestion => {
                self.selection = self
                    .selection
                    .checked_sub(1)
                    .unwrap_or_else(|| self.suggestions.len().saturating_sub(1));
                self.scroll_app_list_to_selection()
            }
            AppMessage::NextSuggestion => {
                self.selection = self
                    .selection
                    .checked_add(1)
                    .and_then(|i| i.checked_rem(self.suggestions.len()))
                    .unwrap_or(0);
                self.scroll_app_list_to_selection()
            }
            AppMessage::Launch => {
                if let Some(command_options) = self
                    .suggestions
                    .get(self.selection)
                    .map(|m| &self.all_candidates[m.index].command)
                {
                    let mut command = command_options.command();
                    if let Err(e) = command.spawn() {
                        tracing::error!("Failed to launch \"{command:?}\": {e}");
                        let notify_result = Notification::new()
                            .appname(APP_NAME)
                            .summary("Failed to launch app")
                            .body(&format!(
                                "Failed to start app: {e}\nCommand was {command:?}"
                            ))
                            .icon("dialog-error")
                            .show();
                        if let Err(e) = notify_result {
                            tracing::warn!("Failed to send desktop notification: {e}");
                        }
                    }
                }
                iced::window::get_latest().and_then(iced::window::close)
            }
            AppMessage::AppListScrolled(viewport) => {
                self.app_list_viewport = Some(viewport);
                iced::Task::none()
            }
            AppMessage::Select(index) => {
                self.selection = index;
                self.scroll_app_list_to_selection()
            }
            AppMessage::SelectAndLaunch(index) => iced::Task::done(AppMessage::Select(index))
                .chain(iced::Task::done(AppMessage::Launch)),
        }
    }

    fn view(&self) -> iced::Element<'_, AppMessage, iced::Theme, iced::Renderer> {
        let font = self
            .settings
            .font
            .as_ref()
            .map(|_| iced::Font::with_name(FONT.get().unwrap()))
            .unwrap_or_default();
        let keyword_font = iced::Font {
            style: iced::font::Style::Italic,
            ..font
        };
        Element::new(Column::with_children([
            Element::new(
                TextInput::new("App to search for", &self.search_pattern)
                    .id(self.search_pattern_id.clone())
                    .on_input(AppMessage::SearchPatternUpdated)
                    .on_submit(AppMessage::Launch)
                    .icon(text_input::Icon {
                        font: Font::DEFAULT,
                        code_point: '🔍',
                        size: None,
                        spacing: 5.0,
                        side: text_input::Side::Left,
                    }),
            ),
            Element::new(
                iced::widget::scrollable(
                    iced::widget::keyed_column(self.suggestions.iter().enumerate().map(
                        |(i, suggestion)| {
                            let candidate = &self.all_candidates[suggestion.index];
                            let icon = candidate
                                .icon
                                .as_ref()
                                .and_then(|icon| self.icons.get(icon).map(Option::as_ref))
                                .flatten()
                                .map_or_else(
                                    || {
                                        Element::new(iced::widget::Space::new(
                                            self.settings.icon_size as f32,
                                            self.settings.icon_size as f32,
                                        ))
                                    },
                                    |icon| Element::new(iced::widget::image(icon.clone())),
                                );
                            let label = suggestion
                                .matched_keyword
                                .as_ref()
                                .map(|k| {
                                    vec![
                                        Element::new(iced::widget::Space::with_width(10)),
                                        Element::new(iced::widget::text(k).font(keyword_font)),
                                    ]
                                })
                                .unwrap_or_default();
                            let mut row = Element::new(
                                Row::with_children([
                                    icon,
                                    Element::new(iced::widget::text(&candidate.name)),
                                ])
                                .extend(label)
                                .extend([Element::new(iced::widget::Space::with_width(
                                    iced::Length::Fill,
                                ))])
                                .align_y(iced::Alignment::Center)
                                .spacing(5),
                            );
                            if self.selection == i {
                                row = iced::widget::container(row)
                                    .style(|theme: &iced::Theme| iced::widget::container::Style {
                                        text_color: Some(
                                            theme.extended_palette().primary.strong.text,
                                        ),
                                        ..iced::widget::container::background(
                                            theme.extended_palette().primary.strong.color,
                                        )
                                    })
                                    .into();
                            }
                            row = MouseArea::new(row)
                                .on_enter(AppMessage::Select(i))
                                .on_press(AppMessage::SelectAndLaunch(i))
                                .into();
                            (suggestion.index, row)
                        },
                    ))
                    .spacing(APP_LIST_SPACING),
                )
                .id(self.app_list_scroll_id.clone())
                .direction(iced::widget::scrollable::Direction::Vertical(
                    Default::default(),
                ))
                .width(Length::Fill)
                .height(Length::Fill)
                .on_scroll(AppMessage::AppListScrolled),
            ),
        ]))
    }

    fn theme(&self) -> iced::Theme {
        self.settings.theme.0.clone()
    }

    fn subscription(&self) -> iced::Subscription<AppMessage> {
        iced::keyboard::on_key_press(|key, _| match key {
            Key::Named(Named::Escape) => Some(AppMessage::Quit),
            Key::Named(Named::ArrowDown) => Some(AppMessage::NextSuggestion),
            Key::Named(Named::ArrowUp) => Some(AppMessage::PrevSuggestion),
            Key::Named(Named::Enter) => Some(AppMessage::Launch),
            _ => None,
        })
    }

    fn scroll_app_list_to_selection(&self) -> iced::Task<AppMessage> {
        if self.all_candidates.is_empty() {
            return iced::Task::none();
        }
        let Some(viewport) = self.app_list_viewport.as_ref() else {
            return iced::Task::none();
        };
        let row_count = self.all_candidates.len();
        let row_height = (viewport.content_bounds().height
            - (row_count - 1) as f32 * APP_LIST_SPACING)
            / row_count as f32;
        let row_top = (row_height + APP_LIST_SPACING) * self.selection as f32;
        let offset = viewport.absolute_offset().y;
        let dy = Some(row_top - offset).filter(|&dy| dy < 0.0).or(Some(
            row_top + row_height - (offset + viewport.bounds().height),
        )
        .filter(|&dy| dy > 0.0));
        dy.map_or_else(iced::Task::none, |dy| {
            iced::widget::scrollable::scroll_by(
                self.app_list_scroll_id.clone(),
                AbsoluteOffset { x: 0.0, y: dy },
            )
        })
    }
}

#[derive(Clone, Debug)]
enum AppMessage {
    SearchPatternUpdated(String),
    Quit,
    Candidate(Candidate),
    AllCandidatesFetched,
    Icon {
        name: String,
        icon: iced::widget::image::Handle,
    },
    Ignore,
    PrevSuggestion,
    NextSuggestion,
    Launch,
    AppListScrolled(iced::widget::scrollable::Viewport),
    Select(usize),
    SelectAndLaunch(usize),
}

#[derive(Clone, Debug, PartialEq)]
struct Theme(iced::Theme);

impl Theme {
    fn all() -> impl Iterator<Item = Self> {
        iced::Theme::ALL.iter().cloned().map(Self)
    }
}

impl Default for Theme {
    fn default() -> Self {
        Self(iced::Theme::Dark)
    }
}

impl Display for Theme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl FromStr for Theme {
    type Err = UnknownTheme;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        iced::Theme::ALL
            .iter()
            .find(|theme| theme.to_string() == s)
            .cloned()
            .map(Self)
            .ok_or_else(|| UnknownTheme(s.into()))
    }
}

impl<'de> Deserialize<'de> for Theme {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        String::deserialize(deserializer)?
            .parse()
            .map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct AppSettings {
    #[serde(default)]
    font: Option<String>,
    #[serde(default = "default_resizable")]
    resizable: bool,
    #[serde(default)]
    icon_theme: Option<String>,
    #[serde(default = "default_icon_size")]
    icon_size: u32,
    #[serde(default = "default_max_distance")]
    max_distance: StrDistance,
    #[serde(default)]
    theme: Theme,
}

impl AppSettings {
    fn load(path: &Path, must_exist: bool) -> Result<(Self, Vec<String>), AppSettingsError> {
        let config = match fs::read_to_string(path) {
            Ok(config) => Ok(config),
            Err(e) if !must_exist && e.kind() == io::ErrorKind::NotFound => Ok(Default::default()),
            Err(e) => Err(AppSettingsError::Io(path.to_owned(), e)),
        }?;
        let mut ignored_paths = Vec::new();
        serde_ignored::deserialize(toml::Deserializer::new(&config), |path| {
            ignored_paths.push(path.to_string())
        })
        .map(|settings| (settings, ignored_paths))
        .map_err(|e| AppSettingsError::Parse(path.to_owned(), e))
    }
}

impl Default for AppSettings {
    fn default() -> Self {
        Self {
            font: None,
            resizable: false,
            icon_theme: None,
            icon_size: DEFAULT_ICON_SIZE,
            max_distance: default_max_distance(),
            theme: Default::default(),
        }
    }
}

#[derive(Debug, Error)]
enum AppSettingsError {
    #[error("Failed to read {0}: {1}")]
    Io(PathBuf, io::Error),
    #[error("Failed to parse {0}: {1}")]
    Parse(PathBuf, toml::de::Error),
}

fn default_config_path() -> PathBuf {
    dirs::config_dir()
        .map(|p| {
            PathBuf::from_iter([
                p.as_os_str(),
                OsStr::new(APP_NAME),
                OsStr::new("settings.toml"),
            ])
        })
        .unwrap_or_default()
}

const fn default_max_distance() -> StrDistance {
    StrDistance(DEFAULT_MAX_DISTANCE)
}

const fn default_icon_size() -> u32 {
    DEFAULT_ICON_SIZE
}

const fn default_resizable() -> bool {
    DEFAULT_RESIZABLE
}

#[derive(Debug, Error)]
#[error("Unknown theme \"{0}\"")]
struct UnknownTheme(String);

fn terminal_styles() -> clap::builder::Styles {
    clap::builder::Styles::styled()
        .header(AnsiColor::Green.on_default().bold())
        .usage(AnsiColor::Green.on_default().bold())
        .literal(AnsiColor::Blue.on_default().bold())
        .placeholder(AnsiColor::Cyan.on_default().bold())
}

/// Application launcher
///
/// Command-line arguments have precedence over corresponding values in the configuration file.
#[derive(Debug, Parser)]
#[command(about, author, styles(terminal_styles()), version)]
struct AppArgs {
    /// Path to configuration file
    ///
    /// If set to an empty string, no configuration file is read.
    #[arg(long, default_value_os_t = default_config_path())]
    config: PathBuf,
    /// Font for GUI
    #[arg(long)]
    font: Option<String>,
    /// Specify that the window should be resizable
    ///
    /// By default it is not resizable. This may cause the window to float if using a tiling window
    /// manager.
    #[arg(long)]
    resizable: bool,
    /// Specify that the window should not be resizable
    ///
    /// By default it is not resizable. This may cause the window to float if using a tiling window
    /// manager.
    #[arg(long, action = ArgAction::SetFalse, conflicts_with("resizable"))]
    no_resizable: (),
    /// Icon theme name
    #[arg(long)]
    icon_theme: Option<String>,
    /// Icon size in pixels
    #[arg(long, default_value_t = DEFAULT_ICON_SIZE)]
    icon_size: u32,
    /// Maximum string distance between search pattern and suggestion
    ///
    /// Suggestions with a greater distance are filtered out. The distance is in [0, 1].
    #[arg(long, default_value_t = DEFAULT_MAX_DISTANCE)]
    max_distance: f64,
    /// GUI themes
    ///
    /// The `list-themes` subcommand lists available themes.
    #[arg(long, default_value_t)]
    theme: Theme,
    #[command(subcommand)]
    subcommand: Option<AppSubcommand>,
}

#[derive(Debug, Subcommand)]
enum AppSubcommand {
    /// List available themes
    ListThemes,
}

fn main() -> iced::Result {
    let raw_args = AppArgs::command().get_matches();
    let args = match AppArgs::from_arg_matches(&raw_args) {
        Ok(args) => args,
        Err(e) => e.exit(),
    };
    if let Some(AppSubcommand::ListThemes) = args.subcommand {
        Theme::all().for_each(|theme| println!("{theme}"));
        return Ok(());
    }
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_ansi(true)
        .pretty()
        .init();
    let mut app_settings = if args.config.as_os_str().is_empty() {
        Default::default()
    } else {
        let must_exist = !uses_default(&raw_args, "config");
        match AppSettings::load(&args.config, must_exist) {
            Ok((settings, ignored_paths)) => {
                if !ignored_paths.is_empty() {
                    for p in &ignored_paths {
                        tracing::warn!("Unknown setting {p}");
                    }
                    let notif_body =
                        format!("Unknown settings:\n{}", ignored_paths.iter().format("\n"));
                    drop(
                        Notification::new()
                            .appname(APP_NAME)
                            .summary("Unknown settings in configuration")
                            .body(&notif_body)
                            .icon("dialog-warning")
                            .show(),
                    );
                }
                settings
            }
            Err(e) => {
                tracing::error!("{e}");
                Default::default()
            }
        }
    };
    if let Some(font) = args.font {
        app_settings.font = Some(font);
    }
    if !uses_default(&raw_args, "resizable") || !uses_default(&raw_args, "no_resizable") {
        app_settings.resizable = args.resizable;
    }
    if let Some(theme) = args.icon_theme {
        app_settings.icon_theme = Some(theme);
    }
    if !uses_default(&raw_args, "icon_size") {
        app_settings.icon_size = args.icon_size;
    }
    if !uses_default(&raw_args, "max_distance") {
        app_settings.max_distance = StrDistance(args.max_distance);
    }
    if !uses_default(&raw_args, "theme") {
        app_settings.theme = args.theme;
    }
    let mut app = iced::application(App::title, App::update, App::view)
        .resizable(app_settings.resizable)
        .theme(App::theme)
        .subscription(App::subscription);
    if let Some(font) = &app_settings.font {
        app = app.default_font(Font::with_name(FONT.get_or_init(|| font.clone())));
    }
    app.run_with(|| App::new(app_settings))
}

fn uses_default(raw_args: &ArgMatches, name: &str) -> bool {
    raw_args.value_source(name) == Some(ValueSource::DefaultValue)
}

#[derive(Clone, Debug)]
struct Suggestion {
    index: usize,
    matched_keyword: Option<String>,
    distance: StrDistance,
}

#[derive(Clone, Debug)]
struct Candidate {
    id: String,
    name: String,
    keywords: Vec<String>,
    command: CommandOptions,
    priority: usize,
    icon: Option<String>,
}

impl Candidate {
    fn read<P>(path: P, priority: usize) -> Result<Self, DesktopEntryError>
    where
        P: AsRef<Path>,
    {
        let entry = DesktopEntry::from_path::<&str>(path.as_ref(), None)?;
        let name = entry
            .name::<&str>(&[])
            .ok_or(DesktopEntryError::NoName)?
            .into_owned();
        let command = entry.exec().ok_or(DesktopEntryError::NoCommand)?.parse()?;
        Ok(Candidate {
            id: entry.id().into(),
            name,
            keywords: entry
                .generic_name::<&str>(&[])
                .into_iter()
                .chain(entry.keywords::<&str>(&[]).unwrap_or_default())
                .chain(
                    entry
                        .categories()
                        .unwrap_or_default()
                        .into_iter()
                        .map(Cow::Borrowed),
                )
                .map(Cow::into_owned)
                .collect(),
            command,
            priority,
            icon: entry.icon().map(ToOwned::to_owned),
        })
    }
}

#[derive(Clone, Debug)]
struct CommandOptions {
    program: String,
    args: Vec<String>,
}

impl CommandOptions {
    fn command(&self) -> Command {
        let mut command = Command::new(&self.program);
        command.args(&self.args);
        command
    }
}

impl FromStr for CommandOptions {
    type Err = DesktopEntryError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s
            .split(' ')
            .filter(|s| !s.is_empty() && !s.starts_with('%'));
        let Some(program) = parts.next() else {
            return Err(DesktopEntryError::NoProgramInCommand);
        };
        Ok(Self {
            program: program.to_owned(),
            args: parts.map(ToOwned::to_owned).collect(),
        })
    }
}

#[derive(Debug, Error)]
enum DesktopEntryError {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    Decoding(#[from] freedesktop_desktop_entry::DecodeError),
    #[error("Missing name")]
    NoName,
    #[error("Missing command")]
    NoCommand,
    #[error("Missing program in Exec command")]
    NoProgramInCommand,
}

const DEFAULT_XDG_DATA_DIRS: &str = "/usr/local/share:/usr/share";

fn default_xdg_data_dirs(reason: &str) -> String {
    let dirs = DEFAULT_XDG_DATA_DIRS;
    tracing::warn!("{reason}; defaulting to {dirs}");
    dirs.into()
}

fn find_all_candidates(sender: mpsc::Sender<Candidate>) {
    let t0 = Instant::now();
    let dirs = match std::env::var("XDG_DATA_DIRS") {
        Ok(dirs) => {
            tracing::debug!("XDG_DATA_DIRS={dirs}");
            if dirs.is_empty() {
                default_xdg_data_dirs("XDG_DATA_DIRS is empty")
            } else {
                dirs
            }
        }
        Err(VarError::NotPresent) => default_xdg_data_dirs("XDG_DATA_DIRS is not set"),
        Err(VarError::NotUnicode(_)) => default_xdg_data_dirs("XDG_DATA_DIRS is not valid Unicode"),
    };
    dirs.split(':')
        .map(Path::new)
        .map(|p| p.join("applications"))
        .filter(|p| p.exists())
        .enumerate()
        .map(|(i, p)| (usize::MAX - i, p))
        .flat_map(|(priority, p)| {
            WalkDir::new(p)
                .follow_links(true)
                .into_iter()
                .filter_map(move |entry| Some(priority).zip(entry.ok()))
        })
        .filter(|(_, entry)| {
            !entry.file_type().is_dir()
                && entry.path().extension().is_some_and(|ext| ext == "desktop")
        })
        .filter_map(
            |(priority, entry)| match Candidate::read(entry.path(), priority) {
                Ok(candidate) => Some(candidate),
                Err(e) => {
                    tracing::warn!("Error processing {}: {e}", entry.path().display());
                    None
                }
            },
        )
        .try_for_each(|candidate| sender.blocking_send(candidate))
        .ok();
    let elapsed = t0.elapsed();
    tracing::info!("Fetching all desktop entries took {elapsed:?}");
}

fn find_all_icons(
    icon_theme: Option<&str>,
    icon_size: u32,
    mut icon_names: mpsc::Receiver<String>,
    icons: mpsc::Sender<(String, iced::widget::image::Handle)>,
) {
    std::iter::from_fn(|| icon_names.blocking_recv())
        .filter_map(|name| find_icon(icon_theme, icon_size, &name).map(|icon| (name, icon)))
        .try_for_each(|named_icon| icons.blocking_send(named_icon))
        .ok();
}

fn find_icon(
    icon_theme: Option<&str>,
    icon_size: u32,
    name: &str,
) -> Option<iced::widget::image::Handle> {
    let path = match Path::new(name).extension().and_then(|ext| ext.to_str()) {
        Some("svg") | Some("png") => PathBuf::from(name),
        _ => {
            let mut search = freedesktop_icons::lookup(name)
                .with_size(icon_size as u16)
                .force_svg();
            if let Some(theme) = icon_theme {
                search = search.with_theme(theme);
            }
            search.find()?
        }
    };
    let pixels = if path.extension().is_some_and(|ext| ext == "svg") {
        load_svg_icon(&path, icon_size)
    } else {
        load_bmp_icon(&path, icon_size)
    };
    let pixels = match pixels {
        Ok(pixels) => Some(pixels),
        Err(e) => {
            tracing::warn!("Failed to load icon {}: {e}", path.display());
            None
        }
    }?;
    Some(iced::widget::image::Handle::from_rgba(
        icon_size, icon_size, pixels,
    ))
}

fn load_svg_icon(path: &Path, size: u32) -> Result<Vec<u8>, LoadIconError> {
    let svg_bytes = fs::read(path)?;
    let svg = usvg::Tree::from_data(&svg_bytes, &Default::default())?;
    let transform = icon_transform(svg.size().width(), svg.size().height(), size as f32);
    let mut pixmap = tiny_skia::Pixmap::new(size, size).ok_or(LoadIconError::OpenSkiaPixmap)?;
    resvg::render(&svg, transform, &mut pixmap.as_mut());
    Ok(pixmap.take())
}

fn load_bmp_icon(path: &Path, size: u32) -> Result<Vec<u8>, LoadIconError> {
    let icon = image::open(path)?;
    let w = icon.width();
    let h = icon.height();
    let icon = icon.into_rgba8().into_raw();
    if w == size && h == size {
        return Ok(icon);
    }
    let transform = icon_transform(w as f32, h as f32, size as f32);
    let mut pixmap = tiny_skia::Pixmap::new(size, size).ok_or(LoadIconError::OpenSkiaPixmap)?;
    pixmap.draw_pixmap(
        0,
        0,
        tiny_skia::PixmapRef::from_bytes(&icon, w, h).ok_or(LoadIconError::OpenSkiaPixmap)?,
        &tiny_skia::PixmapPaint {
            opacity: 1.0,
            blend_mode: tiny_skia::BlendMode::SourceOver,
            quality: tiny_skia::FilterQuality::Bicubic,
        },
        transform,
        None,
    );
    Ok(pixmap.take())
}

fn icon_transform(source_w: f32, source_h: f32, target_size: f32) -> tiny_skia::Transform {
    let scale = (target_size / source_w).min(target_size / source_h);
    let dx = (target_size - source_w * scale) / 2.0;
    let dy = (target_size - source_h * scale) / 2.0;
    tiny_skia::Transform::identity()
        .post_scale(scale, scale)
        .post_translate(dx, dy)
}

#[derive(Debug, Error)]
enum LoadIconError {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    Svg(#[from] usvg::Error),
    #[error(transparent)]
    Bmp(#[from] image::ImageError),
    #[error("Failed to create skia pixmap")]
    OpenSkiaPixmap,
}

#[derive(Clone, Copy, Debug, Deserialize)]
struct StrDistance(f64);

impl StrDistance {
    fn new(a: &str, b: &str) -> Self {
        Self(1.0 - strsim::jaro_winkler(a, b))
    }
}

impl PartialEq for StrDistance {
    fn eq(&self, other: &Self) -> bool {
        self.0.total_cmp(&other.0) == Ordering::Equal
    }
}

impl Eq for StrDistance {}

impl PartialOrd for StrDistance {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StrDistance {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.total_cmp(&other.0)
    }
}
