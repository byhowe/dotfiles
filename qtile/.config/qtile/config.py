import subprocess
from typing import Any

from libqtile import bar, layout, widget, hook
from libqtile.config import (
    Click,
    Drag,
    Group,
    Key,
    KeyChord,
    Match,
    Screen,
    DropDown,
    ScratchPad,
)
from libqtile.lazy import lazy

mod = "mod4"
bar_size = 18
terminal = "alacritty"
font = "FiraCode Nerd Font"
font_size = 12

Workspace = tuple[str, dict[str, Any]]
workspaces: list[Workspace] = [
    ("I", { "layout": "stack" }),
    ("II", { "layout": "stack" }),
    ("III", { "layout": "stack" }),
    ("IV", { "layout": "stack" }),
    ("V", { "layout": "stack" }),
    ("VI", { "layout": "stack" }),
    ("VII", { "layout": "stack" }),
    ("VIII", { "layout": "stack" }),
    ("IX", { "layout": "stack" }),
]

browser = "firefox-developer-edition -P 'arkenfox-{profile}' -new-window"
applications = [
    ("a", "alacritty", "Launch terminal"),
    (
        "w",
        browser.format(profile="vpn"),
        "Launch web browser through VPN",
    ),
    (
        "o",
        f"mullvad-exclude {browser.format(profile='clear')}",
        "Launch web browser through split tunneling",
    ),
    ("e", "emacs", "Launch editor"),
    ("d", "pcmanfm", "Launch file manager"),
    ("r", "rofi -show run", "Launch run menu"),
    ("p", "rofi -show drun", "Launch app menu"),
    ("v", "rofi -show window", "Show available windows"),
]

dropdown_settings = {
    "x": 0.05,
    "y": 0.05,
    "width": 0.90,
    "height": 0.90,
    "opacity": 1,
}

# (key, command, settings, description)
scratchpads = [
    ("a", "alacritty", {
        **dropdown_settings,
        "width": 0.46,
        "x": 0.02,
    }, "Launch terminal in a scratchpad"),
    ("z", "alacritty", {
        **dropdown_settings,
        "width": 0.46,
        "x": 0.52,
    }, "Launch terminal in a scratchpad"),
    ("s", "alacritty -e pulsemixer", {**dropdown_settings}, "Launch sound mixer in a scratchpad"),
    ("d", "alacritty -e ranger", {**dropdown_settings}, "Launch file manager in a scratchpad"),
    ("m", "alacritty -e ncmpcpp", {
        **dropdown_settings,
        "x": 0.3,
        "width": 0.4,
    }, "Launch music player in a scratchpad"),
    ("p", "alacritty -e htop", {**dropdown_settings}, "Launch system monitor in a scratchpad"),
]

keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod], "h", lazy.layout.previous(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.next(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    # Switch between screens
    Key([mod], "Tab", lazy.next_screen(), desc="Move focus to the next screen"),
    Key([mod, "shift"], "Tab", lazy.prev_screen(), desc="Move focus to the previous screen"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.client_to_previous(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.client_to_next(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "shift"], "Return", lazy.layout.toggle_split(), desc="Toggle between split and unsplit sides of stack"),
    # Toggle floating window
    Key([mod], "t", lazy.window.toggle_floating(), desc="Toggle the floating state of the window"),
    # Toggle fullscreen
    Key([mod], "f", lazy.window.toggle_fullscreen(), desc="Toggle fullscreen"),
    # Toggle between different layouts as defined below
    Key([mod], "i", lazy.next_layout(), desc="Toggle between layouts"),
    # management
    Key([mod, "shift"], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "shift", "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "shift", "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "s", lazy.spawn("slock"), desc="Screen lock"),
    # backlight
    Key([], "XF86MonBrightnessDown", lazy.spawn("xbacklight -dec 5"), desc="Lower screen brightness"),
    Key([], "XF86MonBrightnessUp", lazy.spawn("xbacklight -inc 5"), desc="Raise screen brightness"),
    # multimedia
    Key([], "XF86AudioMute", lazy.spawn("amixer set Master toggle"), desc="Mute or unmute sound"),
    Key([], "XF86AudioLowerVolume", lazy.spawn("amixer set Master 5%- unmute"), desc="Lower volume"),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer set Master 5%+ unmute"), desc="Raise volume"),
    Key([mod], "XF86AudioLowerVolume", lazy.spawn("mpc volume -5"), desc="Lower mpd volume"),
    Key([mod], "XF86AudioRaiseVolume", lazy.spawn("mpc volume +5"), desc="Raise mpd volume"),
    Key([mod], "XF86AudioPrev", lazy.spawn("mpc seekthrough -00:00:05"), desc="Seek mpd 5 seconds back"),
    Key([mod], "XF86AudioNext", lazy.spawn("mpc seekthrough +00:00:05"), desc="Seek mpd 5 seconds forward"),
    Key([mod], "Left", lazy.spawn("mpc seekthrough -00:00:05"), desc="Seek music 5 seconds back"),
    Key([mod], "Right", lazy.spawn("mpc seekthrough +00:00:05"), desc="Seek music 5 seconds forward"),
    Key([], "XF86AudioPrev", lazy.spawn("mpc prev"), desc="Play previous mpd track"),
    Key([], "XF86AudioNext", lazy.spawn("mpc next"), desc="Play next mpd track"),
    Key([], "XF86AudioPlay", lazy.spawn("mpc toggle"), desc="Play/pause mpd track"),
    Key([], "XF86AudioStop", lazy.spawn("mpc stop"), desc="Stop mpd track"),
    Key([mod], "Down", lazy.spawn("mpc prev"), desc="Play previous mpd track"),
    Key([mod], "Up", lazy.spawn("mpc next"), desc="Play next mpd track"),
    Key([mod, "control"], "Up", lazy.spawn("mpc toggle"), desc="Play/pause mpd track"),
    Key([mod, "control"], "Down", lazy.spawn("mpc stop"), desc="Stop mpd track"),
    KeyChord(
        ["control"],
        "m",
        [
            Key([], "r", lazy.spawn("mpc repeat"), desc="Toggle mpd repeat mode"),
            Key([], "z", lazy.spawn("mpc random"), desc="Toggle mpd random mode"),
            Key([], "y", lazy.spawn("mpc repeat"), desc="Toggle mpd single mode"),
            Key(["shift"], "r", lazy.spawn("mpc consume"), desc="Toggle mpd consume mode"),
            Key([], "p", lazy.spawn("mpc toggle"), desc="Toggle mpd"),
            Key([], "n", lazy.spawn("mpc next"), desc="Play next mpd track"),
            Key([], "b", lazy.spawn("mpc prev"), desc="Play previous mpd track"),
        ],
    ),
]

for (k, cmd, desc) in applications:
    keys.append(Key(
        [mod],
        k,
        lazy.spawn(cmd),
        desc=desc,
    ))

groups = []
for i, (ws, settings) in enumerate(workspaces, 1):
    group = Group(name=ws, **settings)
    groups.append(group)
    keys.extend([
        # mod1 + letter of group = switch to group
        Key(
            [mod],
            str(i),
            lazy.group[group.name].toscreen(),
            desc="Switch to group {}".format(group.name),
        ),
        # mod1 + shift + letter of group = move focused window to group
        Key(
            [mod, "shift"],
            str(i),
            lazy.window.togroup(group.name, switch_group=False),
            desc="Move focused window to group {}".format(group.name),
        ),
    ])

dropdowns = []
for (k, cmd, settings, desc) in scratchpads:
    sp_name = "ScratchPad{}".format(k)
    dropdowns.append(DropDown(sp_name, cmd, **settings))
    keys.append(Key([mod, "shift"], k, lazy.group["scratchpad"].dropdown_toggle(sp_name), desc=desc))
groups.append(ScratchPad("scratchpad", dropdowns))

layout_theme = {
    "margin": 0,
    "border_width": 2,
    "border_focus": "#63F2F1",
    "border_normal": "#12111E",
}
layouts = [
    # layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=4),
    layout.Max(**layout_theme),
    # Try more layouts by unleashing below layouts.
    layout.Stack(**layout_theme, num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = {
    "font": font,
    "fontsize": font_size,
    "padding": 5,
    "foreground": "#F8F8F2",
    "background": "#272A5B",
}
extension_defaults = { **widget_defaults }

sep = lambda: widget.Sep(linewidth=0, padding=6, background="#12111E")
group_box = lambda: (widget.GroupBox(
    fontsize=10,
    margin_y=3,
    margin_x=0,
    padding_y=5,
    padding_x=2,
    borderwidth=3,
    active="#F8F8F2",
    inactive="#F8F8F2", # not important since hide_unused=True
    # highlight_color="#FF79C6", # used with highlight_method="line"
    this_current_screen_border="#FF5555",
    this_screen_border="#FF79C6",
    other_current_screen_border="#FF5555",
    # other_screen_border="#6C71C4",
    # foreground="#1E1C31",
    rounded=False,
    hide_unused=True,
    highlight_method="block",
))
window_name = lambda: (widget.WindowName(
    foreground="#C991E1",
    background="#12111E",
    padding=0,
))
mpd = lambda: widget.Mpd2(play_states={"pause": "", "play": "", "stop": ""})
net = lambda: widget.Net(format="{down:6.2f}{down_suffix:<2}   {up:6.2f}{up_suffix:<2}")
cpu = lambda: widget.CPU(format=" {load_percent:2.1f}%")
mem = lambda: widget.Memory(format="{MemUsed: .0f}{ms}")
vol = lambda: (widget.Volume(
    fmt=" {}",
    step=5,
))
bat = lambda: (widget.Battery(
    format="{char} {percent:2.0%} {hour:d}:{min:02d}",
    charge_char="",
    full_char=" ",
    discharge_char=" ",
    empty_char=" ",
    show_short_text=False,
))
cur_lay = lambda: widget.CurrentLayout()
clock = lambda: widget.Clock(format="%d/%m/%y %H:%M")
updates = lambda: widget.CheckUpdates(display_format=" {Updates}")
systray = lambda: widget.Systray(background="#12111E")

gpu_icon = lambda: None
gpu_text = lambda: None
try:
    from optimus_manager.var import load_state
    from optimus_manager.client.error_reporting import report_errors
    optimus = load_state()
    fatal = report_errors(optimus)
    if not fatal:
        gpu = optimus["current_mode"]
        gpu_icon_filename = f"~/.config/qtile/prime-tray-{gpu}-symbolic.svg"

        gpu_icon = lambda: widget.Image(filename=gpu_icon_filename)
        gpu_text = lambda: widget.TextBox(gpu)

except ImportError:
    gpu_text = lambda: widget.TextBox("optimus-manager: ImportError")
except:
    gpu_text = lambda: widget.TextBox("optimus-manager: Error")

# get the number of monitors.
monitors = int(
    subprocess.check_output(
        "xrandr --listactivemonitors | awk 'NR==1 {print $2}'", shell=True
    ).decode()
)

def construct_bar(screen_num: int):
    widgets = [
        sep(),
        group_box(),
        sep(),
        window_name(),
        mpd() if screen_num == 0 else None,
        sep() if screen_num == 0 else None,
        net(),
        sep(),
        cpu(),
        sep(),
        mem(),
        sep(),
        vol(),
        # sep(),
        # updates(),
        sep(),
        cur_lay(),
        sep(),
        bat(),
        sep(),
        clock(),
        sep() if screen_num == 0 else None,
        gpu_icon() if screen_num == 0 else None,
        gpu_text() if screen_num == 0 else None,
        systray() if screen_num == 0 else None,
        sep(),
    ]
    return bar.Bar(
        widgets=[item for item in widgets if item is not None],
        size=bar_size,
    )

screens = [
    Screen(
        top=construct_bar(i),
        wallpaper='~/.config/qtile/wallpaper.jpg',
        wallpaper_mode='fill'
    ) for i in range(monitors)
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

@hook.subscribe.client_focus
def focused_client_bring_to_top(window):
    window.bring_to_front()

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    **(layout_theme | {"border_focus": "#FF8080"}),
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(wm_class="pinentry-gtk-2"),  # GPG key password entry
        Match(wm_class="Steam"),  # Steam
        Match(wm_class="bitwarden"),  # Bitwarden
        Match(wm_class="feh"),  # feh
        Match(wm_class="Tor Browser"),  # Tor browser
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
