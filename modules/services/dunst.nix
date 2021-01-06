# To mount android devices
{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.dunst;
in {
  options.modules.services.dunst = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      dunst
      libnotify
    ];
    home.configFile."dunst/dunstrc".text = ''
      [global]
        browser = ${pkgs.chromium}/bin/chromium
        dmenu = ${pkgs.rofi}/bin/rofi -dmenu -p dunst:
        follow = none
        history_length = 20
        idle_threshold = 120
        monitor = 0
        show_age_threshold = 60
        show_indicators = true
        shrink = false
        sort = true
        startup_notification = false
        sticky_history = true

        ## UI: Notification
        # geometry [{width}]x{height}][+/-{x}+/-{y}]
        geometry = 365x15-21+21
        alignment = left
        frame_width = 2
        frame_color = #1a1c25
        horizontal_padding = 20
        padding = 20
        icon_position = right
        separator_color = #1a1c25
        separator_height = 2
        transparency = 1
        max_icon_size = 64

        ## UI: Text
        # Don't bouce wrapped text
        markup = full
        # https://github.com/dwarmstrong/dotfiles/blob/master/.config/dunst/dunstrc#L40
        bounce_freq = 0
        format = "<b>%s</b>\n%b"
        # font = "Fira Sans 11"
        font = Iosevka 12
        line_height = 0
        ignore_newline = false
        indicate_hidden = true
        word_wrap = true
        mouse_left_click = close_current
        mouse_middle_click = do_action
        mouse_right_click = close_all

      [shortcuts]
        close = shift+space
        close_all = ctrl+shift+space
        history = ctrl+period
        context = ctrl+shift+period

      [urgency_low]
        background = "#1E2029"
        foreground = "#bbc2cf"
        timeout = 8
      [urgency_normal]
        background = "#2a2d39"
        foreground = "#bbc2cf"
        timeout = 14
      [urgency_critical]
        background = "#cc6666"
        foreground = "#1E2029"
        timeout = 0
      '';
      onChange = ''
        pkillVerbose=""
        if [[ -v VERBOSE ]]; then
          pkillVerbose="-e"
        fi
        $DRY_RUN_CMD ${pkgs.procps}/bin/pkill -u $USER $pkillVerbose dunst || true
        unset pkillVerbose
    '';

    systemd.user.services.dunst = {
      enable = true;
      description = "Notification daemon";
      after = [ "graphical-session-pre.target" ];
      wantedBy = [ "graphical-session-pre.target" ];
      partOf = [ "graphical-session-pre.target" ];
      path = [ pkgs.dunst ];
      serviceConfig = {
        Restart = "always";
        RestartSec = 2;
        ExecStart = "${pkgs.dunst}/bin/dunst";
      };
    };
  };
}
