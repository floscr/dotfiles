{ options, config, pkgs, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.pi;
  jsonFormat = pkgs.formats.json { };

  # Resolve the real path to this module's directory for out-of-store symlinks.
  # This means edits to prompts/skills/extensions take effect immediately
  # without a nixos-rebuild.
  piModuleDir = "${config.dotfiles.dir}/new/modules/dev/pi";

  settings = {
    lastChangelogVersion = cfg.version;
    defaultProvider = cfg.defaultProvider;
    defaultModel = cfg.defaultModel;
    defaultThinkingLevel = cfg.defaultThinkingLevel;
  } // cfg.extraSettings;

  keybindings = {
    cursorUp = [ "up" ];
    cursorDown = [ "down" ];
    cursorLeft = [ "left" ];
    cursorRight = [ "right" ];
    toggleThinking = [ "ctrl+t" ];
  } // cfg.extraKeybindings;

  defaultPiPackage = pkgs.llm-agents.pi;

  # Check directories for real content (ignoring .gitkeep)
  hasRealFiles = dir:
    builtins.pathExists dir &&
    (filterAttrs (n: _: n != ".gitkeep") (builtins.readDir dir)) != { };
in
{
  options.modules.dev.pi = {
    enable = mkBoolOpt false;

    package = mkOption {
      type = types.package;
      default = defaultPiPackage;
      description = "The pi-coding-agent package to use.";
    };

    version = mkOption {
      type = types.str;
      default = defaultPiPackage.version;
      description = "Pi version for lastChangelogVersion in settings.";
    };

    defaultProvider = mkOption {
      type = types.str;
      default = "anthropic";
      description = "Default AI provider.";
    };

    defaultModel = mkOption {
      type = types.str;
      default = "claude-sonnet-4-20250514";
      description = "Default model to use.";
    };

    defaultThinkingLevel = mkOption {
      type = types.str;
      default = "high";
      description = "Default thinking level (off, minimal, low, medium, high, xhigh).";
    };

    extraSettings = mkOption {
      type = types.attrs;
      default = { };
      description = "Additional settings to merge into settings.json.";
    };

    extraKeybindings = mkOption {
      type = types.attrs;
      default = { };
      description = "Additional keybindings to merge into keybindings.json.";
    };
  };

  config = mkIf cfg.enable {
    user.packages = [ cfg.package ];

    home.file = {
      # Settings & keybindings (nix-generated, goes through the store)
      ".pi/agent/settings.json".source = jsonFormat.generate "pi-settings.json" settings;
      ".pi/agent/keybindings.json".source = jsonFormat.generate "pi-keybindings.json" keybindings;
    }
    # Extensions — copied via nix store (immutable, requires nixos-rebuild)
    // (optionalAttrs (hasRealFiles ./extensions) {
      ".pi/agent/extensions".source = ./extensions;
    });

    # Prompts & skills — direct symlinks to dotfiles repo.
    # Edits take effect immediately without nixos-rebuild.
    system.userActivationScripts.pi-agent-symlinks = ''
      PI_DIR="${config.user.home}/.pi/agent"
      MODULE_DIR="${piModuleDir}"

      mkdir -p "$PI_DIR"

      for dir in prompts skills; do
        target="$MODULE_DIR/$dir"
        link="$PI_DIR/$dir"

        # Remove existing (symlink, file, or dir) so we can recreate
        [ -e "$link" ] || [ -L "$link" ] && rm -rf "$link"

        # Only link if the source dir has real files (not just .gitkeep)
        if [ -d "$target" ] && [ "$(find "$target" -not -name .gitkeep -not -path "$target" | head -1)" ]; then
          ln -sfn "$target" "$link"
        fi
      done
    '';
  };
}
