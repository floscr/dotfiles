{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.shell.git-clone;
  hmLib = inputs.home-manager.lib;

  repoType = types.submodule {
    options = {
      url = mkOption {
        type = types.str;
        description = "Git repository URL";
      };
      dir = mkOption {
        type = types.str;
        description = "Target directory (supports $HOME and ~)";
      };
      depth = mkOption {
        type = types.nullOr types.int;
        default = null;
        description = "Clone depth (null for full clone)";
      };
    };
  };

  # Normalize directory path
  normalizePath = dir:
    builtins.replaceStrings ["~"] ["$HOME"] dir;

  # Generate activation script name from directory
  mkActivationName = dir:
    "gitClone-" + (builtins.replaceStrings ["/"] ["-"] (builtins.baseNameOf dir));

  # Generate activation script for a repo
  mkCloneScript = repo:
    let
      dir = normalizePath repo.dir;
      depthArg = if repo.depth != null then "--depth=${toString repo.depth}" else "";
    in
    ''
      if [ ! -d "${dir}" ]; then
        $DRY_RUN_CMD ${pkgs.git}/bin/git clone ${depthArg} ${repo.url} "${dir}"
      fi
    '';

  # Generate all activation scripts as an attrset
  mkActivations = repos:
    builtins.listToAttrs (map (repo: {
      name = mkActivationName repo.dir;
      value = hmLib.hm.dag.entryAfter [ "writeBoundary" ] (mkCloneScript repo);
    }) repos);
in
{
  options.modules.shell.git-clone = {
    repos = mkOption {
      type = types.listOf repoType;
      default = [];
      description = "List of git repositories to clone if they don't exist";
      example = literalExpression ''
        [
          { url = "git@github.com:user/repo.git"; dir = "~/.config/repo"; }
          { url = "https://github.com/user/repo2"; dir = "~/.local/share/repo2"; depth = 1; }
        ]
      '';
    };
  };

  config = mkIf (cfg.repos != []) {
    home-manager.users.${config.user.name}.home.activation = mkActivations cfg.repos;
  };
}
