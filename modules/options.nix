{ config, options, lib, home-manager, ... }:

with lib;
with lib.my;
{
  options = with types; {
    user = mkOpt attrs { };

    username = mkStrOpt "floscr";
    email = mkStrOpt "";
    workEmail = mkStrOpt "";

    dotfiles = {
      dir = mkOpt path
        (findFirst pathExists (toString ../.) [
          "${config.user.home}/.config/dotfiles"
          "/etc/dotfiles"
        ]);
      binDir = mkOpt path "${config.dotfiles.dir}/bin";
      configDir = mkOpt path "${config.dotfiles.dir}/config";
      modulesDir = mkOpt path "${config.dotfiles.dir}/modules";
      themesDir = mkOpt path "${config.dotfiles.modulesDir}/themes";
    };

    home = {
      file = mkOpt' attrs { } "Files to place directly in $HOME";
      configFile = mkOpt' attrs { } "Files to place in $XDG_CONFIG_HOME";
      dataFile = mkOpt' attrs { } "Files to place in $XDG_DATA_HOME";
      defaultApplications = mkOpt' attrs { } "XDG/MIME default applications";
      desktopEntries = mkOpt' attrs { } "XDG/MIME desktop entries";
    };

    env = mkOption {
      type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
      apply = mapAttrs
        (n: v:
          if isList v
          then concatMapStringsSep ":" (x: toString x) v
          else (toString v));
      default = { };
      description = "TODO";
    };
  };

  config = {
    user =
      let
        user = builtins.getEnv "USER";
        name = if elem user [ "" "root" ] then "floscr" else user;
      in
      {
        inherit name;
        description = "The primary user account";
        extraGroups = [ "wheel" ];
        isNormalUser = true;
        home = "/home/${name}";
        group = "users";
        uid = 1000;
      };

    home-manager = {
      useUserPackages = true;

      backupFileExtension = "back";

      # I only need a subset of home-manager's capabilities. That is, access to
      # its home.file, home.xdg.configFile and home.xdg.dataFile so I can deploy
      # files easily to my $HOME, but 'home-manager.users.hlissner.home.file.*'
      # is much too long and harder to maintain, so I've made aliases in:
      #
      #   home.file        ->  home-manager.users.hlissner.home.file
      #   home.configFile  ->  home-manager.users.hlissner.home.xdg.configFile
      #   home.dataFile    ->  home-manager.users.hlissner.home.xdg.dataFile
      users.${config.user.name} = {
        home = {
          file = mkAliasDefinitions options.home.file;
          # Necessary for home-manager to work with flakes, otherwise it will
          # look for a nixpkgs channel.
          stateVersion = config.system.stateVersion;
        };
        xdg = {
          configFile = mkAliasDefinitions options.home.configFile;
          dataFile = mkAliasDefinitions options.home.dataFile;

          desktopEntries = mkAliasDefinitions options.home.desktopEntries;

          mime.enable = true;
          mimeApps = {
            enable = true;
            defaultApplications =
              mkAliasDefinitions options.home.defaultApplications;
          };
        };
      };
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;

    nix.settings = let users = [ "root" config.user.name ]; in
      {
        trusted-users = users;
        allowed-users = users;
      };

    # must already begin with pre-existing PATH. Also, can't use binDir here,
    # because it contains a nix store path.
    env.PATH = [ "$XDG_CONFIG_HOME/dotfiles/bin" "$PATH" ];

    environment.extraInit =
      concatStringsSep "\n"
        (mapAttrsToList (n: v: "export ${n}=\"${v}\"") config.env);
  };
}
