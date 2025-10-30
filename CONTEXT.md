# Document Title

## Set environment variables via `env`

```nix
    env.ADBLOCK = "1";
```

## Reference the home directory via `homeDir`

```nix
{ options, config, pkgs, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.gtk;
in
{
  options.modules.desktop.gtk = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.configFile = {
      "gtk-3.0/bookmarks".text = ''
        file://${homeDir}/Downloads
      '';
    };
  };
}
```


