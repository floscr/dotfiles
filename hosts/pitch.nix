{ config, options, lib, pkgs, my, ... }:

with lib;
with lib.my;
{
  user.packages = with pkgs; [
    nodePackages.mermaid-cli # Mermaid Diagrams CLI, mmdc
    google-chrome # For cypress
    epiphany
  ];

  modules = {
    dev = {
      clojure.enable = true;
    };
    desktop.apps = {
      zoom.enable = false;
    };
  };

  home-manager.users.${config.user.name}.programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      # Clojure
      betterthantomorrow.calva

      # VSpaceCode
      bodil.file-browser
      kahole.magit
      vscodevim.vim
      vspacecode.vspacecode
      vspacecode.whichkey
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "fuzzy-search";
        publisher = "jacobdufault";
        version = "0.0.3";
        sha256 = "sha256-oN1SzXypjpKOTUzPbLCTC+H3I/40LMVdjbW3T5gib0M=";
      }
      {
        name = "fuzzy-search";
        publisher = "jacobdufault";
        version = "0.0.3";
        sha256 = "sha256-oN1SzXypjpKOTUzPbLCTC+H3I/40LMVdjbW3T5gib0M=";
      }
      {
        name = "vscode-parinfer";
        publisher = "shaunlebron";
        version = "0.6.0";
        sha256 = "sha256-AMk7GOU1GbANOzACj2Q+vctSMb5zIVSYz4yE5JxFoCI=";
      }
    ];
    # userSettings = with builtins; fromJSON (readFile ./settings.json);
    # keybindings = with builtins; fromJSON (readFile ./keybindings.json);
  };

  environment.shellAliases = {
    specify = "npx @specifyapp/cli";
  };

  modules.bindings.items = [
    {
      description = "Epiphany (Webkit)";
      command = "${pkgs.epiphany}/bin/epiphany";
    }
  ];
}
