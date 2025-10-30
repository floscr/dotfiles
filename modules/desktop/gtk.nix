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

  config = mkIf cfg.enable (let
    fileChooserSettings = ''
      LocationMode=path-bar
      ShowHidden=false
      ShowSizeColumn=true
      SortColumn=modified
      SortOrder=descending
      StartupMode=recent
    '';
  in {
    home.configFile = {
      "gtk-2.0/gtkfilechooser.ini".text = ''
        [Filechooser Settings]
        ${fileChooserSettings}
        GeometryX=442
        GeometryY=212
        GeometryWidth=1036
        GeometryHeight=609
      '';
      
      "gtk-3.0/bookmarks".text = ''
        file://${homeDir}/Downloads
        file://${homeDir}/Documents
        file://${homeDir}/Media/Screencapture
      '';
      
      "gtk-3.0/settings.ini".text = ''
        [Settings]
        gtk-recent-files-max-age=0
        gtk-recent-files-enabled=true

        [Filechooser Settings]
        ${fileChooserSettings}
      '';
    };
  });
}
