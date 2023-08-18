{ options, config, pkgs, lib, ... }:

with lib;
let
  cfg = config.modules.desktop.gtk;
in
{
  options.modules.desktop.gtk = {
    enable = my.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.file.".config/gtk-2.0/gtkfilechooser.ini" = {
      text = ''
        [Filechooser Settings]
        LocationMode=path-bar
        ShowHidden=false
        ShowSizeColumn=true
        GeometryX=442
        GeometryY=212
        GeometryWidth=1036
        GeometryHeight=609
        SortColumn=modified
        SortOrder=ascending
        StartupMode=recent
      '';
    };
  };
}
