{ config, lib, pkgs, ... }:

{
  modules.bindings.items = [
    {
      description = "Nautilus";
      command = "nautilus";
    }
    {
      description = "Playlist: scdl";
      command = "mpv --shuffle --loop-playlist=yes ~/Media/Music/scdl/**";
    }
    {
      description = "Playlist: Das hätt es früher nicht gegeben";
      command = "mpv --shuffle --loop-playlist=yes ~/Media/Music/Das haett es frueher nicht gegeben/**";
    }
  ];

}
