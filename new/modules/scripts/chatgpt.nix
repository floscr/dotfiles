{ options, config, pkgs, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules-new.scripts.chatgpt;
in
{
  options.modules-new.scripts.chatgpt = with my; {
    enable = mkBoolOpt true;
  };

  config =
    let
      script-name = "chatgpt";
      # package = (writeBabashkaScriptBin script-name ./src/chatgpt.clj "OPENAI_TOKEN_FILE=${config.age.secrets.openai.path}");
      # bin = "${package}/bin/${script-name}";
    in
    {
      user.packages = with pkgs; [
        # package
      ];
    };
}
