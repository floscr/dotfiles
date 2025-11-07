final: previous:
with final;
let
  bb = pkgs.writers.writeBash "nix-bb" ''
    ${pkgs.babashka}/bin/bb \
        --deps-root $HOME/.config/dotfiles/new/modules/scripts \
        --config $HOME/.config/dotfiles/new/modules/scripts/bb.edn \
        "$@"
  '';
in
{
  writeBb = name: { content, deps ? [ ], env ? {}, useSourcePath ? false }:
    let
      envPrefix = lib.concatStringsSep " " (lib.mapAttrsToList (k: v: "${k}=${v}") env);
      interpreter = if envPrefix == "" then bb else pkgs.writers.writeBash "nix-bb-with-env" ''
        ${envPrefix} ${bb} "$@"
      '';
    in
    if useSourcePath then
      # Create a wrapper that calls the source file directly
      pkgs.writeShellScriptBin name ''
        ${envPrefix} ${bb} ${content} "$@"
      ''
    else
      (writers.makeScriptWriter
        {
          interpreter = interpreter;
        }
        "/bin/${name}"
        content
      ).overrideAttrs (old: rec {
        buildInputs = deps;
      });
}
