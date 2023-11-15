final: previous:
with final;
let
  bb = pkgs.writers.writeBash "nix-bb" ''
    ${pkgs.user.babashka}/bin/bb \
        --deps-root $HOME/.config/dotfiles/new/modules/scripts \
        --config $HOME/.config/dotfiles/new/modules/scripts/bb.edn \
        "$@"
  '';
in
{
  writeBb = name: { content, deps ? [ ] }:
    (writers.makeScriptWriter
      {
        interpreter = bb;
      }
      "/bin/${name}"
      content
    ).overrideAttrs (old: rec {
      buildInputs = deps;
    });
}
