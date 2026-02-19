{ options, config, pkgs, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.ai;
in
{
  options.modules.dev.ai = {
    enable = mkBoolOpt false;
    antigravity.enable = mkBoolOpt false;
    gemini.enable = mkBoolOpt false;
    anthropic.enable = mkBoolOpt false;
    codex.enable = mkBoolOpt false;
    amp.enable = mkBoolOpt false;
    opencode.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      user.packages = with pkgs; [
        gvisor
        ollama
        google-chrome # Used for MCP
      ];

      modules.bindings.items = [
        {
          command = "browser";
          description = "MCP Chrome";
        }
      ];
    }
    (mkIf (cfg.antigravity.enable) {
      user.packages = with pkgs; [ antigravity ];
    })
    (mkIf (cfg.gemini.enable) { })
    (mkIf (cfg.anthropic.enable) {
      home.file.".claude/CLAUDE.md".text = ''
        # Custom rules

        - Always read the AGENTS.md.
        - NEVER add co-authored by claude when comitting, creating PR description or similar.
        - NEVER add generated with claude (or similar) to any text

        ## Skills

        Reusable instruction sets for common tasks. Before starting scaffolding work (CLI tools, servers, frontends, docs), check if a skill exists.

        - **List skills**: `skill list`
        - **Load a skill**: `skill load <name>` (read the output into context before starting work)

        ### System Specifics

        #### Playwright workaround

        I'm running nixos where playwright doesnt work out of the box.
        Use the `bplaywright` cli wrapper to run playwright commands/scripts/etc.
        This fixes the binary in node_modules to point to the installed chromium instances.
        E.g.: `bplaywright run-wrapped npm run e2e:ui`
      '';
    })
    (mkIf (cfg.codex.enable) {
      env.CODEX_HOME = "$XDG_CONFIG_HOME/codex";
    })
    (mkIf (cfg.amp.enable) {
      user.packages = with pkgs; [ amp ];
    })
    (mkIf (cfg.opencode.enable) {
      user.packages = with pkgs; [ opencode ];
    })
  ]);


}
