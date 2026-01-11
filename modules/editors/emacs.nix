{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.emacs;
  emacsPkg = (pkgs.emacsPackagesFor pkgs.unstable.emacs).emacsWithPackages
    (epkgs: with epkgs; [
      treesit-grammars.with-all-grammars
      vterm
      pdf-tools
      mu4e
    ]);
in
{
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    enableServer = mkBoolOpt false;
    enableMail = mkBoolOpt false;
    package = mkOption {
      type = types.package;
      default = emacsPkg;
      description = "The Emacs package to use";
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    user.packages = with pkgs; [
      ## Emacs itself
      binutils # native-comp needs 'as', provided by this

      emacsPkg

      parinfer-rust-emacs

      ## Doom dependencies
      git
      (ripgrep.override { withPCRE2 = true; })
      gnutls # for TLS connectivity

      ## Optional dependencies
      fd # faster projectile indexing
      imagemagickBig # for image-dired

      (mkIf (cfg.enableMail) mu)
      (mkIf (cfg.enableMail) isync)

      (mkIf (config.programs.gnupg.agent.enable)
        pinentry-emacs) # in-emacs gnupg prompts
      zstd # for undo-fu-session/undo-tree compression

      ## Module dependencies
      # :checkers spell
      (aspellWithDicts (ds: with ds; [
        de
        en
        en-computers
        en-science
      ]))
      # :checkers grammar
      languagetool
      wordnet # Offline dictionary
      # :tools editorconfig
      editorconfig-core-c # per-project style config
      # :tools lookup
      sqlite
      gcc
      # :lang cc
      ccls
      # :lang javascript
      nodePackages.javascript-typescript-langserver
      # nodePackages.indium
      nodePackages.eslint_d
      # etc
      zstd # for undo-tree compression
      pandoc # Convert stuf
      wmctrl # Window information
      user.jsonrepair
      # Edb
      # (lib.mkIf (config.modules.editors.emacs.withEdbi)
      #   perlPackages.DBI
      #   perlPackages.RPCEPCService
      #   perlPackages.DBDPg
      #   perlPackages.DBDmysql)

      (texlive.combine {
        # LaTeX with org mode!
        inherit (texlive)
          scheme-medium wrapfig capt-of collection-langother ucs
          collection-fontsextra collection-fontsrecommended;
      })

      emacs-lsp-booster

      perlPackages.FileMimeInfo # Mime type
    ];

    services.emacs = {
      enable = cfg.enableServer;
      package = cfg.package;
    };

    env = {
      PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];
      # lsp: use plists instead of hashtables for performance improvement
      # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
      LSP_USE_PLISTS = "true";
    };

    modules.shell.zsh.rcFiles = [ "${configDir}/emacs/aliases.zsh" ];

    fonts.packages = [ pkgs.emacs-all-the-icons-fonts ];

    home = {
      defaultApplications = {
        "x-scheme-handler/org-protocol" = "org-protocol.desktop";
      };
      desktopEntries."org-protocol" = {
        name = "Org-Protocol";
        exec = "emacsclient -- %u";
        icon = "emacs-icon";
        type = "Application";
        mimeType = [ "x-scheme-handler/org-protocol" ];
      };
      desktopEntries."my-emacsclient" =
        let
          cmd = (pkgs.writeScriptBin "my-emacsclient-cmd" ''
            #!${pkgs.stdenv.shell}
            emacsclient -n --alternate-editor="emacs" $@ &
          '');
        in
        {
          name = "Emacsclient";
          exec = "${cmd}/bin/my-emacsclient-cmd -- %u";
          icon = "emacs-icon";
          type = "Application";
        };
    };

    home.configFile.".aspell.conf".text = ''
      dict-dir $HOME/.nix-profile/lib/aspell
      master en_US
      extra-dicts en-computers.rws
      add-extra-dicts en_US-science.rws
    '';

    # Clone Doom Emacs and config if they don't exist
    modules.shell.git-clone.repos = [
      { url = "https://github.com/doomemacs/doomemacs"; dir = "~/.config/emacs"; depth = 1; }
      { url = "git@github.com:floscr/emacs.d.git"; dir = "~/.config/doom"; }
    ];
  };
}
