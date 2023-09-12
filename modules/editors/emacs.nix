{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.emacs;
in
{
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    enableServer = mkBoolOpt false;
    enableMail = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    user.packages = with pkgs; [
      ## Emacs itself
      binutils # native-comp needs 'as', provided by this
      ((emacsPackagesFor emacsNativeComp).emacsWithPackages
        (epkgs: [ epkgs.vterm ]))

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
        pinentry_emacs) # in-emacs gnupg prompts
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
      # Edb
      # (lib.mkIf (config.modules.editors.emacs.withEdbi)
      #   perlPackages.DBI
      #   perlPackages.RPCEPCService
      #   perlPackages.DBDPg
      #   perlPackages.DBDmysql)

      perlPackages.FileMimeInfo # Mime type
    ];

    services.emacs = {
      enable = cfg.enableServer;
      package = pkgs.emacsPkgs.emacsNativeComp;
    };

    env = {
      PATH = [ "$HOME/.emacs.d/bin" ];
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
  };
}
