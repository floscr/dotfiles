{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.emacs;
in {
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    user.packages = with pkgs; [
      ## Emacs itself
      binutils       # native-comp needs 'as', provided by this
      emacsPgtkGcc   # 28 + pgtk + native-comp

      ## Doom dependencies
      git
      (ripgrep.override {withPCRE2 = true;})
      gnutls              # for TLS connectivity

      ## Optional dependencies
      fd                  # faster projectile indexing
      imagemagickBig      # for image-dired
      (mkIf (config.programs.gnupg.agent.enable)
        pinentry_emacs)   # in-emacs gnupg prompts
      zstd                # for undo-fu-session/undo-tree compression

      ## Module dependencies
      # :checkers spell
      (aspellWithDicts (ds: with ds; [
        de
        en en-computers en-science
      ]))
      # :checkers grammar
      languagetool
      wordnet # Offline dictionary
      # :tools editorconfig
      editorconfig-core-c # per-project style config
      # :tools lookup
      sqlite gcc
      # :lang cc
      ccls
      # :lang javascript
      nodePackages.javascript-typescript-langserver
      nodePackages.indium
      nodePackages.eslint_d
      # etc
      zstd   # for undo-tree compression
      pandoc # Convert stuf
      wmctrl # Window information
      # Edb
      # (lib.mkIf (config.modules.editors.emacs.withEdbi)
      #   perlPackages.DBI
      #   perlPackages.RPCEPCService
      #   perlPackages.DBDPg
      #   perlPackages.DBDmysql)
    ];

    modules.bindings.items = [
      {
        description = "Emacs";
        categories = "Editor";
        command = "emacs";
      }
    ];

    env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    modules.shell.zsh.rcFiles = [ "${configDir}/emacs/aliases.zsh" ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    # home.mimeApps = {
    #   enable = true;
    #   defaultApplications = {
    #     "application/x-directory" = "emacs-dired.desktop";
    #     "inode/directory" = "emacs-dired.desktop";
    #     "text/english" = "emacs.desktop";
    #     "text/plain" = "emacs.desktop";
    #     "text/x-c" = "emacs.desktop";
    #     "text/x-c++" = "emacs.desktop";
    #     "text/x-c++hdr" = "emacs.desktop";
    #     "text/x-c++src" = "emacs.desktop";
    #     "text/x-chdr" = "emacs.desktop";
    #     "text/x-csrc" = "emacs.desktop";
    #     "text/x-java" = "emacs.desktop";
    #     "text/x-makefile" = "emacs.desktop";
    #     "text/x-moc" = "emacs.desktop";
    #     "text/x-pascal" = "emacs.desktop";
    #     "text/x-tcl" = "emacs.desktop";
    #     "text/x-tex" = "emacs.desktop";
    #   };
    # };

    home.configFile.".aspell.conf".text = ''
      dict-dir $HOME/.nix-profile/lib/aspell
      master en_US
      extra-dicts en-computers.rws
      add-extra-dicts en_US-science.rws
    '';
  };
}
