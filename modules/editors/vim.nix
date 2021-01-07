{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.vim;
in {
  options.modules.editors.vim = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      editorconfig-core-c
      neovim
    ];

    programs.neovim = {
      enable = true;
      vimAlias = true;
      viAlias = true;

      configure = {
        packages.metalogical = with pkgs.vimPlugins; {
          start = [
            vim-airline
            fzfWrapper fzf-vim
            deoplete-nvim LanguageClient-neovim
            vim-nix # vim-addon-nix?
            vim-go # TODO move to go-dev.nix
          ];
          opt = [ ];
        };
        customRC = ''
          set undodir=$XDG_DATA_HOME/vim/undo
          set directory=$XDG_DATA_HOME/vim/swap
          set backupdir=$XDG_DATA_HOME/vim/backup
          set viewdir=$XDG_DATA_HOME/vim/view
          set viminfo+='1000,n$XDG_DATA_HOME/vim/viminfo
          set runtimepath=$XDG_CONFIG_HOME/vim,$VIMRUNTIME,$XDG_CONFIG_HOME/vim/after
        '';
      };
    };
  };
}
