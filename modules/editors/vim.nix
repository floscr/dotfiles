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
          ];
          opt = [ ];
        };

        customRC = ''
          " -*-vimrc-*-
          " Disable any state from vim
          set nobackup
          set noswapfile
          set hidden
          let g:netrw_dirhistmax = 0

          let mapleader=" "
          nnoremap <leader>ff <cmd>Telescope find_files<cr>
          nnoremap <leader>fg <cmd>Telescope live_grep<cr>

          " Better defaults
          set smartindent

          set scrolloff=8
          set incsearch

          set viewdir=$XDG_DATA_HOME/vim/view
        '';
      };
    };
  };
}
