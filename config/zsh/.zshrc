[ -d "$ZGEN_DIR" ] || git clone https://github.com/tarjoilija/zgen "$ZGEN_DIR"
source $ZGEN_SOURCE
if ! zgen saved; then
  echo "Initializing zgen"
  zgen load hlissner/zsh-autopair autopair.zsh
  zgen load zsh-users/zsh-history-substring-search
  zgen load zdharma/history-search-multi-word
  zgen load zsh-users/zsh-completions src
  zgen load junegunn/fzf shell
  zgen load kutsan/zsh-system-clipboard
  [ -z "$SSH_CONNECTION" ] && zgen load zdharma/fast-syntax-highlighting
  zgen save
fi

source $ZDOTDIR/config.zsh

# Sets a x window property on every directory change in the shell
# So I can stay in the same directory when opening a new terminal
function my_set_xwindow_path_hook() {
  if [[ $TERM == "xterm-256color" && ! -z $WINDOWID ]]; then
    xprop -id $WINDOWID -f MY_XWINDOW_PATH 8s -set MY_XWINDOW_PATH "$(pwd)"
  fi
}
chpwd_functions=(${chpwd_functions[@]} "my_set_xwindow_path_hook")
precmd_functions=(${precmd_functions[@]} "my_set_xwindow_path_hook")

if [[ $TERM != dumb ]]; then
  source $ZDOTDIR/keybinds.zsh
  source $ZDOTDIR/completion.zsh
  source $ZDOTDIR/aliases.zsh

  function _cache {
    command -v "$1" >/dev/null || return 1
    local cache_dir="$XDG_CACHE_HOME/${SHELL##*/}"
    local cache="$cache_dir/$1"
    if [[ ! -f $cache || ! -s $cache ]]; then
      echo "Caching $1"
      mkdir -p $cache_dir
      "$@" >$cache
    fi
    source $cache || rm -f $cache
  }

  # fd > find
  if command -v fd >/dev/null; then
    export FZF_DEFAULT_OPTS="--reverse --ansi"
    export FZF_DEFAULT_COMMAND="fd ."
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_ALT_C_COMMAND="fd -t d . $HOME"
  fi

  # Load nix generated files
  source $ZDOTDIR/extra.zshrc

  autoload -Uz compinit && compinit -u -d $ZSH_CACHE/zcompdump
  autopair-init

  # For local-only configuration
  [ -f ~/.zshrc ] && source ~/.zshrc
fi
