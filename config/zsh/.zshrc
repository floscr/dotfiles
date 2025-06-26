# Only attempt to load zgen if we're in an interactive shell
if [[ -o interactive ]]; then
  # Clone zgen if it doesn't exist
  [ -d "$ZGEN_DIR" ] || git clone https://github.com/tarjoilija/zgen "$ZGEN_DIR"
  
  # Only source zgen if the file exists
  if [[ -f "$ZGEN_SOURCE" ]]; then
    source "$ZGEN_SOURCE"
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
  fi
fi

source $ZDOTDIR/config.zsh

if [[ $TERM != dumb ]]; then
  autoload -Uz compinit && compinit -u -d $ZSH_CACHE/zcompdump

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

  # Only run autopair-init if the function exists
  if typeset -f autopair-init > /dev/null; then
    autopair-init
  fi

  function use_default_directory() {
    local default_dir_file="$HOME/.config/.default-directory"
    if [[ -f "$default_dir_file" ]]; then
      local target_dir=$(cat "$default_dir_file")
      if [[ -d "$target_dir" ]]; then
        cd "$target_dir"
      fi
    fi
  }

  use_default_directory

  # For local-only configuration
  [ -f ~/.zshrc ] && source ~/.zshrc
fi
