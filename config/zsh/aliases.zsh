alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias -- -='cd -'

alias q=exit
alias clr=clear
alias sudo='sudo '
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias mkdir='mkdir -p'
alias wget='wget -c'

SMORGAS_DIR="~/Code/Smorgasbord"
alias smo="bb --deps-root $SMORGAS_DIR $SMORGAS_DIR/smorgus.clj"
alias \,="smo ,"

alias rg='noglob rg'
alias ag='noglob ag -p $HOME/.config/ag/agignore'

alias mk=make
alias rcp='rsync -vaP --delete'
alias rmirror='rsync -rtvu --delete'
alias gurl='curl --compressed'

alias y='xclip -selection clipboard -in'
alias p='xclip -selection clipboard -out'

alias scu='systemctl --user'
alias sc=systemctl
alias ssc='sudo systemctl'
alias jc='journalctl'

alias dun='notify-send "done"'

if command -v eza >/dev/null; then
  alias exa="eza --group-directories-first";
  alias l="eza -1";
  alias ll="eza -lg";
  alias la="LC_COLLATE=C eza -la";
fi

autoload -U zmv

take() {
  mkdir "$1" && cd "$1";
}; compdef take=mkdir

zman() {
  PAGER="less -g -I -s '+/^       "$1"'" man zshall;
}

emptytrash() {
  rm -rf ~/.Trash
  mkdir ~/.Trash
  notify-send "Trash emptied"
}

alias o='open'
function open () {
  xdg-open "$@">/dev/null 2>&1
}

r() {
  local time=$1; shift
  sched "$time" "notify-send --urgency=critical 'Reminder' '$@'; ding";
}; compdef r=sched
