#!/usr/bin/env zsh

g() { [[ $# = 0 ]] && git status --short . || git $*; }

alias git='noglob git'
alias ga='git add'
alias gap='git add --patch'
alias gb='git branch -av'
alias gop='git open'
alias gbl='git blame'
alias gc='git commit'
alias gcm='git commit -m'
alias gca='git commit --amend'
alias gcf='git commit --fixup'
alias gcl='git clone'
alias gco='git checkout'
alias gcoo='git checkout --'
alias gf='git fetch'
alias gi='git init'
alias gl='git log --graph --pretty="format:%C(yellow)%h%Creset %C(red)%G?%Creset%C(green)%d%Creset %s %Cblue(%cr) %C(bold blue)<%aN>%Creset"'
alias gll='git log --pretty="format:%C(yellow)%h%Creset %C(red)%G?%Creset%C(green)%d%Creset %s %Cblue(%cr) %C(bold blue)<%aN>%Creset"'
alias gL='gl --stat'
alias gp='git push'
alias gpl='git pull --rebase --autostash'
alias gs='git status --short .'
alias gss='git status'
alias gst='git stash'
alias gr='git reset HEAD'
alias grv='git rev-parse'

function clonecd {
  url=$1;
  # If there is no user input name the repo folder after the repo name
  if [[ -z "$2" ]]; then
    reponame=$(echo "$url" | awk -F/ '{print $NF}' | sed -e 's/.git$//');
  else
    reponame="$2"
  fi
  git clone --recursive "$url" "$reponame";
  cd "$reponame";
}

alias gccd='clonecd'

# Go to the top level dir of the repository
function groot() {
  cd "$(git rev-parse --show-toplevel)"
}
