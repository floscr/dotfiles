#!/usr/bin/env zsh

function e() {
  emacsclient -n "${1:-.}"
}
ediff() { e --eval "(ediff-files \"$1\" \"$2\")"; }
