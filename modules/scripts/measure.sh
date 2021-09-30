#!/usr/bin/env bash

TMPFILE=$(mktemp -p /tmp --suffix ".png")

maim -us "$TMPFILE"

WIDTH=$(identify -ping -format '%w' "$TMPFILE")
HEIGHT=$(identify -ping -format '%h' "$TMPFILE")

HALF_WIDTH=$(echo "$WIDTH / 2" | bc)
HALF_HEIGHT=$(echo "$HEIGHT / 2" | bc)

notify-send "Measured Dimensions" "@1x:   $WIDTH x $HEIGHT\n@0.5x: $HALF_WIDTH x $HALF_HEIGHT"
