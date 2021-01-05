#!/usr/bin/env bash

function display_name() {
    xrandr -q | grep primary | cut -d' ' -f1
}

function is_laptop_display() {
    [ `display_name` == "eDP1" ]
}

function screen_width {
    xdpyinfo | awk -F'[ x]+' '/dimensions:/{print $3}'
}

function screen_height {
    xdpyinfo | awk -F '[ x]+' '/dimensions:/{print $4}'
}

function set_window_size () {
    local window_dimensions="$(bspc query -T -n | jq '.client.tiledRectangle | .width, .height')"
    local window_width=$(echo "$window_dimensions" | head -n 1)
    local window_height=$(echo "$window_dimensions" | tail -n 1)
    local display_dimensions="$(bspc query -T -m $(bspc query -M -d focused) | jq '.rectangle | .width, .height')"
    local display_width=$(echo "$display_dimensions" | head -n 1)
    local display_height=$(echo "$display_dimensions" | tail -n 1)

    local diffwidth=$((display_width / 100 * 80))
    local diffheight=$((display_height / 100 * 95))
    local x=$(((display_width - diffwidth) / 2))
    local y=$(((display_height - diffheight) / 2))

    echo "Window Width: $window_width, Display Width: $display_width"
    echo "Window Height: $window_height, Display Height: $display_height"
    echo "Diff Height: $diffwidth, Diff Height: $diffheight"
    echo "x: $x, y: $y"
    bspc node focused -t floating
    xdotool getactivewindow windowmove $x $y
    xdotool getactivewindow windowsize $diffwidth $diffheight
}
