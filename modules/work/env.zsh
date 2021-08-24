#!/usr/bin/env zsh

export MEISTERLABS_DIR="$HOME/Code/Meisterlabs"
export MEISTERLABS_MINDMEISTER_BE_DIR="$MEISTERLABS_DIR/mindmeister"
export MEISTERLABS_MINDMEISTER_FE_DIR="$MEISTERLABS_DIR/mindmeister-web"
export MEISTERLABS_MINDMEISTER_VAGRANT_DIR="$MEISTERLABS_DIR/Vagrant"

function mm_sh {
    docker exec -it mm-rails /bin/bash
}

function mm_load {
    cd $MEISTERLABS_MINDMEISTER_VAGRANT_DIR
    ./mindmeister/rake load
}

function mm_bundle {
    cd $MEISTERLABS_MINDMEISTER_VAGRANT_DIR
    ./mindmeister/bundle
}

function mm_attach {
    cd $MEISTERLABS_MINDMEISTER_VAGRANT_DIR
    ./mindmeister/attach
}

function mm_start {
    cd $MEISTERLABS_MINDMEISTER_VAGRANT_DIR
    if [[ "$( docker container inspect -f '{{.State.Running}}' mm-rails )" == "true" ]]; then
        ./mindmeister/restart
        notify-send "MM Docker: Restarted"
    else
        ./mindmeister/start
        notify-send "MM Docker: Started"
    fi
}

function mm_serve {
    alacritty --hold -d ~/Code/Meisterlabs/Vagrant -e "bash -c \"vagrant up; vagrant ssh -c 'cd mindmeister; rails s; /bin/bash' -- -L 3001:localhost:3001\"" &
    alacritty --hold -d ~/Code/Meisterlabs/mindmeister-web &
}
