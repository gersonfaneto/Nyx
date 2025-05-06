#!/usr/bin/env bash

# Use fzf to select tmux windows
# Source: https://stackoverflow.com/a/38160854

# set -e
# set -x

tmux list-windows -aF '#S:#I (#W)' \
    | sed -e 's/^\([^:]*\):/\x1b[34m\1\x1b[0m:/' \
        -e 's/:\([0-9]\+\) /\:\x1b[32m\1\x1b[0m /' \
        -e 's/(\(.*\))$/(\x1b[33m\1\x1b[0m)/' \
    | fzf --no-preview --ansi \
        --color hl:reverse:-1,hl+:reverse:-1 \
        --prompt 'Tmux windows> ' \
        --border=rounded \
        --tmux "center,$(
            w=$(tmux display -p '#{window_width}')
            echo $((w < 64 ? w : 64))
        ),$(
            h=$(tmux display -p '#{window_height}')
            echo $((h < 16 ? h : 16))
        )" \
        --bind "ctrl-x:execute(echo {+} | \
        sed 's/([^()]*)//g' | \
        sed 's/\s\+/\n/g' | \
        tac | \
        xargs -I % tmux kill-window -t %)+reload(tmux list-windows -aF '#S:#I (#W)' \
        | sed -e 's/^\([^:]*\):/\x1b[34m\1\x1b[0m:/' \
              -e 's/:\([0-9]\+\) /\:\x1b[32m\1\x1b[0m /' \
              -e 's/(\(.*\))$/(\x1b[33m\1\x1b[0m)/')" \
    | tail -n1 \
    | sed 's/\s*(.*)$//' \
    | xargs -r tmux switch -t
