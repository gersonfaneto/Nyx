if not status is-interactive
    exit
end

abbr --position command --add please sudo
abbr --position command --add btw 'clear -x && fastfetch'

abbr --position command --add cp 'cp -i'
abbr --position command --add mv 'mv -i'
abbr --position command --add mkdir 'mkdir -p'

abbr --position command --add .- 'cd -'
abbr --position command --add .2 'cd ../..'
abbr --position command --add .3 'cd ../../..'
abbr --position command --add .4 'cd ../../../..'

abbr --position command --add df 'df -h'
abbr --position command --add du 'du -hs'
abbr --position command --add free 'free -h'

abbr --position command --add g git

abbr --position command --add cl clear

abbr --position command --add rm trash-put
abbr --position command --add rml trash-list
abbr --position command --add rme trash-empty
abbr --position command --add rmp 'yes | trash-empty --verbose'
abbr --position command --add rmr trash-restore

abbr --position command --add tm tmux
abbr --position command --add tma 'tmux attach -t'
abbr --position command --add tmn 'tmux new-session -s (basename $PWD)'
abbr --position command --add tml 'tmux list-session'
abbr --position command --add tmrs 'tmux rename-session'
abbr --position command --add tmrw 'tmux rename-window'
abbr --position command --add tmka 'tmux kill-server'
abbr --position command --add tmks 'tmux kill-session'

abbr --position command --add ls 'ls -lhXN --group-directories-first'
abbr --position command --add la 'ls -lhAXN --group-directories-first'

abbr --position command --add dea 'direnv allow'
abbr --position command --add ded 'direnv deny'
abbr --position command --add dee 'direnv edit'
abbr --position command --add der 'direnv reload'

abbr --position command --add tree 'tree -N -L 4 -C --gitignore --dirsfirst'

abbr --position command --add eg 'emacsclient --create-frame --alternate-editor \'emacs\''
abbr --position command --add et 'emacsclient --tty --alternate-editor \'emacs --no-window-system\''

abbr --position command --add tpon 'sudo echo 0 | sudo tee /sys/class/input/event13/device/inhibited'
abbr --position command --add tpoff 'sudo echo 1 | sudo tee /sys/class/input/event13/device/inhibited'

abbr --position anywhere --add cut '| xclip -selection clipboard'
abbr --position command --add yank 'xclip -selection clipboard -out'

function __command_abbr_v_fn --description 'Abbreviation function for `v`'
    if command -q nvim
        echo nvim
        return
    end
    if command -q vim
        echo vim
        return
    end
    echo vi
end

abbr --position command --add v --function __command_abbr_v_fn
