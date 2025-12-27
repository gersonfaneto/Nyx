if not status is-interactive
    exit
end

# Command abbreviations
function __command_abbr --description 'Add an command abbreviation' -a trigger
    abbr --add $trigger --position command $argv[2..-1]
end

# Just for fun...
__command_abbr please 'sudo'
__command_abbr btw    'clear -x && fastfetch'

__command_abbr cp    'cp -i'
__command_abbr mv    'mv -i'
__command_abbr mkdir 'mkdir -p'

__command_abbr df   'df -h'
__command_abbr du   'du -hs'
__command_abbr free 'free -h'

__command_abbr g 'git'

__command_abbr cl 'clear'

__command_abbr rm  'trash-put'
__command_abbr rml 'trash-list'
__command_abbr rme 'trash-empty'
__command_abbr rmp 'yes | trash-empty --verbose'
__command_abbr rmr 'trash-restore'

__command_abbr tm   'tmux'
__command_abbr tma  'tmux attach -t'
__command_abbr tmn  'tmux new-session -s (basename $PWD)'
__command_abbr tml  'tmux list-session'
__command_abbr tmrs 'tmux rename-session'
__command_abbr tmrw 'tmux rename-window'
__command_abbr tmka 'tmux kill-server'
__command_abbr tmks 'tmux kill-session'

__command_abbr ls 'ls -lhXN --group-directories-first'
__command_abbr la 'ls -lhAXN --group-directories-first'

__command_abbr dea 'direnv allow'
__command_abbr ded 'direnv deny'
__command_abbr dee 'direnv edit'
__command_abbr der 'direnv reload'

__command_abbr tree 'tree -N -L 4 -C --gitignore --dirsfirst'

__command_abbr tpon  'sudo echo 0 | sudo tee /sys/class/input/event13/device/inhibited'
__command_abbr tpoff 'sudo echo 1 | sudo tee /sys/class/input/event13/device/inhibited'

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

abbr --add v --position command --function __command_abbr_v_fn
