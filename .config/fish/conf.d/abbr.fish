if not status is-interactive
    exit
end

# Spell fix abbreviation
set -g __spellfix_pairs \
    'ture true' \
    'Ture True' \
    'flase false' \
    'fasle false' \
    'Flase False' \
    'Fasle False' \
    'lcaol local' \
    'lcoal local' \
    'locla local' \
    'sahre share' \
    'saher share' \
    'balme blame'
function __spellfix_abbr_fn --description 'Fix spelling errors'
    # $argv: matched string
    set -l str "$argv"
    for pair_str in $__spellfix_pairs
        set -l pair (string split ' ' $pair_str)
        set pair[1] (string escape --style regex $pair[1])
        set pair[2] (string escape --style regex $pair[2])
        set pair[1] (string join '' '\b' $pair[1] '\b')
        set str (string replace --all --regex -- $pair $str)
    end
    echo -- $str
end

abbr --add spellfix --position anywhere \
    --regex '\S*' --function __spellfix_abbr_fn

# Command abbreviations
function __command_abbr --description 'Add an command abbreviation' -a trigger
    abbr --add $trigger --position command $argv[2..-1]
end

# Just for fun...
__command_abbr please 'sudo'
__command_abbr btw 'fastfetch'

__command_abbr cp 'cp -i'
__command_abbr mv 'mv -i'
__command_abbr mkdir 'mkdir -p'

__command_abbr rm 'trash-put'
__command_abbr rml 'trash-list'
__command_abbr rme 'trash-empty'
__command_abbr rmp 'yes | trash-empty --verbose'
__command_abbr rmr 'trash-restore'

__command_abbr df 'df -h'
__command_abbr du 'du -hs'

__command_abbr cl 'clear'

__command_abbr g 'git'

__command_abbr lt 'lazygit'
__command_abbr lq 'lazysql'
__command_abbr lk 'lazydocker'

__command_abbr tm 'tmux'
__command_abbr tma 'tmux attach -t'
__command_abbr tmn 'tmux new-session -s (basename $PWD)'
__command_abbr tml 'tmux list-session'
__command_abbr tmrs 'tmux rename-session'
__command_abbr tmrw 'tmux rename-window'
__command_abbr tmka 'tmux kill-server'
__command_abbr tmks 'tmux kill-session'

__command_abbr dk 'docker'
__command_abbr dkp 'docker ps'
__command_abbr dkpa 'docker ps --all'

__command_abbr ls 'ls -lhX --group-directories-first'
__command_abbr la 'ls -lhAX --group-directories-first'

__command_abbr dea 'direnv allow'
__command_abbr ded 'direnv deny'
__command_abbr dee 'direnv edit'
__command_abbr der 'direnv reload'

__command_abbr tree 'tree -N -L 4 -C --'

__command_abbr oc 'opencode'

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
