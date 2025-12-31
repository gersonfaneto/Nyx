if not status is-interactive
    exit
end

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

abbr --position anywhere --add cut '| xclip -selection clipboard'
abbr --position command --add yank 'xclip -selection clipboard -out'

abbr --position anywhere --add lines '| wc --lines'
abbr --position anywhere --add filter '| grep --ignore-case --regexp'

abbr --position anywhere --add pager '| less'

abbr --position anywhere --add or '||'
abbr --position anywhere --add and '&&'

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
