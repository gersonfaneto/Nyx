if not status is-interactive
    exit
end

if type -q sesh
    sesh completion fish | source

    alias ss 'sesh connect "$(\
        sesh list --icons | fzf --no-border \
        --list-border \
        --no-sort --prompt \'Search :: \' \
        --input-border \
        --header-border \
        --bind \'tab:down,btab:up\' \
        --bind \'ctrl-a:change-prompt(Create :: )+reload(sesh list --icons)\' \
        --bind \'ctrl-t:change-prompt(Session :: )+reload(sesh list -t --icons)\' \
        --bind \'ctrl-x:change-prompt(Zoxide :: )+reload(sesh list -z --icons)\' \
        --bind \'ctrl-d:change-prompt()+execute(tmux kill-session -t {2..})+change-prompt(Search :: )+reload(sesh list --icons)\' \
        --preview-window \'right:70%,border\' \
        --preview \'sesh preview {}\' \
        )"'
end
