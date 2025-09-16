if not status is-interactive
    exit
end

if type -q atuin
    atuin init fish | source
    atuin gen-completion --shell fish | source
end
