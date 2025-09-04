if not status is-interactive
    exit
end

if type -q zoxide
    alias cat 'bat --theme ansi --style plain'
end
