if not status is-interactive
    exit
end

# Setup zoxide if z.fish is not available
if type -q zoxide
    zoxide init fish | source
    alias cd z
end
