if not status is-interactive
    exit
end

# Setup zoxide if z.fish is not available
# if type -q zoxide; and not type -q __z
if type -q zoxide
    zoxide init fish | source
end
