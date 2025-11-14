if not status is-interactive
    exit
end

if test -f $__fish_config_dir/local.fish
    source $__fish_config_dir/local.fish
end

# TODO: This need to go!
if type -q zoxide
    zoxide init fish | source
end

# vim:ts=4:sw=4:et:
