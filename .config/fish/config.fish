# vim:ft=fish:ts=4:sw=4:sts=4:et:

if not status is-interactive
    exit
end

if test -f $__fish_config_dir/local.fish
    source $__fish_config_dir/local.fish
end
