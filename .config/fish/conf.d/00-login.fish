# Initialize fish shell once on login, including settings global variables etc.
if not status is-login
    exit
end

set -Ux NVIM_NF 1

set -Ux GOPATH "$HOME/.go"
set -Ux GOBIN "$HOME/.go/bin"

fish_add_path --move \
    $HOME/.bin \
    $HOME/.local/bin \
    $HOME/.local/share/bin

fish_add_path --move \
    $HOME/.go/bin \
    $HOME/.cargo/bin

if test -f $HOME/.envvars
    source $HOME/.envvars
end

if test -f $__fish_config_dir/fish_envvars
    source $__fish_config_dir/fish_envvars
end

for editor in nvim vim vi
    if type -q $editor
        set -gx EDITOR $editor
        if test $editor = nvim
            set -gx MANPAGER 'nvim +Man!'
        end
        break
    end
end

# Ensure color theme files are correctly linked
if not test -f $__fish_config_dir/themes/Current.theme
    type -q background; and background dark &
end

# Automatically login to proot-distro on termux
if type -q proot-distro
    and test -n "$PROOT_DISTRO"
    and test -n "$PROOT_USER"
    and test -n "$TERMUX_VERSION"
    exec proot-distro login $PROOT_DISTRO --user $PROOT_USER --termux-home
end
