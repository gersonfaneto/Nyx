if not status is-login
    exit
end

# Set rg config path
set -gx RIPGREP_CONFIG_PATH $HOME/.config/rg/ripgreprc
