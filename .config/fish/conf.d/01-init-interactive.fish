# Initialization for interactive shell
if not status is-interactive
    return
end

set -gx FZF_CTRL_R_OPTS --no-preview
set -gx FZF_PREVIEW_DISABLE_UB true # Disable ueberzug preview

if type -q fzf_key_bindings
    fzf_key_bindings
end

# Ensure color theme files are correctly linked
if status is-login
    type -q setbg; and setbg
    type -q setcolors; and setcolors
end

# Disable venv prompt provided by `activate.fish` -- we handle it ourselves in
# `functions/fish_right_prompt.fish`
# Also, shouldn't export this variable with `set -gx` as we don't want to to
# affect other shells, e.g. `bash` that is nested in `fish`
set -g VIRTUAL_ENV_DISABLE_PROMPT true

hash --add "$HOME/.bin"
hash --add "$HOME/.local/bin"

type -q direnv; and direnv hook fish | source

type -q zoxide; and zoxide init fish | source

type -q nvim; and set -gx EDITOR nvim && set -gx MANPAGER 'nvim +Man!' \
    && fc-list - family | cut -d , -f 1 | sort | uniq | grep -q 'Nerd Font' \
    && set -gx NVIM_NF true

set -gx SNIPPETS "$HOME/.snippets"

hash --add "$HOME/.intelFPGA_lite/24.1std/quartus/bin"

set -gx LM_LICENSE_FILE "$HOME/.licenses/LR-223410_License.dat"
