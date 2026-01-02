#!/usr/bin/env sh

has() {
  command -v "$1" > /dev/null 2>&1
}

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

export PATH="$HOME/.bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.go/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"

if [ -r "$HOME/.envvars" ]; then
  . "$HOME/.envvars"
fi

for editor in nvim vim vi; do
  if has "$editor"; then
    export EDITOR="$editor"
    [ "$editor" = nvim ] && export MANPAGER='nvim +Man!'
    break
  fi
done

export RIPGREP_CONFIG_PATH="$HOME/.config/rg/ripgreprc"

export BAT_THEME=ansi

export FZF_DEFAULT_OPTS="--reverse \
  --preview='previewer {}' \
  --preview-window=right,55%,border-none,nocycle \
  --info=inline-right \
  --no-separator \
  --no-scrollbar \
  --border=none \
  --margin=1,0,0 \
  --height=~75% \
  --min-height=16 \
  --scroll-off=999 \
  --multi \
  --ansi \
  --color=fg:-1,bg:-1,hl:bold:cyan \
  --color=fg+:-1,bg+:-1,hl+:bold:cyan \
  --color=border:white,preview-border:white \
  --color=marker:bold:cyan,prompt:bold:red,pointer:bold:red \
  --color=gutter:grey,info:bold:red,spinner:cyan,header:white \
  --bind=ctrl-k:kill-line \
  --bind=alt-a:toggle-all \
  --bind=alt-up:first,alt-down:last \
  --bind=shift-up:preview-up,shift-down:preview-down \
  --bind=alt-v:preview-half-page-up,ctrl-v:preview-half-page-down"

fd=$(has fd && echo fd || echo fdfind)

if has "$fd"; then
  export FZF_DEFAULT_COMMAND="$fd -p -H -L -td -tf -tl -c=always"
  export FZF_ALT_C_COMMAND="$fd -p -H -L -td -c=always"
else
  export FZF_DEFAULT_COMMAND="find -L . -mindepth 1 \\( \
    -path '*%*'                \
    -o -path '*.*Cache*/*'     \
    -o -path '*.*cache*/*'     \
    -o -path '*.*wine/*'       \
    -o -path '*.cargo/*'       \
    -o -path '*.conda/*'       \
    -o -path '*.dot/*'         \
    -o -path '*.env/*'         \
    -o -path '*.fonts/*'       \
    -o -path '*.git/*'         \
    -o -path '*.ipython/*'     \
    -o -path '*.java/*'        \
    -o -path '*.jupyter/*'     \
    -o -path '*.luarocks/*'    \
    -o -path '*.mozilla/*'     \
    -o -path '*.npm/*'         \
    -o -path '*.nvm/*'         \
    -o -path '*.steam*/*'      \
    -o -path '*.thunderbird/*' \
    -o -path '*.tmp/*'         \
    -o -path '*.venv/*'        \
    -o -path '*Cache*/*'       \
    -o -path '*\\\$*'          \
    -o -path '*\\~'            \
    -o -path '*__pycache__/*'  \
    -o -path '*cache*/*'       \
    -o -path '*dosdevices/*'   \
    -o -path '*env/*'          \
    -o -path '*node_modules/*' \
    -o -path '*vendor/*'       \
    -o -path '*venv/*'         \
    -o -fstype 'sysfs'         \
    -o -fstype 'devfs'         \
    -o -fstype 'devtmpfs'      \
    -o -fstype 'proc' \\) -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | cut -b3-"

  export FZF_ALT_C_COMMAND="find -L . -mindepth 1 \\( \
    -path '*%*'                \
    -o -path '*.*Cache*/*'     \
    -o -path '*.*cache*/*'     \
    -o -path '*.*wine/*'       \
    -o -path '*.cargo/*'       \
    -o -path '*.conda/*'       \
    -o -path '*.dot/*'         \
    -o -path '*.fonts/*'       \
    -o -path '*.git/*'         \
    -o -path '*.ipython/*'     \
    -o -path '*.java/*'        \
    -o -path '*.jupyter/*'     \
    -o -path '*.luarocks/*'    \
    -o -path '*.mozilla/*'     \
    -o -path '*.npm/*'         \
    -o -path '*.nvm/*'         \
    -o -path '*.env/*'         \
    -o -path '*.steam*/*'      \
    -o -path '*.thunderbird/*' \
    -o -path '*.tmp/*'         \
    -o -path '*.venv/*'        \
    -o -path '*Cache*/*'       \
    -o -path '*\\\$*'          \
    -o -path '*\\~'            \
    -o -path '*__pycache__/*'  \
    -o -path '*cache*/*'       \
    -o -path '*dosdevices/*'   \
    -o -path '*env/*'          \
    -o -path '*node_modules/*' \
    -o -path '*vendor/*'       \
    -o -path '*venv/*'         \
    -o -fstype 'sysfs'         \
    -o -fstype 'devfs'         \
    -o -fstype 'devtmpfs'      \
    -o -fstype 'proc' \\) -prune \
    -o -type d -print 2> /dev/null | cut -b3-"
fi

export FZF_CTRL_R_OPTS=--no-preview
export FZF_PREVIEW_DISABLE_UB=true

# has background && (background dark &) 2> /dev/null

if [ ! -e "${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}}/greeted" ]; then
  touch "${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}}/greeted"

  if has fastfetch; then
    fetch=fastfetch
  elif has neofetch; then
    fetch=neofetch
  fi

  if [ -n "$fetch" ]; then
    if script -q /dev/null -c exit > /dev/null 2>&1; then
      script -q /dev/null -c "$fetch"
    else
      script -q /dev/null "$fetch"
    fi
  fi
fi

# vim:ts=2:sw=2:et:ft=sh:
