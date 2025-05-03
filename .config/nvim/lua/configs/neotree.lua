require('neo-tree').setup({
  close_if_last_window = true,
  popup_border_style = 'single',
  enable_git_status = true,
  enable_modified_markers = true,
  enable_diagnostics = true,
  sort_case_insensitive = true,
  default_component_configs = {
    indent = {
      with_markers = true,
      with_expanders = true,
    },
    modified = {
      symbol = ' ',
      highlight = 'NeoTreeModified',
    },
    ---@diagnostic disable-next-line: missing-fields
    icon = {
      folder_closed = '',
      folder_open = '',
      folder_empty = '',
      folder_empty_open = '',
    },
    git_status = {
      symbols = {
        -- Change type
        added = '',
        deleted = '',
        modified = '',
        renamed = '',
        -- Status type
        untracked = '',
        ignored = '',
        unstaged = '',
        staged = '',
        conflict = '',
      },
    },
  },
  window = {
    position = 'float',
    width = 35,
  },
  filesystem = {
    hijack_netrw_behavior = 'disabled',
    use_libuv_file_watcher = true,
    filtered_items = {
      hide_dotfiles = false,
      hide_gitignored = false,
      hide_by_name = {
        'node_modules',
      },
      never_show = {
        '.DS_Store',
        'thumbs.db',
      },
    },
  },
  ---@diagnostic disable-next-line: missing-fields
  source_selector = {
    winbar = true,
    sources = {
      { source = 'filesystem', display_name = '   Files ' },
      { source = 'buffers', display_name = '   Bufs ' },
      { source = 'git_status', display_name = '   Git ' },
    },
  },
  event_handlers = {
    {
      event = 'neo_tree_window_after_open',
      handler = function(args)
        if args.position == 'left' or args.position == 'right' then
          vim.cmd('wincmd =')
        end
      end,
    },
    {
      event = 'neo_tree_window_after_close',
      handler = function(args)
        if args.position == 'left' or args.position == 'right' then
          vim.cmd('wincmd =')
        end
      end,
    },
  },
})

vim.keymap.set('n', '<C-p>', function()
  local reveal_file = vim.fn.expand('%:p')
  if reveal_file == '' then
    reveal_file = vim.fn.getcwd()
  else
    local f = io.open(reveal_file, 'r')
    if f then
      f.close(f)
    else
      reveal_file = vim.fn.getcwd()
    end
  end
  require('neo-tree.command').execute({
    action = 'show',
    source = 'filesystem',
    position = 'right',
    reveal_file = reveal_file,
    reveal_force_cwd = true,
  })
end, { desc = 'Open neo-tree at current file or working directory' })
