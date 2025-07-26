return {
  cmd = { 'nil', '--stdio' },
  filetypes = { 'nix' },
  root_markers = { 'flake.nix', '.git' },
  settings = {
    ['nil'] = {
      formatting = {
        command = { 'alejandra' },
      },
    },
  },
}
