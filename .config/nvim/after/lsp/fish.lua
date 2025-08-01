---@type lsp_config_t
return {
  filetypes = { 'fish' },
  cmd = { 'efm-langserver' },
  requires = { 'fish' },
  name = 'fish',
  settings = {
    languages = {
      fish = {
        {
          lintSource = 'fish',
          lintCommand = 'fish --no-execute "${INPUT}"',
          lintFormats = { '%.%#(line %l): %m' },
          lintAfterOpen = true,
          lintIgnoreExitCode = true,
        },
      },
    },
  },
}
