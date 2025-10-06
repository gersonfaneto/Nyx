-- Language server for YAML files
-- https://github.com/redhat-developer/yaml-language-server

---@type lsp.config
return {
  filetypes = { 'yaml', 'yaml.gh' },
  cmd = {
    'yaml-language-server',
    '--stdio',
  },
  settings = {
    -- Don't send telemetry to redhat
    -- https://github.com/redhat-developer/vscode-redhat-telemetry#how-to-disable-telemetry-reporting
    redhat = {
      telemetry = {
        enabled = false,
      },
    },
    yaml = {
      format = {
        enable = true,
      },
      validate = true,
      schemas = {
        ['https://json.schemastore.org/clang-format.json'] = '.clang-format',
        ['https://json.schemastore.org/clangd.json'] = '.clangd',
      },
    },
  },
}
