---@type vim.lsp.Config
return {
  cmd = { 'verible-verilog-ls' },
  filetypes = { 'systemverilog', 'verilog' },
  root_markers = { '.git' },
}
