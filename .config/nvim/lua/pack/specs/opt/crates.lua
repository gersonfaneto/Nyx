---@type pack.spec
return {
  src = 'https://github.com/saecki/crates.nvim',
  version = 'stable',
  data = {
    postload = function()
      require('crates').setup({
        lsp = {
          enabled = true,
          on_attach = function(_, _) end,
          actions = true,
          completion = true,
          hover = true,
        },
      })
    end,
  },
}
