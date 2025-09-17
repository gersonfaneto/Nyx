---@type vim.pack.Spec
return {
  src = 'https://github.com/mrcjkb/haskell-tools.nvim',
  version = vim.version.range('^6'),
  data = {
    init = function()
      vim.g.haskell_tools = {
        hls = {
          settings = {
            haskell = {
              formattingProvider = 'stylish-haskell',
            },
          },
        },
      }
    end,
  },
}
