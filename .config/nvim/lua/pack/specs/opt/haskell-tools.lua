---@type vim.pack.Spec
return {
  src = 'https://github.com/mrcjkb/haskell-tools.nvim',
  data = {
    version = '^6', -- Recommended
    ft = { 'haskell', 'cabal' },
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
