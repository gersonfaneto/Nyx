return {
  -- Haskell
  {
    'mrcjkb/haskell-tools.nvim',
    version = '^6', -- Recommended
    ft = { 'haskell', 'cabal' },
  },
  {
    'neovimhaskell/haskell-vim',
    ft = { 'haskell', 'cabal' },
  },
  {
    'ndmitchell/ghcid',
    ft = { 'haskell', 'cabal' },
    config = function(plugin)
      vim.opt.rtp:append(plugin.dir .. '/plugins/nvim/')
    end,
  },
}
