return {
  -- Haskell
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
