return {
  src = 'https://github.com/ndmitchell/ghcid',
  data = {
    ft = { 'haskell', 'cabal' },
    config = function(plugin)
      vim.opt.rtp:append(plugin.dir .. '/plugins/nvim/')
    end,
  },
}
