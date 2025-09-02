return {
  -- Clojure
  {
    'Olical/conjure',
    ft = { 'clojure' },
    dependencies = {
      {
        'Saghen/blink.compat',
      },
      {
        'PaterJason/cmp-conjure',
      },
      {
        'saghen/blink.cmp',
        opts = {
          sources = {
            per_filetype = {
              clojure = {
                'conjure',
              },
            },
            providers = {
              conjure = {
                name = 'conjure',
                module = 'blink.compat.source',
              },
            },
          },
        },
      },
    },
  },

  -- Haskell
  {
    'mrcjkb/haskell-tools.nvim',
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
