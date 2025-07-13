return {
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

  -- Clojure
  {
    'Olical/conjure',
    ft = { 'clojure' },
  },

  -- Java
  -- {
  --   'nvim-java/nvim-java',
  --   ft = { 'java' },
  -- },

  -- Rust
  {
    'mrcjkb/rustaceanvim',
    version = '^6',
    ft = { 'rust' },
  },
}
