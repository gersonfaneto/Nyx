---@type minimal.pack.spec
return {
  src = 'https://github.com/Olical/conjure',
  data = {
    deps = {
      {
        src = 'https://github.com/saghen/blink.cmp',
      },
      {
        src = 'https://github.com/saghen/blink.compat',
        data = {
          deps = {
            {
              src = 'https://github.com/PaterJason/cmp-conjure',
            },
          },
        },
      },
    },
    events = {
      event = 'FileType',
      pattern = 'clojure',
    },
    preload = function()
      vim.g['conjure#filetypes'] = { 'clojure' }
      vim.g['conjure#mapping#prefix'] = ',c'
      vim.g['conjure#mapping#doc_word'] = false
    end,
    postload = function()
      require('blink.cmp').setup({
        sources = {
          default = {
            'conjure',
          },
          provides = {
            conjure = {
              name = 'conjure',
              module = 'blink.compat.source',
              score_offset = -3,
            },
          },
        },
        appearance = { kind_icons = { Conjure = '🪄' } },
      })

      require('which-key').add({
        { '<LocalLeader>c', group = 'Conjure' },
      })
    end,
  },
}
