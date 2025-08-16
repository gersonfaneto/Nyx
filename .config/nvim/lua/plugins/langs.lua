return {
  {
    -- Fix python indent
    -- Without this plugin:
    -- a = [|] -> press <Enter> ->
    -- a = [
    --         |
    --         ]
    -- With this plugin:
    -- a =  [
    --     |
    -- ]
    'Vimjas/vim-python-pep8-indent',
    ft = 'python',
  },
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
}
