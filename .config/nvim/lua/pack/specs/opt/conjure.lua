---@type vim.pack.Spec
return {
  src = 'https://github.com/Olical/conjure',
  data = {
    ft = { 'clojure' },
    deps = {
      {
        src = 'https://github.com/Saghen/blink.compat',
      },
      {
        src = 'https://github.com/PaterJason/cmp-conjure',
      },
      {
        src = 'https://github.com/saghen/blink.cmp',
        data = {
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
  },
}
