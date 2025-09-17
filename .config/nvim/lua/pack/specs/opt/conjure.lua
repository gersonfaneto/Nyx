---@type vim.pack.Spec
return {
  src = 'https://github.com/Olical/conjure',
  data = {
    preload = function()
      vim.g['conjure#filetypes'] = { 'clojure' }
    end,
  },
}
