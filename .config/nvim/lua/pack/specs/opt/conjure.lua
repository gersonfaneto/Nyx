---@type minimal.pack.spec
return {
  src = 'https://github.com/Olical/conjure',
  data = {
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
      require('which-key').add({
        { '<LocalLeader>c', group = 'Conjure' },
      })
    end,
  },
}
