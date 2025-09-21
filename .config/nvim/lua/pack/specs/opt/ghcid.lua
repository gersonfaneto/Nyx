return {
  src = 'https://github.com/ndmitchell/ghcid',
  data = {
    postload = function()
      -- TODO: This is kinda ugly...
      local plugin = vim.pack.get({ 'ghcid' })[1] --[[@as vim.pack.PlugData]]
      vim.opt.rtp:append(plugin.path .. '/plugins/nvim/')
    end,
  },
}
