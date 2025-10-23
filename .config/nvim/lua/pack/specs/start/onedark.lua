---@type pack.spec
return {
  src = 'https://github.com/navarasu/onedark.nvim',
  data = {
    postload = function()
      local hl = require('utils.hl')

      hl.persist(function()
        if vim.g.colors_name and vim.g.colors_name ~= 'onedark' then
          return
        end

        -- HACK: For some reason this is a thing...
        if vim.fn.argc() > 0 then
          return
        end

        local onedark = require('onedark')

        onedark.setup({
          style = vim.go.background, -- make `set bg=light/dark` work
          diagnostics = {
            darker = false,
          },
        })

        onedark.load()

        hl.set(0, 'WinBar', { link = 'StatusLine' })
        hl.set(0, 'WinBarNC', { link = 'StatusLineNC' })
        hl.set(0, 'FloatTitle', { link = 'NormalFloat', fg = 'Title' })
      end)
    end,
  },
}
