---@type pack.spec
return {
  src = 'https://github.com/cvigilv/esqueleto.nvim',
  data = {
    postload = function()
      ---@diagnostic disable-next-line: missing-fields
      require('esqueleto').setup({
        directories = {
          (vim.env.XDG_CONFIG_HOME or vim.fs.normalize('~/.config'))
            .. '/nvim/templates',
        },
        -- autouse = false,
      })
    end,
  },
}
