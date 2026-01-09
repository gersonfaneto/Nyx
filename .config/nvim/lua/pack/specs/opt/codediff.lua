---@type pack.spec
return {
  src = 'https://github.com/esmuellert/codediff.nvim',
  data = {
    deps = {
      src = 'https://github.com/MunifTanjim/nui.nvim',
    },
    cmds = {
      'CodeDiff',
    },
    -- opts = {
    --   explorer = {
    --     view_mode = 'tree',
    --   },
    -- },
  },
}
