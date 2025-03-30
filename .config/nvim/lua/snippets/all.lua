local M = {}

local un = require('utils.snippets.nodes')
local us = require('utils.snippets.snips')
local conds = require('utils.snippets.conds')
local ls = require('luasnip')
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local d = ls.dynamic_node
local c = ls.choice_node

M.snippets = {
  us.s(
    {
      trig = 'date',
      desc = 'Current date and time',
      condition = conds.in_tsnode({ 'comment', 'string', 'curly_group' }),
      show_condition = conds.in_tsnode({ 'comment', 'string', 'curly_group' }),
    },
    c(1, {
      i(nil, os.date()),
      i(nil, os.date('%m.%d.%Y')),
    })
  ),
  us.sn(
    {
      trig = 'vimopt',
      desc = 'Vim options for the current file',
    },
    un.fmtad('<commentstring>vim: <options> :', {
      commentstring = d(2, function()
        local bufnr = vim.api.nvim_get_current_buf()
        local str = vim.fn.trim(
          vim.api.nvim_get_option_value('commentstring', { buf = bufnr }),
          '%s',
          2
        )

        return sn(nil, t(str))
      end),
      options = c(1, {
        i(1),
        t('sw=4 ts=4 sts=4 et tw=80 nospell'),
      }),
    })
  ),
}

return M
