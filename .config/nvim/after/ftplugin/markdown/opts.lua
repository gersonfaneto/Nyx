vim.bo.shiftwidth = 4
vim.bo.tabstop = 4
vim.bo.expandtab = true

vim.bo.cindent = false
vim.bo.smartindent = false

vim.bo.commentstring = '<!-- %s -->'

vim.wo.conceallevel = 2
vim.wo.concealcursor = 'c'

vim.wo.foldminlines = 1

vim.wo.wrap = true

---Don't join title/first line of list item with previous lines when yanking
---with joined paragraphs
---@param line string
---@return boolean
---@diagnostic disable-next-line: duplicate-set-field
function vim.b.should_join_line(line)
  return line ~= ''
    and not line:match('^%s*[-*#]%s+')
    and not line:match('^%s*%d+%.%s+')
end
