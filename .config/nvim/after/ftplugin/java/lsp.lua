if vim.fn.executable('jdtls') == 0 then
  return
end

local jdtls = require('jdtls')

local config = {
  cmd = { 'jdtls' },
  root_dir = vim.fs.dirname(
    vim.fs.find({ 'gradlew', '.git', 'mvnw' }, { upward = true })[1]
  ),
}

jdtls.start_or_attach(config)

local opts = { silent = true, buffer = 0 }

-- stylua: ignore start
vim.keymap.set({ 'n' }, '<localleader>oi', "<Cmd>lua require('jdtls').organize_imports()<CR>", opts)
vim.keymap.set({ 'n' }, '<localleader>ev', "<Cmd>lua require('jdtls').extract_variable()<CR>", opts)
vim.keymap.set({ 'x' }, '<localleader>ev', "<Esc><Cmd>lua require('jdtls').extract_variable(true)<CR>", opts)
vim.keymap.set({ 'n' }, '<localleader>ec', "<Esc><Cmd>lua require('jdtls').extract_constant()<CR>", opts)
vim.keymap.set({ 'x' }, '<localleader>ec', "<Esc><Cmd>lua require('jdtls').extract_constant(true)<CR>", opts)
vim.keymap.set( { 'x' }, '<localleader>em', "<Esc><Cmd>lua require('jdtls').extract_method(true)<CR>", opts)
-- stylua: ignore off
