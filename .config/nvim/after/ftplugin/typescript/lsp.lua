-- Shared lsp config for front end dev:
-- - javascript
-- - typescript
-- - javascriptreact
-- - typescriptreact
-- - json
-- - jsonc
-- - html
-- - css
-- - tailwindcss

vim.lsp.enable('typescript-language-server')
vim.lsp.enable('vtsls')
vim.lsp.enable('eslint')
vim.lsp.enable('prettier')
vim.lsp.enable('biome') -- prefer biome over prettier as formatter
vim.lsp.enable('tailwindcss')
