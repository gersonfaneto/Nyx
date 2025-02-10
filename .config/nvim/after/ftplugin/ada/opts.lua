-- Make sure to disable `ada-vim` INSANE default keymaps.
local bufnr = vim.api.nvim_get_current_buf()

pcall(vim.keymap.del, "i", "<leader>aj", { buffer = bufnr })
pcall(vim.keymap.del, "i", "<leader>al", { buffer = bufnr })
