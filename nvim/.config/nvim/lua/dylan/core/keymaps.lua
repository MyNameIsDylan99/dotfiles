-- Set leader key to space
vim.g.mapleader = " "

local opts = { noremap = true, silent = true } -- common options for keymaps

---------------------
-- General Keymaps -------------------

-- Use jk to exit insert mode
vim.api.nvim_set_keymap("i", "jk", "<ESC>", opts)
vim.api.nvim_set_keymap("i", "jj", "<ESC>", opts)

-- Clear search highlights
vim.api.nvim_set_keymap("n", "<leader>nh", ":nohl<CR>", opts)

-- Increment/decrement numbers
vim.api.nvim_set_keymap("n", "<leader>+", "<C-a>", opts)
vim.api.nvim_set_keymap("n", "<leader>-", "<C-x>", opts)

-- Window management
vim.api.nvim_set_keymap("n", "<leader>sv", "<C-w>v", opts)
vim.api.nvim_set_keymap("n", "<leader>sh", "<C-w>s", opts)
vim.api.nvim_set_keymap("n", "<leader>se", "<C-w>=", opts)
vim.api.nvim_set_keymap("n", "<leader>sx", "<cmd>close<CR>", opts)

vim.api.nvim_set_keymap("n", "<leader>to", "<cmd>tabnew<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>tx", "<cmd>tabclose<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>tn", "<cmd>tabn<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>tp", "<cmd>tabp<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>tf", "<cmd>tabnew %<CR>", opts)
