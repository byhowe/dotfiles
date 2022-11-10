local opts = { noremap = true, silent = true }
local term_opts = { silent = true }

local keys =
  { { "n", "<leader>wh", "<C-w>h" } -- window navigation
  , { "n", "<leader>wj", "<C-w>j" }
  , { "n", "<leader>wk", "<C-w>k" }
  , { "n", "<leader>wl", "<C-w>l" }

  , { "n", "<leader>e", ":Lex 30<CR>" } -- file explorer
  , { "n", "<leader>ff", ":lua require('telescope.builtin').find_files()<CR>" }
  , { "n", "<leader>fg", ":lua require('telescope.builtin').live_grep()<CR>" }
  , { "n", "<leader>fb", ":lua require('telescope.builtin').buffers()<CR>" }
  , { "n", "<leader>fh", ":lua require('telescope.builtin').help_tags()<CR>" }
  }

-- Shorten function name
local key = vim.api.nvim_set_keymap

--Remap space as leader key
key("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

for _, km in ipairs(keys) do
  vim.api.nvim_set_keymap(km[1], km[2], km[3], opts)
end

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Normal --
-- Better window navigation
-- key("n", "<leader>wh", "<C-w>h", opts)
-- key("n", "<leader>wj", "<C-w>j", opts)
-- key("n", "<leader>wk", "<C-w>k", opts)
-- key("n", "<leader>wl", "<C-w>l", opts)
-- 
-- key("n", "<leader>e", ":Lex 30<cr>", opts)

-- Resize with arrows
key("n", "<C-Up>", ":resize +2<CR>", opts)
key("n", "<C-Down>", ":resize -2<CR>", opts)
key("n", "<C-Left>", ":vertical resize -2<CR>", opts)
key("n", "<C-Right>", ":vertical resize +2<CR>", opts)

-- Navigate buffers
key("n", "<S-l>", ":bnext<CR>", opts)
key("n", "<S-h>", ":bprevious<CR>", opts)

-- Insert --

-- Visual --
-- Stay in indent mode
key("v", "<", "<gv", opts)
key("v", ">", ">gv", opts)

-- Move text up and down
key("v", "<A-j>", ":m .+1<CR>==", opts)
key("v", "<A-k>", ":m .-2<CR>==", opts)
key("v", "p", '"_dP', opts)

-- Visual Block --
-- Move text up and down
key("x", "J", ":move '>+1<CR>gv-gv", opts)
key("x", "K", ":move '<-2<CR>gv-gv", opts)
key("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
key("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)

-- Terminal --
-- Better terminal navigation
key("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
key("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
key("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
key("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)
