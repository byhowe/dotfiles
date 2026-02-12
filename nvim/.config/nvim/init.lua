-- Neovim configuration file.

-- [[ Global Settings ]]

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
vim.g.have_nerd_font = true -- set to true if a nerd font is installed

-- [[ General Settings ]]

-- user interface appearence
vim.o.number = true -- show absolute line numbers
vim.o.relativenumber = true --show relative line numbers
vim.o.cursorline = true -- highlight current line
vim.o.colorcolumn = '80' -- highlight the 80th column
vim.o.signcolumn = 'yes' -- show the sign column for git
vim.o.termguicolors = true -- enable true color support
vim.o.mouse = 'a' -- enable mouse mode
vim.o.showmode = false -- hide mode indicator

-- editing experience
vim.o.list = true -- show invisible characters
vim.opt.listchars = { -- characters for invisible whitespace
  tab = '→ ',
  trail = '•',
  nbsp = '␣'
}
vim.o.scrolloff = 16 -- minimum lines above/below cursor when scrolling

-- window management
vim.o.splitright = true -- vertical split opens to the right
vim.o.splitbelow = true -- horizontal split opens below
vim.o.confirm = true -- prompt to save changes when closing unsaved buffers

-- search behaviour
vim.o.ignorecase = true -- ignore the case in search patterns
vim.o.smartcase = true -- override the ignorecase if search includes uppercase
vim.o.inccommand = 'split' -- live preview the effects of a command in a preview window

-- file and session persistence
vim.o.undofile = true -- save undo history to a file
vim.o.swapfile = false -- disable swap files
vim.o.backup = false -- disable backup files

-- performance and responsiveness
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Sync clipboard between OS and Neovim.
-- NOTE: Setting the clipboard may take some time. To reduce the startup time
-- slighly, we schedule it to be run soon by the main event-loop.
vim.schedule(function() vim.o.clipboard = 'unnamedplus' end)

-- [[ Indent Settings ]]

-- Set global defaults
vim.o.smartindent = true -- indent when starting a new line
vim.o.breakindent = true -- wrap lines with indentation preserved

-- Create augroup for indentation settings
local indent_group = vim.api.nvim_create_augroup("FileTypeIndent", { clear = true })

local global_indent_settings = {
  tabstop = 8,
  shiftwidth = 4,
  softtabstop = 4,
  expandtab = true,
}

-- Filetype-specific overrides
local indent_settings = {
  lua = { shiftwidth = 2, softtabstop = 2 },
  haskell = { shiftwidth = 2, softtabstop = 2 },
  make = { expandtab = false },
}

-- Apply global indent settings
for opt, val in pairs(global_indent_settings) do
  vim.o[opt] = val
end

-- Apply file type specific indent settings
for ft, settings in pairs(indent_settings) do
  vim.api.nvim_create_autocmd("FileType", {
    group = indent_group,
    pattern = ft,
    callback = function()
      local opts = vim.tbl_extend('force', global_indent_settings, settings or {})
      for opt, val in pairs(opts) do
        vim.opt_local[opt] = val
      end
    end,
  })
end

-- [[ Basic Autocommands ]]
--  See `:help lua-guide-autocommands`

-- Highlight when yanking (copying) text
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('my-highlight-yank', { clear = true }),
  callback = function() vim.hl.on_yank() end,
})

-- [[ Basic Keymaps ]]
-- See `:help vim.keymap.set()`

-- When doing window splits, neovim transfers the focus to the new window. This
-- function keeps track of the previous window and sets it as the current
-- window after doing the window split.
function split_keep_focus(cmd)
  local win = vim.api.nvim_get_current_win()
  vim.cmd(cmd)
  vim.api.nvim_set_current_win(win)
end

-- Keybinds to make split navigation easier.
-- Clear highlights on search when pressing <Esc> in normal mode
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')
vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

-- TIP: Disable arrow keys in normal mode
vim.keymap.set('n', '<left>', '<cmd>echo "Use h to move!!"<CR>')
vim.keymap.set('n', '<down>', '<cmd>echo "Use j to move!!"<CR>')
vim.keymap.set('n', '<up>', '<cmd>echo "Use k to move!!"<CR>')
vim.keymap.set('n', '<right>', '<cmd>echo "Use l to move!!"<CR>')

--  Use CTRL+<hjkl> to switch between windows
vim.keymap.set('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
vim.keymap.set('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
vim.keymap.set('n', '<C-j>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
vim.keymap.set('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })

-- Window splits
vim.keymap.set('n', '<leader>wv', function() split_keep_focus('vsplit') end, { desc = 'Split window vertically' })
vim.keymap.set('n', '<leader>ws', function() split_keep_focus('split') end, { desc = 'Split window horizontally' })

-- Window closing
vim.keymap.set('n', '<leader>wd', '<C-w>c', { desc = 'Close current window' })
vim.keymap.set('n', '<leader>wq', '<C-w>q', { desc = 'Quit current window' }) -- quits neovim if it is the last window
vim.keymap.set('n', '<leader>wo', '<C-w>o', { desc = 'Close all other windows' })

-- Shift highlighted regions up and down
vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv", { silent = true, desc = 'Shift highlighted block up' })
vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv", { silent = true, desc = 'Shift highlighted block down' })

-- File operations
vim.keymap.set('n', '<leader>fs', '<cmd>w<CR>', { desc = 'Save file' })
vim.keymap.set('n', '<leader>fe', '<cmd>Ex<CR>', { desc = 'Open file explorer' })

-- Keep the cursor in the middle
vim.keymap.set('n', 'n', 'nzzzv')
vim.keymap.set('n', 'N', 'Nzzzv')

-- Visual line navigation
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

-- [[ Plugin Manager ]]

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- [[ Plugins ]]

require('lazy').setup({
  {
    "nvim-treesitter/nvim-treesitter",
    config = function()
      local filetypes = { "c", "lua", "markdown", "rust", "python" }
      require("nvim-treesitter").install(filetypes)
      vim.api.nvim_create_autocmd("FileType", {
        pattern = filetypes,
        callback = function() vim.treesitter.start() end,
      })
    end
  }
})
