-- :help options

local options =
  { backup = false
  , clipboard = 'unnamedplus'
  , cmdheight = 1
  , completeopt = { 'menu', 'menuone', 'noselect' }
  , conceallevel = 0
  , fileencoding = 'utf-8'
  , hlsearch = true
  , ignorecase = true
  , mouse = 'a'
  , pumheight = 10
  , showmode = false
  , showtabline = 2
  , smartcase = true
  , smartindent = true
  , splitbelow = true
  , splitright = true
  , swapfile = false
  , termguicolors = true
  , timeoutlen = 1000
  , undofile = true
  , updatetime = 300
  , writebackup = false
  , expandtab = true
  , shiftwidth = 2
  , tabstop = 4
  , cursorline = true
  , number = false
  , relativenumber = false
  , numberwidth = 4
  , signcolumn = 'no'
  , wrap = false
  , scrolloff = 8
  , sidescrolloff = 8
  , guifont = 'monospace:h17'
  , background = 'dark'
  }

for opt, val in pairs(options) do
  vim.opt[opt] = val
end

vim.opt.shortmess:append 'c'
vim.opt.iskeyword:append '-'
vim.opt.whichwrap:append '<,>,[,],h,l'
-- vim.cmd [[set formatoptions-=cro]]
