local M = {}

local theme = require('byron.config').theme

local catppuccin =
  { 'catppuccin/nvim', as = 'catppuccin'
  , run = ':CatppuccinCompile'
  , config = function()
      vim.g.catppuccin_flavour = 'mocha'
      require('catppuccin').setup
        { term_colors = true
        , compile =
            { enabled = true
            , path = vim.fn.stdpath 'cache' .. '/catppuccin'
            }
        , styles =
            { comments = { 'italic' }
            , conditionals = { 'italic' }
            }
        , integrations =
            { cmp = true
            , treesitter = true
            , telescope = true
            }
        }
      vim.cmd [[colorscheme catppuccin]]
      if require('byron.config').theme.transparent then
        vim.api.nvim_set_hl(0, 'Normal', { guibg = NONE, ctermbg = NONE })
      end
    end
  , disable = theme.colorscheme ~= 'catppuccin'
  }

local tokyonight =
  { 'folke/tokyonight.nvim'
  , config = function()
      require('tokyonight').setup
        { style = 'night' -- 'storm', 'night', 'day'
        , transparent = require('byron.config').theme.transparent
        , terminal_colors = true
        , styles =
            { comments = 'italic'
            , functions = 'italic'
            }
        }
      vim.cmd [[colorscheme tokyonight]]
    end
  , disable = theme.colorscheme ~= 'tokyonight'
  }

local gruvbox =
  { 'ellisonleao/gruvbox.nvim'
  , config = function()
      vim.o.background = 'dark'
      require('gruvbox').setup
        { italic = true
        , contrast = 'hard'
        }
      vim.cmd [[colorscheme gruvbox]]
      if require('byron.config').theme.transparent then
        vim.api.nvim_set_hl(0, 'Normal', { guibg = NONE, ctermbg = NONE })
      end
    end
  , disable = theme.colorscheme ~= 'gruvbox'
  }

M.plugins =
  { catppuccin
  , tokyonight
  , gruvbox
  }

return M
