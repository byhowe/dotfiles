local plugins =
  { { 'wbthomason/packer.nvim' }

    -- general
  , { 'nvim-lua/popup.nvim' }
  , { 'nvim-lua/plenary.nvim' }

    -- editor
  , { 'nvim-treesitter/nvim-treesitter'
    , config = function() require('byron.treesitter').setup() end
    }
  , { 'neovim/nvim-lspconfig'
    , after = { 'nvim-cmp', 'rust-tools.nvim' }
    , config = function() require('byron.lsp').setup() end
    }
  , { 'hrsh7th/cmp-buffer' }
  , { 'hrsh7th/cmp-nvim-lua' }
  , { 'hrsh7th/cmp-nvim-lsp' }
  , { 'hrsh7th/cmp-nvim-lsp-signature-help' }
  , { 'hrsh7th/cmp-path' }
  , { 'hrsh7th/cmp-cmdline' }

  , { 'L3MON4D3/LuaSnip' }
  , { 'saadparwaiz1/cmp_luasnip' }
  , { 'rafamadriz/friendly-snippets' }

  , { 'hrsh7th/nvim-cmp'
    , after = { 'cmp-buffer'
              , 'cmp-nvim-lua'
              , 'cmp-nvim-lsp'
              , 'cmp-nvim-lsp-signature-help'
              , 'cmp-path'
              , 'cmp-cmdline'
              , 'LuaSnip'
              , 'cmp_luasnip'
              , 'friendly-snippets'
              }
    , config = function() require('byron.cmp').setup() end
    }

  -- util
  , { 'nvim-telescope/telescope.nvim', tag = '0.1.0'
    , requires = { { 'nvim-lua/plenary.nvim' } }
    , after = { 'telescope-media-files.nvim' }
    , config = function() require('byron.telescope').setup() end
    }
  , { 'nvim-telescope/telescope-media-files.nvim'
    }
  , { 'akinsho/toggleterm.nvim'
    , config = function()
        require('toggleterm').setup
          { size = 20
          , open_mapping = [[<C-a>]]
          , hide_numbers = true  -- hide the number column in toggleterm buffers
          , direction = 'float'
          }
      end
    }

  -- lang
  , { 'simrat39/rust-tools.nvim' }
  }

-- ensure packer is installed
local ensure_packer = function()
  local install_path = vim.fn.stdpath('data')
    .. '/site/pack/packer/start/packer.nvim'
  if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    print('Downloading packer.nvim...')
    vim.fn.system
      { 'git'
      , 'clone'
      , '--depth'
      , '1'
      , 'https://github.com/wbthomason/packer.nvim'
      , install_path
      }
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end
local packer_bootstrap = ensure_packer()

-- compile plugins when this file is changed
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerInstall
  augroup end
]]

-- packer config
local packer_config =
  { display =
    { open_fn = function()
        return require('packer.util').float({ border = 'rounded' })
      end
    }
  }

return require('packer').startup
  { function(use)
      for _, plugin in ipairs(plugins) do
        use(plugin)
      end

      for _, plugin in ipairs(require('byron.colorscheme').plugins) do
        use(plugin)
      end

      if packer_bootstrap then
        print('Installing plugins...')
        require('packer').sync()
      end
    end
  , config = packer_config
  }
