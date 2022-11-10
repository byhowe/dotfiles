local M = {}

M.setup = function()
  require('nvim-treesitter.configs').setup
    { ensure_installed = { 'c', 'cpp', 'glsl'
                          , 'rust', 'haskell'
                          , 'lua', 'python' 
                          }
    , sync_install = false
    , auto_install = false
    , highlight =
      { enable = true
      }
    }
end

return M
