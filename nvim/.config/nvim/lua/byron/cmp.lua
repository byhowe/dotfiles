local M = {}

local kind_icons =
  { Text          = '  '
  , Method        = '  '
  , Function      = '  '
  , Constructor   = '  '
  , Field         = '  '
  , Variable      = '  '
  , Class         = ' ﴯ '
  , Interface     = '  '
  , Module        = '  '
  , Property      = ' ﰠ '
  , Unit          = '  '
  , Value         = '  '
  , Enum          = '  '
  , Keyword       = '  '
  , Snippet       = '  '
  , Color         = '  '
  , File          = '  '
  , Reference     = '  '
  , Folder        = '  '
  , EnumMember    = '  '
  , Constant      = '  '
  , Struct        = '  '
  , Event         = '  '
  , Operator      = '  '
  , TypeParameter = '  '
  }

M.setup = function()
  local cmp = require('cmp')
  local luasnip = require('luasnip')

  require('luasnip.loaders.from_vscode').lazy_load()

  cmp.setup
    { snippet =
        { expand = function(args)
            luasnip.lsp_expand(args.body)
          end
        }
    , window =
        { completion =
            { winhighlight = 'Normal:Pmenu,FloatBorder:Pmenu,Search:None'
            , col_offset = -3
            , side_padding = 0
            }
        , documentation = cmp.config.window.bordered()
        }
    , view = { entries = { name = 'custom', selection_order = 'near_cursor' } }
    , formatting =
        { fields = { 'kind', 'abbr', 'menu' }
        , format = function(entry, vim_item)
            vim_item.kind = string.format('%s', kind_icons[vim_item.kind])
            vim_item.menu =
             ({ buffer = '[Buffer]'
              , nvim_lsp = '[LSP]'
              , luasnip = '[Snip]'
              , nvim_lua = '[Lua]'
              , latex_symbols = '[LaTeX]'
              })[entry.source.name]
            return vim_item
          end
        }
    , mapping =
        { ['<C-k>'] = cmp.mapping.select_prev_item()
        , ['<C-j>'] = cmp.mapping.select_next_item()

        , ['<C-b>'] = cmp.mapping.scroll_docs(-2)
        , ['<C-f>'] = cmp.mapping.scroll_docs(2)
        , ['<C-Space>'] = cmp.mapping.complete()
        , ['<C-e>'] = cmp.mapping.abort()
        , ['<CR>'] = cmp.mapping.confirm({ select = false })
        }
    , sources = cmp.config.sources(
        { { name = 'nvim_lsp' }
        , { name = 'nvim_lsp_signature_help' }
        , { name = 'nvim_lua' }
        , { name = 'luasnip' }
        , { name = 'path' }
        }
      , { { name = 'buffer', keyword_length = 3 }
        }
      )
    , experimental =
        { ghost_text = true
        }

      -- disable completion for comments
    , enabled = function()
        local context = require('cmp.config.context')
        -- keep command mode completion enabled when cursor is in a comment
        if vim.api.nvim_get_mode().mode == 'c' then
          return true
        elseif vim.api.nvim_buf_get_option(0, 'buftype') == 'prompt' then
          return false
        else
          return not context.in_treesitter_capture('comment') and
                 not context.in_syntax_group('Comment')
        end
      end
    }
end

return M
