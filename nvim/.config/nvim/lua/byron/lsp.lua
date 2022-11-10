local M = {}

M.setup = function()
  local capabilities = require('cmp_nvim_lsp').default_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true

  local on_attach = function(client, bufnr)
    vim.api.nvim_buf_set_option(bufnr, "formatexpr", "v:lua.vim.lsp.formatexpr()")
	vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
	vim.api.nvim_buf_set_option(bufnr, "tagfunc", "v:lua.vim.lsp.tagfunc")
  end

  require('rust-tools').setup {
    server = {
      capabilities = capabilities,
      on_attach = on_attach
    }
  }

  local lspconfig = require('lspconfig')
  lspconfig.pyright.setup{
      on_attach = on_attach,
  }
  lspconfig.tsserver.setup{
      on_attach = on_attach,
  }
  lspconfig.rust_analyzer.setup{
      on_attach = on_attach,
      capabilities = capabilities,
      -- Server-specific settings...
      settings = {
        ["rust-analyzer"] = {}
      },
  }
  lspconfig.sumneko_lua.setup
    { settings =
        { Lua =
            { runtime = { version = 'LuaJIT' }
            , diagnostics = { globals = { 'vim' } }
            , workspace = { library = vim.api.nvim_get_runtime_file('', true) }
            , telemetry = { enable = false }
            }
        }
    }
end

return M
