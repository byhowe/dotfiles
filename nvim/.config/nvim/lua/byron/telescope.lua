local M = {}

M.setup = function()
local actions = require('telescope.actions')

local cfg = {}
cfg.defaults = {}
cfg.defaults.mappings = {}
cfg.defaults.preview = {}
cfg.pickers = {}

cfg.pickers.find_files = { theme = 'dropdown' }
cfg.pickers.live_grep = { theme = 'dropdown' }

cfg.defaults.mappings.i =
  { ['<ESC>'] = actions.close -- quit the prompt in insert mode
  }

cfg.defaults.preview.mime_hook = function(filepath, bufnr, opts)
  local is_image = function(fp)
    local image_extensions = { 'png', 'jpg', 'jpeg' }
    local split_path = vim.split(fp:lower(), '.', { plain = true })
    local extensions = split_path[#split_path]
    return vim.tbl_contains(image_extensions, extensions)
  end

  if is_image(filepath) then
    local term = vim.api.nvim_open_term(bufnr, {})
    local send_output = function(_, data, _)
      for _, d in ipairs(data) do
        vim.api.nvim_chan_send(term, d .. '\r\n')
      end
    end
    vim.fn.jobstart(
      { 'catimg'
      , '-w100'
      , filepath
      }
    , { on_stdout = send_output, stdout_buffered = true
      }
    )
  else
    require('telescope.previewers.utils').set_preview_message(bufnr, opts.winid, 'Binary cannot be previewed')
  end
end

require('telescope').setup(cfg)
end

return M
