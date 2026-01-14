--
-- PLUGINS
--
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  {"williamboman/mason.nvim"},
  {"williamboman/mason-lspconfig.nvim"},
  {"VonHeikemen/lsp-zero.nvim", branch = "v3.x"},
  {"nvimtools/none-ls.nvim", dependencies = { "nvim-lua/plenary.nvim", }},
  {"hrsh7th/cmp-nvim-lsp"},
  {"hrsh7th/nvim-cmp"},
  {"L3MON4D3/LuaSnip"},
  {"folke/tokyonight.nvim", lazy = false, priority = 1000, opts = {},},
  {"tpope/vim-fugitive"},
  {"lewis6991/gitsigns.nvim"},
  -- {"github/copilot.vim"},
  {"mfussenegger/nvim-dap"},
  {"rcarriga/nvim-dap-ui"},
  {"nvim-lualine/lualine.nvim"},
  {"nvim-treesitter/nvim-treesitter"},
  {"nvim-telescope/telescope.nvim"},
  {"normen/vim-pio"},
  {"coddingtonbear/neomake-platformio"},
  {"rest-nvim/rest.nvim"},
  {"nvim-tree/nvim-tree.lua", lazy = false, dependencies = { "nvim-web-devicons", }},
  {"numToStr/Comment.nvim"},
  {"pocco81/auto-save.nvim"},
  {"vimlab/split-term.vim"},
  {"nvim-zh/colorful-winsep.nvim"},
  {"HiPhish/rainbow-delimiters.nvim"},
})

--
-- PLUGIN CONFIGS
--

-- Mason & LSP
require("mason").setup()
require("mason-lspconfig").setup({
  ensure_installed = {
    "ts_ls",
    "pyright",
    "clangd",
  },
})

local lsp_zero = require("lsp-zero")
lsp_zero.on_attach(function(client, bufnr)
  lsp_zero.default_keymaps({buffer = bufnr})
end)

vim.lsp.config.ts_ls = {
  cmd = { 'typescript-language-server', '--stdio' },
  root_markers = { 'tsconfig.json', 'package.json', '.git' },
  filetypes = { 'typescript', 'typescriptreact', 'javascript', 'javascriptreact' },
}

vim.lsp.config.pyright = {
  cmd = { 'pyright-langserver', '--stdio' },
  root_markers = { 'pyproject.toml', 'setup.py', '.git' },
  filetypes = { 'python' },
}

vim.lsp.config.clangd = {
  cmd = { 'clangd' },
  root_markers = { 'compile_commands.json', '.git' },
  filetypes = { 'c', 'cpp', 'objc', 'objcpp' },
}

vim.lsp.enable({ 'ts_ls', 'pyright', 'clangd' })

vim.api.nvim_create_autocmd('LspAttach', {
  desc = 'LSP actions',
  callback = function(event)
    local bufmap = function(mode, lhs, rhs)
      local opts = {buffer = event.buf}
      vim.keymap.set(mode, lhs, rhs, opts)
    end

    bufmap('i', '<C-Space>', '<C-x><C-o>')
    bufmap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>')
    bufmap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>')
    bufmap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>')
    bufmap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>')
    bufmap('n', 'go', '<cmd>lua vim.lsp.buf.type_definition()<cr>')
    bufmap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>')
    bufmap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<cr>')
    bufmap('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>')
    bufmap('n', '<F3>', '<cmd>lua vim.lsp.buf.format()<cr>')
    bufmap('n', '<leader>qf', '<cmd>lua vim.lsp.buf.code_action()<cr>')
    bufmap('n', '<leader>d', '<cmd>lua vim.diagnostic.open_float()<cr>')
  end
})

vim.diagnostic.config({
  float = {
    border = 'rounded',
  },
})

vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(
  vim.lsp.handlers.hover,
  {border = 'rounded'}
)

vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(
  vim.lsp.handlers.signature_help,
  {border = 'rounded'}
)

-- none-ls (null-ls)
local null_ls = require("null-ls")

null_ls.setup({
  sources = {
    null_ls.builtins.formatting.prettier.with({
      filetypes = { "javascript", "typescript", "html", "css", "json", "yaml", "markdown" },
    }),
  },
})

vim.keymap.set("n", "<leader>fmt", function()
  vim.lsp.buf.format({ async = true })
end, { desc = "Format current buffer" })

-- Lualine
require("lualine").setup({
  options = {
    icons_enabled = true,
    theme = "tokyonight",
    section_separators = "",
    component_separators = "",
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch" },
    lualine_c = { "filename" },
    lualine_x = { "encoding", "fileformat", "filetype" },
    lualine_y = { "progress" },
    lualine_z = { "location" },
  },
})

-- Treesitter
require("nvim-treesitter.configs").setup {
  ensure_installed = {
    "c", "lua", "vim", "vimdoc", "markdown",
    "cpp", "python", "fish", "json", "yaml",
    "html", "css", "javascript", "typescript", "tsx",
    "swift", "kotlin", "http",
  },
  highlight = { enable = true, },
  indent = { enable = true, },
  autotag = { enable = true, },
  context_commentstring = { enable = true, },
  textobjects = {
    select = {
      enable = true,
      keymaps = {
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
    swap = {
      enable = true,
      swap_next = { ["<leader>a"] = "@parameter.inner", },
      swap_previous = { ["<leader>A"] = "@parameter.inner", },
    },
    move = {
      enable = true,
      set_jumps = true,
      goto_next_start = { ["]m"] = "@function.outer", ["]]"] = "@class.outer", },
      goto_next_end = { ["]M"] = "@function.outer", ["]["] = "@class.outer", },
      goto_previous_start = { ["[m"] = "@function.outer", ["[["] = "@class.outer", },
      goto_previous_end = { ["[M"] = "@function.outer", ["[]"] = "@class.outer", },
    },
  },
}

-- nvim-tree
require("nvim-tree").setup {
  sort = {
    sorter = "name",
    folders_first = true,
  },
  view = {
    width = {
      min = 30,
      max = -1,
      padding = 2,
    },
    float = {
      enable = false,
      quit_on_focus_loss = false,
    },
  },
  renderer = {
    group_empty = true,
    highlight_git = all,
    highlight_opened_files = all,
    indent_markers = {
      enable = true,
    },
  },
  update_focused_file = {
    enable = true,
    update_cwd = false,
  },
}

-- Telescope
vim.cmd "nnoremap <leader>f :Telescope find_files<CR>"
vim.cmd "nnoremap <leader>s :Telescope live_grep<CR>"
vim.cmd "nnoremap gb :Telescope buffers<CR>"
vim.cmd "nnoremap <leader>b :Telescope buffers<CR>"
vim.cmd "nnoremap <leader>r :Telescope lsp_references<CR>"
vim.cmd "nnoremap <leader>ic :Telescope lsp_incoming_calls<CR>"
vim.cmd "nnoremap <leader>oc :Telescope lsp_outgoing_calls<CR>"
vim.cmd "nnoremap <leader>def :Telescope lsp_definitions<CR>"
vim.cmd "nnoremap <leader>imp :Telescope lsp_implementations<CR>"
vim.cmd "nnoremap <leader>sym :Telescope lsp_document_symbols<CR>"
vim.cmd "nnoremap <leader>wsym :Telescope lsp_dynamic_workspace_symbols<CR>"

-- Gitsigns
require("gitsigns").setup()
vim.keymap.set("n", "<leader>bl", ":Gitsigns toggle_current_line_blame<CR>", { desc = "Toggle blame" })

--
-- OPTIONS
--

vim.opt.compatible = false
vim.cmd "filetype off"

vim.opt.mouse = "a"
vim.opt.cp = false
vim.opt.backup = false
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.incsearch = true
vim.opt.hlsearch = true
vim.opt.ruler = true
vim.opt.number = true
vim.opt.cursorline = true
vim.opt.breakindent = true
vim.opt.wrap = true
vim.opt.linebreak = true
vim.opt.backspace = "eol,indent,start"
vim.opt.expandtab = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.smartindent = true
vim.opt.diffopt = "filler,iwhite"
vim.opt.showmatch = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.wildignore = "*.o,*.obj,*~,*.pyc,__pycache__,*.class,**/node_modules/**,**/dist/**,**/build/**,**/target/**,.git/**,.svn/**,CVS/**,vendor/**,*.egg-info/**,*.egg/**,*.log,**/tmp/**"

vim.cmd "syntax on"
vim.cmd "filetype on"
vim.cmd "filetype plugin on"
vim.cmd "filetype plugin indent on"

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 1
vim.g.netrw_list_hide = ".DS_Store"

vim.opt.termguicolors = true
vim.cmd "colorscheme tokyonight"

--
-- KEYMAPS
--

vim.cmd "nnoremap t :NvimTreeFocus<CR>"
vim.cmd "nnoremap gm :lua vim.diagnostic.open_float()<CR>"
vim.cmd "nnoremap gca :lua vim.lsp.buf.code_action()<CR>"

vim.cmd "vnoremap <D-c> \"+y"
vim.cmd "xnoremap <D-c> \"+y"
vim.cmd "vnoremap <D-x> \"+yd"
vim.cmd "xnoremap <D-x> \"+yd"
vim.cmd "inoremap <D-v> <Esc>\"+Pa"

--
-- AUTOCMDS
--

-- Trim trailing whitespace on save
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*",
  callback = function()
    local save_cursor = vim.fn.getpos(".")
    vim.cmd([[%s/\s\+$//e]])
    vim.fn.setpos(".", save_cursor)
  end,
})

-- Filetype-specific settings (replaces ftplugin files)
vim.api.nvim_create_autocmd("FileType", {
  pattern = "json",
  callback = function()
    vim.bo.formatprg = "jq --indent 2 --sort-keys"
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "http",
  callback = function()
    vim.api.nvim_buf_set_keymap(0, "n", "<leader>r", ":Rest run<CR>", { noremap = true })
  end,
})

--
-- GUI (Neovide)
--

if vim.g.neovide then
  vim.g.neovide_cursor_animation_length = 0
  vim.g.neovide_cursor_short_animation_length = 0
  vim.g.neovide_cursor_animate_in_insert_mode = false
  vim.g.neovide_cursor_animate_command_line = false
  vim.g.neovide_cursor_trail_size = 0
  vim.g.neovide_scroll_animation_length = 0

  vim.g.neovide_floating_blur_amount_x = 2.5
  vim.g.neovide_floating_blur_amount_y = 2.5

  vim.g.neovide_floating_shadow = true
  vim.g.neovide_floating_z_height = 10
  vim.g.neovide_light_angle_degrees = 45
  vim.g.neovide_light_radius = 5
end
