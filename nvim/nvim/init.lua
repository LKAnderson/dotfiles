--
-- LEADER KEY (must be set before lazy.nvim)
--
vim.g.mapleader = "\\"

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
  {"hrsh7th/cmp-nvim-lsp"},
  {"hrsh7th/nvim-cmp"},
  {"L3MON4D3/LuaSnip"},
  {"folke/tokyonight.nvim", lazy = false, priority = 1000, opts = {}},
  {"tpope/vim-fugitive"},
  {"lewis6991/gitsigns.nvim"},
  {"mfussenegger/nvim-dap"},
  {"rcarriga/nvim-dap-ui", dependencies = { "mfussenegger/nvim-dap", "nvim-neotest/nvim-nio" }},
  {"nvim-lualine/lualine.nvim"},
  {"nvim-treesitter/nvim-treesitter"},
  {"nvim-treesitter/nvim-treesitter-textobjects"},
  {"nvim-telescope/telescope.nvim"},
  {"normen/vim-pio"},
  {"coddingtonbear/neomake-platformio"},
  {"rest-nvim/rest.nvim"},
  {"nvim-tree/nvim-tree.lua", lazy = false, dependencies = { "nvim-web-devicons" }},
  {"nvim-zh/colorful-winsep.nvim"},
  {"HiPhish/rainbow-delimiters.nvim"},
})

--
-- PLUGIN CONFIGS
--

-- Mason & LSP
require("mason").setup()
require("mason-lspconfig").setup({
  ensure_installed = { "ts_ls", "pyright", "clangd" },
})

vim.lsp.config.ts_ls = {
  cmd = { "typescript-language-server", "--stdio" },
  root_markers = { "tsconfig.json", "package.json", ".git" },
  filetypes = { "typescript", "typescriptreact", "javascript", "javascriptreact" },
}

vim.lsp.config.pyright = {
  cmd = { "pyright-langserver", "--stdio" },
  root_markers = { "pyproject.toml", "setup.py", ".git" },
  filetypes = { "python" },
}

vim.lsp.config.clangd = {
  cmd = { "clangd" },
  root_markers = { "compile_commands.json", ".git" },
  filetypes = { "c", "cpp", "objc", "objcpp" },
}

vim.lsp.enable({ "ts_ls", "pyright", "clangd" })

-- nvim-cmp (completion)
local cmp = require("cmp")
cmp.setup({
  sources = {
    { name = "nvim_lsp" },
  },
  snippet = {
    expand = function(args)
      require("luasnip").lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<CR>"] = cmp.mapping.confirm({ select = true }),
    ["<C-e>"] = cmp.mapping.abort(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-p>"] = cmp.mapping.select_prev_item(),
  }),
})

vim.api.nvim_create_autocmd("LspAttach", {
  desc = "LSP actions",
  callback = function(event)
    local opts = { buffer = event.buf }
    vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
    vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
    vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
    vim.keymap.set("n", "go", vim.lsp.buf.type_definition, opts)
    vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
    vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, opts)
    vim.keymap.set("n", "<F2>", vim.lsp.buf.rename, opts)
    vim.keymap.set("n", "<F3>", vim.lsp.buf.format, opts)
    vim.keymap.set("n", "<leader>qf", vim.lsp.buf.code_action, opts)
    vim.keymap.set("n", "<leader>d", vim.diagnostic.open_float, opts)
  end
})

vim.diagnostic.config({
  float = { border = "rounded" },
})

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
  vim.lsp.handlers.hover,
  { border = "rounded" }
)

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(
  vim.lsp.handlers.signature_help,
  { border = "rounded" }
)

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
require("nvim-treesitter.configs").setup({
  ensure_installed = {
    "c", "lua", "vim", "vimdoc", "markdown",
    "cpp", "python", "fish", "json", "yaml",
    "html", "css", "javascript", "typescript", "tsx",
    "swift", "kotlin", "http",
  },
  highlight = { enable = true },
  indent = { enable = true },
  autotag = { enable = true },
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
      swap_next = { ["<leader>a"] = "@parameter.inner" },
      swap_previous = { ["<leader>A"] = "@parameter.inner" },
    },
    move = {
      enable = true,
      set_jumps = true,
      goto_next_start = { ["]m"] = "@function.outer", ["]]"] = "@class.outer" },
      goto_next_end = { ["]M"] = "@function.outer", ["]["] = "@class.outer" },
      goto_previous_start = { ["[m"] = "@function.outer", ["[["] = "@class.outer" },
      goto_previous_end = { ["[M"] = "@function.outer", ["[]"] = "@class.outer" },
    },
  },
})

-- nvim-tree
require("nvim-tree").setup({
  sort = {
    sorter = "name",
    folders_first = true,
  },
  view = {
    width = { min = 30, max = -1, padding = 2 },
    float = { enable = false, quit_on_focus_loss = false },
  },
  renderer = {
    group_empty = true,
    highlight_git = "all",
    highlight_opened_files = "all",
    indent_markers = { enable = true },
  },
  update_focused_file = { enable = true, update_cwd = false },
})

-- Telescope
vim.keymap.set("n", "<leader>f", "<cmd>Telescope find_files<cr>")
vim.keymap.set("n", "<leader>s", "<cmd>Telescope live_grep<cr>")
vim.keymap.set("n", "gb", "<cmd>Telescope buffers<cr>")
vim.keymap.set("n", "<leader>b", "<cmd>Telescope buffers<cr>")
vim.keymap.set("n", "<leader>ic", "<cmd>Telescope lsp_incoming_calls<cr>")
vim.keymap.set("n", "<leader>oc", "<cmd>Telescope lsp_outgoing_calls<cr>")
vim.keymap.set("n", "<leader>def", "<cmd>Telescope lsp_definitions<cr>")
vim.keymap.set("n", "<leader>imp", "<cmd>Telescope lsp_implementations<cr>")
vim.keymap.set("n", "<leader>sym", "<cmd>Telescope lsp_document_symbols<cr>")
vim.keymap.set("n", "<leader>wsym", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>")
vim.keymap.set("n", "<leader>r", "<cmd>Telescope lsp_references<cr>")

-- Gitsigns
require("gitsigns").setup()
vim.keymap.set("n", "<leader>bl", "<cmd>Gitsigns toggle_current_line_blame<cr>", { desc = "Toggle blame" })

-- Colorful winsep
require("colorful-winsep").setup()

-- DAP
require("dapui").setup()

--
-- OPTIONS
--

vim.opt.mouse = "a"
vim.opt.backup = false
vim.opt.updatetime = 2000
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
vim.opt.termguicolors = true

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.cmd.colorscheme("tokyonight")

--
-- KEYMAPS
--

vim.keymap.set("n", "t", "<cmd>NvimTreeFocus<cr>")
vim.keymap.set("n", "gm", vim.diagnostic.open_float)
vim.keymap.set("n", "gca", vim.lsp.buf.code_action)

vim.keymap.set("v", "<D-c>", '"+y')
vim.keymap.set("x", "<D-c>", '"+y')
vim.keymap.set("v", "<D-x>", '"+yd')
vim.keymap.set("x", "<D-x>", '"+yd')
vim.keymap.set("i", "<D-v>", '<Esc>"+Pa')

--
-- AUTOCMDS
--

-- Auto-save after 2s idle, on focus lost, or buffer leave
vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI", "BufLeave", "FocusLost" }, {
  pattern = "*",
  callback = function()
    if vim.bo.modified and vim.bo.buftype == "" and vim.fn.expand("%") ~= "" then
      vim.cmd("silent! write")
    end
  end,
})

-- Trim trailing whitespace on save
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*",
  callback = function()
    local save_cursor = vim.fn.getpos(".")
    vim.cmd([[%s/\s\+$//e]])
    vim.fn.setpos(".", save_cursor)
  end,
})

-- Format with prettier on save
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = { "*.js", "*.ts", "*.jsx", "*.tsx", "*.html", "*.css", "*.json", "*.yaml", "*.md" },
  callback = function()
    local view = vim.fn.winsaveview()
    vim.cmd("silent! %!prettier --stdin-filepath " .. vim.fn.expand("%"))
    vim.fn.winrestview(view)
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "json",
  callback = function()
    vim.bo.formatprg = "jq --indent 2 --sort-keys"
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "http",
  callback = function()
    vim.keymap.set("n", "<leader>r", "<cmd>Rest run<cr>", { buffer = true })
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
