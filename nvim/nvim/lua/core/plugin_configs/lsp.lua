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

-- Configure LSP servers using the new vim.lsp.config API
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

-- Enable the configured servers
vim.lsp.enable({ 'ts_ls', 'pyright', 'clangd' })

-- The following code came from https://vonheikemen.github.io/devlog/tools/neovim-lsp-client-guide/.
-- Thanks, man!

vim.api.nvim_create_autocmd('LspAttach', {
  desc = 'LSP actions',
  callback = function(event)
    local bufmap = function(mode, lhs, rhs)
      local opts = {buffer = event.buf}
      vim.keymap.set(mode, lhs, rhs, opts)
    end

    -- Trigger code completion
    bufmap('i', '<C-Space>', '<C-x><C-o>')

    -- Display documentation of the symbol under the cursor
    bufmap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>')

    -- Jump to the definition
    bufmap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>')

    -- Jump to declaration
    bufmap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>')

    -- Lists all the implementations for the symbol under the cursor
    bufmap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>')

    -- Jumps to the definition of the type symbol
    bufmap('n', 'go', '<cmd>lua vim.lsp.buf.type_definition()<cr>')

    -- Lists all the references
    bufmap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>')

    -- Displays a function's signature information
    bufmap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<cr>')

    -- Renames all references to the symbol under the cursor
    bufmap('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>')

    -- Format current file
    bufmap('n', '<F3>', '<cmd>lua vim.lsp.buf.format()<cr>')

    -- Selects a code action available at the current cursor position
    bufmap('n', '<leader>qf', '<cmd>lua vim.lsp.buf.code_action()<cr>')

    -- Lists all the diagnostics of the current buffer
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
