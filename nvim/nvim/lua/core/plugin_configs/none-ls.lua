local null_ls = require("null-ls")

null_ls.setup({
  sources = {
    null_ls.builtins.formatting.prettier.with({
      -- optional: specify filetypes or use defaults
      filetypes = { "javascript", "typescript", "html", "css", "json", "yaml", "markdown" },
    }),
  },
  on_attach = function(client, bufnr)
    -- Auto formatting has some irritating interactions with auto-saving....
    --
    -- if client.supports_method("textDocument/formatting") then
    --   -- Format on save
    --   vim.api.nvim_create_autocmd("BufWritePre", {
    --     buffer = bufnr,
    --     callback = function()
    --       vim.lsp.buf.format({ bufnr = bufnr, timeout_ms = 1000 })
    --     end,
    --   })
    -- end
  end,
})

vim.keymap.set("n", "<leader>fmt", function()
  vim.lsp.buf.format({ async = true })
end, { desc = "Format current buffer" })
