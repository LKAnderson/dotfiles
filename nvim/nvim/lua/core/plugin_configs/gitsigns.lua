require("gitsigns").setup()

vim.keymap.set("n", "<leader>bl", ":Gitsigns toggle_current_line_blame<CR>", { desc = "Toggle blame" })
