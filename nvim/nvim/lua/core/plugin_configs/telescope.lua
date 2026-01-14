vim.cmd "nnoremap <leader>f :Telescope find_files<CR>"
vim.cmd "nnoremap <leader>s :Telescope live_grep<CR>"
vim.cmd "nnoremap gb :Telescope buffers<CR>"
vim.cmd "nnoremap <leader>b :Telescope buffers<CR>"

-- LSP
vim.cmd "nnoremap <leader>r :Telescope lsp_references<CR>"
vim.cmd "nnoremap <leader>ic :Telescope lsp_incoming_calls<CR>"
vim.cmd "nnoremap <leader>oc :Telescope lsp_outgoing_calls<CR>"
vim.cmd "nnoremap <leader>def :Telescope lsp_definitions<CR>"
vim.cmd "nnoremap <leader>imp :Telescope lsp_implementations<CR>"
vim.cmd "nnoremap <leader>sym :Telescope lsp_document_symbols<CR>"
vim.cmd "nnoremap <leader>wsym :Telescope lsp_dynamic_workspace_symbols<CR>"

