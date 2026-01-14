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

