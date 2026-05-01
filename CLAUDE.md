# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

A macOS dotfiles repo. Each tool's config lives in a `<tool>/<tool>/` subdirectory (e.g. `nvim/nvim/`, `nushell/nushell/`). `install.sh` symlinks these into `~/.config/` and a few macOS-specific locations.

## Installation

```sh
./install.sh
```

This creates symlinks — it does not copy files. Editing files in this repo immediately affects the live config.

## Fixing problems in config files

**Before modifying a config file to work around a bug, check whether an update to the relevant tool or plugin fixes it first.** Only make config-level workarounds if no upstream fix is available or forthcoming.

- Neovim plugins: `:Lazy update <plugin-name>`
- Neovim itself: update via Homebrew (`brew upgrade neovim`)
- Nushell: `brew upgrade nushell`

## Machine-specific files (gitignored)

Two files are intentionally excluded from version control and must be created locally:

- `nushell/nushell/extras.nu` — machine-specific shell config; copy from `extras.nu.starter`
- `git/git/config.local` — local git identity (name, email, templateDir); copy from `config.local.template`

## Neovim

`nvim/nvim/init.lua` is a single-file config using [lazy.nvim](https://github.com/folke/lazy.nvim). Plugin lock file is `nvim/nvim/lazy-lock.json`. Per-project nvim config is supported via `vim.opt.exrc = true` (`.nvim.lua` in project roots).

LSP is configured using the Neovim 0.11+ native API (`vim.lsp.config` / `vim.lsp.enable`) rather than nvim-lspconfig.

## Nushell

`nushell/nushell/config.nu` is the entry point. It sources `functions.nu` (shared functions) and `extras.nu` (machine-specific, gitignored). On macOS, `~/Library/Application Support/nushell` is symlinked to `~/.config/nushell` by `install.sh`.
