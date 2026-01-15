;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; Set up package management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Set default font
(set-face-attribute 'default nil :font "MesloLGM Nerd Font Mono-14")
(add-to-list 'default-frame-alist '(font . "MesloLGM Nerd Font Mono-14"))

;; Enable line numbers
(global-display-line-numbers-mode 1)

;; Disable backup files (no ~ turds)
(setq make-backup-files nil)

;; Editor settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
(setq-default standard-indent 2)
(setq c-basic-offset 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq case-fold-search t)
(setq search-upper-case t)
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; Prefer splitting below and right (like nvim splitbelow/splitright)
(setq split-window-preferred-function 'split-window-sensibly)

;; Word wrap settings (like nvim wrap/linebreak/breakindent)
(setq-default word-wrap t)
(setq-default truncate-lines nil)
(global-visual-line-mode 1)

;; Use y or n instead of yes or no
(setq use-short-answers t)

;; Highlight current line
(global-hl-line-mode 1)

;; Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Trim trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Install and configure evil-mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  ;; macOS clipboard keybindings (Cmd+C/X/V)
  (when (eq system-type 'darwin)
    (evil-define-key 'visual 'global (kbd "s-c") 'evil-yank)
    (evil-define-key 'visual 'global (kbd "s-x") (lambda () (interactive) (evil-yank (region-beginning) (region-end)) (evil-delete (region-beginning) (region-end))))
    (evil-define-key 'insert 'global (kbd "s-v") 'yank)
    (evil-define-key 'normal 'global (kbd "s-v") 'yank)))

;; Optional: evil-collection provides better evil bindings for many Emacs modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Evil leader key (backslash like nvim)
(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "\\")
  ;; File/search bindings (like Telescope)
  (evil-leader/set-key
    "f" 'project-find-file
    "s" 'consult-ripgrep
    "b" 'consult-buffer
    "r" 'lsp-find-references
    "d" 'flymake-show-buffer-diagnostics
    ;; LSP bindings
    "qf" 'lsp-execute-code-action
    "ic" 'lsp-find-incoming-calls
    "oc" 'lsp-find-outgoing-calls
    "def" 'lsp-find-definition
    "imp" 'lsp-find-implementation
    "sym" 'consult-lsp-file-symbols
    "wsym" 'consult-lsp-symbols
    ;; Git
    "bl" 'git-gutter:popup-hunk
    ;; Parameter swap (like treesitter textobjects)
    "a" 'transpose-sexps))

;; Magit - Git interface
(use-package magit
  :bind ("C-x g" . magit-status))

;; Git gutter - Show git changes in gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

;; Git blame (toggle with \bl)
(use-package git-modes)
(use-package blamer
  :config
  (setq blamer-idle-time 0.3)
  (setq blamer-min-offset 40)
  (evil-leader/set-key "bl" 'blamer-mode))

;; All-the-icons - Icon support for doom-modeline
(use-package all-the-icons
  :if (display-graphic-p))

;; Doom modeline - Enhanced status line
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon t))

;; Neotree - File tree sidebar (like nvim-tree)
(use-package neotree
  :config
  (setq neo-theme 'nerd)
  (setq neo-window-width 35)
  (setq neo-smart-open t)
  (setq neo-show-hidden-files t)
  (setq neo-autorefresh t)
  ;; t to focus neotree (like nvim NvimTreeFocus)
  (defun my/neotree-focus ()
    "Focus or open neotree at current file."
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-show)
      (neotree-find)))
  (evil-define-key 'normal 'global (kbd "t") 'my/neotree-focus)
  ;; gb for buffer list (like nvim Telescope buffers)
  (evil-define-key 'normal 'global (kbd "gb") 'consult-buffer)
  :bind ("<f8>" . neotree-toggle))

;; Evil support for neotree and comments
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Rainbow delimiters - Colorful brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; REST client (with \r to run request like nvim)
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (evil-define-key 'normal restclient-mode-map
    (kbd "\\r") 'restclient-http-send-current))

;; Tokyo Night theme
(use-package tokyonight-themes
  :vc (:url "https://github.com/xuchengpeng/tokyonight-themes")
  :config
  (load-theme 'tokyonight-moon :no-confirm))

;; Auto-revert files when they change on disk
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; Auto-save edited files
(auto-save-visited-mode 1)
(setq auto-save-visited-interval 2)

;; Save window size and position between sessions
(desktop-save-mode 1)
(setq desktop-restore-frames t)
(setq desktop-restore-in-current-display t)
(setq desktop-restore-eager 0)

;; Vertico - Vertical completion UI
(use-package vertico
  :init
  (vertico-mode))

;; Orderless - Fuzzy matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia - Rich annotations in minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;; Consult - Enhanced search and navigation commands
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-y" . consult-yank-pop))
  :config
  (setq consult-narrow-key "<"))

;; Embark - Actions on minibuffer candidates
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

;; Embark-Consult integration
(use-package embark-consult
  :after (embark consult))

;; Snippets (like LuaSnip)
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; Completion framework (keybindings matching nvim-cmp)
(use-package company
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1)
  ;; Keybindings matching nvim-cmp: C-Space, C-n/C-p, C-e, RET
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-e") 'company-abort)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map (kbd "<return>") 'company-complete-selection)
  (global-set-key (kbd "C-SPC") 'company-complete))

;; LSP support
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-snippet t
        lsp-headerline-breadcrumb-enable nil)
  ;; Evil keybindings matching nvim LSP attach (K, gd, gD, gi, go, gr, etc.)
  (evil-define-key 'normal lsp-mode-map
    "K" 'lsp-describe-thing-at-point
    "gd" 'lsp-find-definition
    "gD" 'lsp-find-declaration
    "gi" 'lsp-find-implementation
    "go" 'lsp-find-type-definition
    "gr" 'lsp-find-references
    "gm" 'flymake-show-buffer-diagnostics
    "gca" 'lsp-execute-code-action)
  (evil-define-key 'normal lsp-mode-map (kbd "C-k") 'lsp-signature-activate)
  (evil-define-key 'normal lsp-mode-map (kbd "<f2>") 'lsp-rename)
  (evil-define-key 'normal lsp-mode-map (kbd "<f3>") 'lsp-format-buffer)
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (kotlin-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (sh-mode . lsp-deferred)
         (fish-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (swift-mode . lsp-deferred)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-border "rounded"
        lsp-ui-sideline-enable t))

;; Consult integration with LSP for symbol search
(use-package consult-lsp
  :after (consult lsp-mode))

(use-package lsp-java
  :after lsp-mode)

;; DAP mode - Debug Adapter Protocol (like nvim-dap)
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

(use-package dap-ui
  :ensure nil
  :after dap-mode)

;; Evil text objects for tree-sitter (like nvim-treesitter-textobjects)
;; af/if for function, ac/ic for class
(use-package evil-textobj-tree-sitter
  :after evil
  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  ;; Navigation like nvim ]m [m for functions, ]] [[ for classes
  (define-key evil-normal-state-map (kbd "]m") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  (define-key evil-normal-state-map (kbd "[m") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  (define-key evil-normal-state-map (kbd "]]") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer")))
  (define-key evil-normal-state-map (kbd "[[") (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t))))

;; Tree-sitter support (Emacs 29+)
(when (>= emacs-major-version 29)
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

  ;; Auto-install tree-sitter grammars
  (dolist (lang '(bash c cpp go java javascript python rust typescript tsx))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang)))

  ;; Use tree-sitter modes when available
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (java-mode . java-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-mode . js-ts-mode)
          (python-mode . python-ts-mode)
          (sh-mode . bash-ts-mode)
          (rust-mode . rust-ts-mode)
          (typescript-mode . typescript-ts-mode))))

;; Format with prettier on save (like nvim BufWritePre autocmd)
(defun my/prettier-format-buffer ()
  "Format buffer with prettier if available."
  (when (and (executable-find "prettier")
             (member major-mode '(js-mode js-ts-mode javascript-mode
                                  typescript-mode typescript-ts-mode tsx-ts-mode
                                  web-mode html-mode css-mode
                                  json-mode json-ts-mode
                                  yaml-mode yaml-ts-mode
                                  markdown-mode)))
    (let ((point-before (point)))
      (shell-command-on-region (point-min) (point-max)
                               (concat "prettier --stdin-filepath " (buffer-file-name))
                               (current-buffer) t)
      (goto-char (min point-before (point-max))))))

(add-hook 'before-save-hook 'my/prettier-format-buffer)

;; JSON formatting with jq (like nvim formatprg)
(defun my/jq-format-buffer ()
  "Format JSON buffer with jq."
  (interactive)
  (when (and (executable-find "jq")
             (member major-mode '(json-mode json-ts-mode js-json-mode)))
    (shell-command-on-region (point-min) (point-max)
                             "jq --indent 2 --sort-keys"
                             (current-buffer) t)))

(add-hook 'json-mode-hook
          (lambda () (setq-local format-buffer-function 'my/jq-format-buffer)))

;; Language-specific modes
(use-package kotlin-mode)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package rust-mode)

(use-package go-mode)

(use-package swift-mode)

(use-package fish-mode)

;; Nushell mode
(use-package nushell-mode
  :mode "\\.nu\\'")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((tokyonight-themes :url
                        "https://github.com/xuchengpeng/tokyonight-themes"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
