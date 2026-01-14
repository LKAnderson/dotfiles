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
  :config
  (evil-mode 1))

;; Optional: evil-collection provides better evil bindings for many Emacs modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Magit - Git interface
(use-package magit
  :bind ("C-x g" . magit-status))

;; Git gutter - Show git changes in gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

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

;; Neotree - File tree sidebar
(use-package neotree
  :config
  (setq neo-theme 'nerd)
  (setq neo-window-width 35)
  (setq neo-smart-open t)
  (setq neo-show-hidden-files t)
  (evil-define-key 'normal 'global (kbd "t") 'neotree-toggle)
  :bind ("<f8>" . neotree-toggle))

;; Evil support for neotree and comments
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Rainbow delimiters - Colorful brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; REST client
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

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

;; Completion framework
(use-package company
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

;; LSP support
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-snippet t
        lsp-headerline-breadcrumb-enable nil)
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
        lsp-ui-sideline-enable t))

(use-package lsp-java
  :after lsp-mode)

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
 '(package-selected-packages
   '(all-the-icons company consult doom-modeline doom-themes embark
                   embark-consult evil-collection evil-nerd-commenter
                   fish-mode git-gutter go-mode kotlin-mode lsp-java
                   lsp-mode lsp-ui magit marginalia neotree
                   nushell-mode orderless rainbow-delimiters
                   restclient rust-mode swift-mode tokyonight-themes
                   typescript-mode vertico))
 '(package-vc-selected-packages
   '((tokyonight-themes :url
                        "https://github.com/xuchengpeng/tokyonight-themes"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
