;; Initialize package sources
(require 'package)

;; Add MELPA repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialize package system
(package-initialize)

;; Refresh package contents on first load
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)          ; Show keys after 0.3 seconds
  (setq which-key-popup-type 'minibuffer)  ; Show in minibuffer
  (setq which-key-show-early-on-C-h t)     ; Show immediately for C-h
  (setq which-key-side-window-location 'bottom))

;; Install and configure Ivy
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-x b" . ivy-switch-buffer)
   ("C-c v" . ivy-push-view)
   ("C-c V" . ivy-pop-view)))

;; Install and configure Counsel (adds Ivy-powered versions of common commands)
(use-package counsel
  :after ivy
  :config (counsel-mode 1)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c C-r" . counsel-recentf)
   ("C-x f" . counsel-fzf)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)))

;; Install and configure Swiper
(use-package swiper
  :after ivy
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper-backward)))

;; Some basic quality of life settings
(setq-default
 inhibit-startup-screen t    ; Disable startup screen
 make-backup-files nil       ; Disable backup files
 auto-save-default nil)      ; Disable auto save

;; Enable line numbers
(global-display-line-numbers-mode)

;; Show matching parentheses
(show-paren-mode 1)

(electric-pair-mode 1)

;; Basic clipboard settings
(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; Install and enable xclip for terminal mode
(unless (package-installed-p 'xclip)
  (package-refresh-contents)
  (package-install 'xclip))

(require 'xclip)
(xclip-mode 1)

;; Enable recent files mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :custom
  ;; Use python3 by default
  (python-shell-interpreter "python3"))

;; Update LSP mode configuration to include Python
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((c-mode . lsp)
   (c++-mode . lsp)
   (typescript-mode . lsp)
   (python-mode . lsp-deferred))  ; Add Python hook
  :commands (lsp lsp-deferred)
  :config
  ;; Performance tweaks for LSP
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-idle-delay 0.500)
  ;; Configure pyright
  (setq lsp-pyright-use-library-code-for-types t
        lsp-pyright-auto-import-completions t
        lsp-pyright-auto-search-paths t))

(defun linux-kernel-coding-style ()
  "Apply Linux kernel coding style"
  (interactive)
  (setq-local tab-width 8)
  (setq-local indent-tabs-mode t)
  (setq-local c-basic-offset 8)
  (c-set-style "linux"))

;; Apply kernel style for kernel files
(add-hook 'c-mode-hook
          (lambda ()
            (when (string-match "/linux" (buffer-file-name))
              (linux-kernel-coding-style))))

;; LSP UI for additional features
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t             ; Enable documentation popups
        lsp-ui-doc-show-with-cursor t   ; Show doc when cursor moves
        lsp-ui-doc-delay 0.5            ; Delay before showing doc
        lsp-ui-doc-position 'at-point   ; Show doc at point (alternatives: 'top 'bottom)
        lsp-ui-doc-max-height 30        ; Max height of doc window
        lsp-ui-peek-enable t            ; Enable peek feature
        lsp-ui-sideline-enable t        ; Enable sideline info
        lsp-ui-sideline-show-diagnostics t  ; Show diagnostics in sideline
        lsp-ui-sideline-show-code-actions t) ; Show code actions in sideline
  :bind
  (:map lsp-ui-mode-map
        ("M-." . lsp-ui-peek-find-definitions)     ; Peek definition
        ("M-?" . lsp-ui-peek-find-references)      ; Peek references
        ("C-c i" . lsp-ui-imenu)                   ; Symbol overview
        ("C-c d" . lsp-ui-doc-show)))


;; Install and configure Company mode for completion
(use-package company
  :after lsp-mode
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0.1          ; Delay before showing suggestions
        company-minimum-prefix-length 1  ; Show suggestions after typing one character
        company-selection-wrap-around t  ; Wrap around to top when reaching bottom of suggestions
        company-tooltip-align-annotations t ; Align annotations to the right
        company-require-match nil)       ; Don't require an exact match
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("M-n" . company-select-next)
        ("M-p" . company-select-previous)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(python-mode typescript-mode xclip which-key company lsp-ui lsp-mode counsel ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
