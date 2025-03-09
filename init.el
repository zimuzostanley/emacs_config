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

(use-package super-save
  :ensure t
  :custom
  (super-save-auto-save-when-idle t)
  :config
  (super-save-mode +1))

(global-auto-revert-mode 1)
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer t t))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)          ; Show keys after 0.3 seconds
  (setq which-key-popup-type 'minibuffer)  ; Show in minibuffer
  (setq which-key-show-early-on-C-h t)     ; Show immediately for C-h
  (setq which-key-side-window-location 'bottom))

;; Install monokai theme
;;(use-package monokai-theme)

;; Load theme
;;(load-theme 'monokai t)

(menu-bar-mode -1)
(setq indent-tabs-mode nil)
(setq whitespace-style '(tabs newline))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq show-trailing-whitespace 'trailing-whitespace)
(setq indicate-empty-lines t)

;; (require 'google)
;; (require 'google-java-format)
;; (require 'google-diformat)

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
   ("C-x f" . fzf-git)
   ("C-x d" . counsel-find-file)
   ("C-c C-r" . counsel-recentf)
   ("C-x p" . counsel-ag)
   ("C-c p" . counsel-git-grep)))

(use-package treemacs
 :ensure t
 :bind
 (("C-c t" . treemacs)
  ("M-0" . treemacs-select-window))
 :config
 (treemacs-follow-mode t)
 (treemacs-filewatch-mode t)
 (treemacs-git-mode 'simple)
 :custom
 (treemacs-width 35)
 (treemacs-position 'left))

(use-package fzf
  :ensure t
  :bind
  ("C-x C-f" . fzf))

;; Install the rg package
(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))  ; Sets up default keybindings (C-c s)

(use-package deadgrep
  :ensure t
  :bind ("C-c h" . deadgrep))

;; Install and configure Swiper
(use-package swiper
  :after ivy
  :bind
  (("C-s" . counsel-grep-or-swiper)
   ("C-r" . counsel-rg)))

(setq counsel-grep-base-command
 "rg -i --no-heading --line-number --color never '%s' %s")

;; Some basic quality of life settings
(setq-default
 inhibit-startup-screen t    ; Disable startup screen
 make-backup-files nil       ; Disable backup files
 auto-save-default nil)      ; Disable auto save

;; Enable line numbers
(global-display-line-numbers-mode)

;; Show matching parentheses
(show-paren-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 4)

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

(unless (display-graphic-p)
  (defun osc52-copy (string &optional target)
    (let* ((target (or target "clipboard"))
           (str (base64-encode-string string t))
           (data (concat "\e]52;" target ";" str "\e\\"))
           (max-size 100000))
      (when (> (length data) max-size)
        (error "Selection too long to copy to terminal (%d vs %d)"
               (length data) max-size))
      (send-string-to-terminal data)))

  (setq interprogram-cut-function 'osc52-copy))

(use-package bm
 :ensure t
 :init
 :bind
 (("C-c b t" . bm-toggle)
  ("C-c b n" . bm-next)
  ("C-c b p" . bm-previous)
  ("C-c b s" . bm-show)
  ("C-c b a" . bm-show-all)))

(use-package magit
  :ensure t
  :init
  :bind
  (("C-x g" . magit-status)
   ("C-x G" . magit-blame)
   ("C-x L" . magit-log-buffer-file))
)

;; Enable recent files mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(defun my-cpp-mode-setup ()
  "Setup for C++ mode"
  (setq-local tab-width 4)
  (setq-local c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (setq c-default-style "stroustrup"))

(add-hook 'c++-mode-hook 'my-cpp-mode-setup)

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package python-mode
  :custom
  ;; Use python3 by default
  (python-shell-interpreter "python3"))

;; Performance settings
  ;; Increase read/write timeout
(setq eglot-connect-timeout 30)
(setq eglot-sync-connect nil)

;; Configure events to ignore (improves performance)
(setq eglot-ignored-server-capabilities '(:documentHighlightProvider))

;; Performance optimizations
(setq eglot-send-changes-idle-time 0.5)

;; Set larger limits for communicating with servers
(setq eglot-events-buffer-size 0) ; Disable events buffer
(setq eglot-confirm-server-initiated-edits nil) ; Don't ask to confirm edits


;; Basic Eglot setup
(use-package eglot
  :ensure t
  :hook (
         (c++-mode . eglot-ensure)
         (c-mode . eglot-ensure)

         ;; TypeScript/JavaScript
         (typescript-mode . eglot-ensure)
         (typescript-tsx-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js2-mode . eglot-ensure)

         ;; Rust
         (rust-mode . eglot-ensure)
         (rustic-mode . eglot-ensure)

         ;; Python
         (python-mode . eglot-ensure)

         ;; SCSS/CSS
         (scss-mode . eglot-ensure)
         (css-mode . eglot-ensure)

         ;; HTML
         (html-mode . eglot-ensure)
         (web-mode . eglot-ensure)
         )

    ;; Keybindings
  :bind (:map eglot-mode-map
              ("M-RET" . eglot-code-actions)
              ("M-." . xref-find-definitions)
              ("M-?" . xref-find-references)
              ("M-," . xref-pop-marker-stack))

  :config
  ;; Shutdown server when buffer is killed
  (setq eglot-autoshutdown t)

  ;; Set LSP server paths/configurations
  (add-to-list 'eglot-server-programs
               `((typescript-mode typescript-tsx-mode js-mode js2-mode)
                 . ("typescript-language-server" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((rust-mode rustic-mode) . ("rust-analyzer")))

  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((scss-mode css-mode) . ("vscode-css-language-server" "--stdio")))

  (add-to-list 'eglot-server-programs
               '((html-mode web-mode) . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               `((c-mode c++-mode)
                 . ("clangd"
                    "--compile-commands-dir=${workspaceFolder}"
                    "--clang-tidy"
                    "--header-insertion=iwyu"
                    "--completion-style=detailed"
                    "-j=4"
                    "--query-driver=/usr/bin/clang"))))

(use-package consult
  :ensure t
  :config
  ;; Configure xref to use consult
  (setq xref-show-definitions-function #'consult-xref
        xref-show-xref-function #'consult-xref))

;; Make sure you have the required language servers installed:
;; - typescript-language-server: npm install -g typescript-language-server typescript
;; - rust-analyzer: https://rust-analyzer.github.io/manual.html#installation
;; - pyright: pip install pyright
;; - vscode-css-language-server: npm install -g vscode-langservers-extracted
;; - vscode-html-language-server: npm install -g vscode-langservers-extracted

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

(use-package kconfig-mode
  :ensure t)

(use-package vterm
  :ensure t)

;; Move to personal
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "M-r"))

(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package prettier
  :ensure t
  :hook ((typescript-mode js-mode) . prettier-mode))

(use-package helm-gtags
  :ensure t
  :bind (("M-t" . helm-gtags-find-tag) ; Better to use other-window version to prevent conflicts
         ("M-." . helm-gtags-find-tag)
         ("M-," . helm-gtags-pop-stack)
         ("M-r" . helm-gtags-find-rtag)
         ("M-?" . helm-gtags-find-rtag)
	 ("M-p" . helm-show-kill-ring))
  :hook ((c-mode . helm-gtags-mode)
         (c++-mode . helm-gtags-mode)
         (java-mode . helm-gtags-mode))
  )

(use-package kotlin-mode
 :ensure t
 :mode "\\.kt\\'")

(defun format-typescript-perfetto ()
  "Format the current typescript file in the Perfetto project."
  (interactive)
  (when (and buffer-file-name
             (or (string= (file-name-extension buffer-file-name) "ts")
                 (string= (file-name-extension buffer-file-name) "js")
                 (string= (file-name-extension buffer-file-name) "scss")))
    (let* ((root-dir (locate-dominating-file default-directory "ui"))
           (format-script "/usr/local/ssd/extra/android/perfetto/ui/format-sources"))
      (when root-dir
        (message "Formatting %s" buffer-file-name)
        (shell-command (format "%s %s" format-script buffer-file-name))
        (revert-buffer t t t)))))

(defun setup-typescript-formatting ()
  "Set up formatting hooks for TypeScript/JavaScript/SCSS files."
  (when (and buffer-file-name
             (or (string= (file-name-extension buffer-file-name) "ts")
                 (string= (file-name-extension buffer-file-name) "js")
                 (string= (file-name-extension buffer-file-name) "scss")))))

;; Add this hook to relevant major modes
(add-hook 'typescript-mode-hook 'setup-typescript-formatting)
(add-hook 'js-mode-hook 'setup-typescript-formatting)
(add-hook 'js2-mode-hook 'setup-typescript-formatting)
(add-hook 'web-mode-hook 'setup-typescript-formatting)
(add-hook 'scss-mode-hook 'setup-typescript-formatting)
(add-hook 'css-mode-hook 'setup-typescript-formatting)

;; Or bind to a key for manual formatting
(global-set-key (kbd "C-c f") 'format-typescript-perfetto)

;; Install and configure vlf (View Large Files)
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup)
  ;; Set the size threshold for VLF mode (in bytes)
  (setq vlf-application-threshold 10000000) ; 1MB threshold
  ;; Add vlf to file-name-handler-alist
  (custom-set-variables
   '(vlf-application-prefer-external nil))
  ;; Optional: Add a key binding to manually trigger vlf-mode
  :bind (("C-c v" . vlf-mode)))

;; ;; Logcat highlight
;; (defface logcat-error
;;   '((t :foreground "#FF6B68" ))
;;   "Face for global variables."
;;   :group 'logcat )
;; (defvar logcat-error 'logcat-error)

;; (defface logcat-warn
;;   '((t :foreground "#BBB529" ))
;;   "Face for global variables."
;;   :group 'logcat )
;; (defvar logcat-warn 'logcat-warn)

;; (defface logcat-info
;;   '((t :foreground "#6A8759" ))
;;   "Face for global variables."
;;   :group 'logcat)
;; (defvar logcat-info 'logcat-info)

;; (defface logcat-debug
;;   '((t :foreground "#6897BB" ))
;;   "Face for global variables."
;;   :group 'logcat )
;; (defvar logcat-debug 'logcat-debug)

;; ;; (setq logcat-highlights
;; ;;       '(("^.* (E|c0) .*$" . logcat-error)
;; ;;         ("^.* (W|c1) .*$" . logcat-warn)
;; ;;         ("^.* (I|c2) .*$" . logcat-info)
;; ;;         ("^.* (D|c3) .*$" . logcat-debug)))

;; (setq logcat-highlights
;;       '(("^.* E .*$" . logcat-error)
;;         ("^.* W .*$" . logcat-warn)
;;         ("^.* I .*$" . logcat-info)
;;         ("^.* D .*$" . logcat-debug)))

;; (define-derived-mode adb-logcat fundamental-mode "logcat"
;;   "major mode for adb logcat"
;;   (visual-line-mode)
;;   (setq font-lock-defaults `(logcat-highlights t)))

;; (defun bugreport ()
;;   (interactive)
;;   (visual-line-mode)
;;   (fundamental-mode)
;;   (buffer-disable-undo)
;;   (font-lock-mode -1)
;;   (setq buffer-read-only t))

;; (rg-define-search search-bugreport
;;   "Search bugreport"
;;   :query "was the duration of"
;;   :format literal
;;   :files "everything"
;;   :flags ("--text")
;;   :dir current
;;   :menu ("Search" "b" "Bugreport"))

(setq gc-cons-threshold 10000000)

(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between beg & end into kill ring."
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun copy-word (&optional arg)
      "Copy words at point into kill-ring"
       (interactive "P")
       (copy-thing 'backward-word 'forward-word arg)
       ;;(paste-to-mark arg)
      )
 (defun copy-line (&optional arg)
      "Save current line into Kill-Ring without mark the line "
       (interactive "P")
       (copy-thing 'beginning-of-line 'end-of-line arg)
       )

(global-set-key (kbd "C-c w")         (quote copy-word))
(global-set-key (kbd "C-c l")         (quote copy-line))

;;(global-set-key (kbd "M-B") 'bugreport)
;;(global-set-key (kbd "M-l") 'adb-logcat)

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode t)
  (diff-hl-margin-mode t)
  (setq diff-hl-draw-borders t)
  (setq diff-hl-flydiff-delay 0.1)
  :config
  ;; Update diff-hl after git operations
  ;;(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  ;; Update diff-hl after saving
  (add-hook 'after-save-hook #'diff-hl-update))

;; Protobuf mode configuration
(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'")

(use-package sqlformat
  :ensure t)

(defvar import-line-regexp
  (concat "^import[ \t]+\\([a-zA-Z0-9_]+\\.\\)*"
          "\\([a-zA-Z0-9_]+\\)[ \t]*;"))

(defun delete-bogus-imports ()
  (interactive)
  (setq starting-position (point-marker))
  (let ((deletes (delete-bogus-imports-1)))
    (cond ((> deletes 0)
           (goto-char (point-min))
           (or (re-search-forward import-line-regexp nil t)
               (re-search-forward "^package" nil t))
           (beginning-of-line))
          (t
           (message "No imports deleted")
           (goto-char starting-position)))))


(defun delete-bogus-imports-1 ()
  (let ((current-pos 0)
        (current-class "")
        (last-import-pos 0)
        (deletes 0)
        (initialized nil))
    (save-excursion
      (end-of-buffer)
      ;; Look for last import line in file
      (cond ((re-search-backward import-line-regexp nil t)

             ;; Remember where last import line is so we can search
             ;; starting from that point for uses of a class
             (end-of-line)
             (setq last-import-pos (point-marker))
             (goto-char last-import-pos)

             ;; Loop through the import lines
             (goto-char (point-min))
             (while (re-search-forward import-line-regexp nil t)
               (setq current-pos (point-marker))
               (setq current-class (match-string 2))

               ;; search for the package in the body of the file
               (goto-char last-import-pos)
               (let ((case-fold-search nil)
                     (word-regexp (concat "\\b" (regexp-quote current-class)
                                          "\\b")))
                 (cond ((re-search-forward word-regexp nil t)
                        ;; found -- do next
                        (goto-char current-pos)
                        )
                       (t
                        ;; not found, delete line into kill ring
                        (goto-char current-pos)

                        (cond ((not initialized)
                               (setq initialized t)
                               ;; Initialize kill ring
                               (kill-region (point) (point))))

                        (let ((kill-whole-line t))
                          (setq deletes (+ deletes 1))
                          (beginning-of-line)
                          (append-next-kill)
                          (kill-line))))))

             ;; Sort import lines
             (goto-char (point-min))
             (cond ((re-search-forward import-line-regexp nil t)
                    (beginning-of-line)
                    (sort-lines nil (point) last-import-pos)
                    (if remove-duplicate-imports
                        (setq deletes (+ deletes
                                         (delete-duplicates-in-region (point) last-import-pos t)))))))))
    (sit-for 0)
    (if (not (= deletes 0))
        (message (concat (number-to-string deletes) " deleted imports in kill ring.")))
    deletes))

(defun delete-duplicates-in-region (start-position end-position append-lines-p)
  (interactive (list (region-beginning) (region-end) nil))
  (if (> start-position end-position)
      (let ((temp start-position))
        (setq start-position end-position)
        (setq end-position temp)))
  (save-excursion
    (let ((current-position 0)
          (prev-line "")
          (current-line "")
          (current-position 0)
          (first-kill (not append-lines-p))
          (deletes 0))

      (goto-char end-position)
      (beginning-of-line)
      (setq prev-line (buffer-substring (point) end-position))

      (forward-line -1)
      (beginning-of-line)
      (setq current-position (point-marker))
      (catch 'bol
        (while (>= current-position start-position)
          (beginning-of-line)
          (let ((bol (point-marker)))
            (end-of-line)
            (setq current-line (buffer-substring bol (point))))
          (cond ((equal current-line prev-line)
                 ;;(message (concat "delete " current-line)) (sit-for 1)
                 (beginning-of-line)
                 (if (and (not first-kill) (not (null kill-ring)))
                     (append-next-kill))
                 (let ((kill-whole-line t))
                   (kill-line))
                 (setq deletes (+ deletes 1))
                 (setq first-kill nil)))
          (if (< (forward-line -1) 0) (throw 'bol t))
          (setq current-position (point-marker))
          (setq prev-line current-line)))
      deletes)))

;; Install and configure Company mode for completion
(use-package company
;;  :after lsp-mode
  :config
  (global-company-mode 1)
  (setq company-idle-delay 1          ; Delay before showing suggestions
        company-minimum-prefix-length 1  ; Show suggestions after typing one character
        company-selection-wrap-around t  ; Wrap around to top when reaching bottom of suggestions
        company-tooltip-align-annotations t ; Align annotations to the right
        company-require-match nil)       ; Don't require an exact match
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company)))
