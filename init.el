;;; Emacs is not a package manager, and here we load its package manager!

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
))
(package-initialize)

;;(desktop-save-mode 1)
(global-linum-mode t)
(setq tramp-default-method "ssh")

;;; Define Packages to check for on startup
(defvar required-packages
  '(
    magit
    auto-complete
    yasnippet
    flycheck
    web-mode
    ag
    projectile
    perspective
    persp-projectile
    exec-path-from-shell
    jedi
    auctex
    auto-complete-auctex
    typescript
    tss
    yaxception ;; Typescript dependency
  ) "a list of packages to ensure are installed at launch.")

;;; method to check if all packages are installed
(require 'cl)
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;;; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(load-theme 'tango-dark t)

;; Add load-path
(add-to-list 'load-path "~/.emacs.d/lib")

;; Set PATH, MANPATH and exec-path from shell to emacs
;; Necessary for OSX
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; projectile for project management
(require 'projectile)
(projectile-global-mode)


;; perspective for switching workspaces
(persp-mode)
(require 'persp-projectile)
(define-key projectile-mode-map (kbd "C-c p w") 'projectile-persp-switch-project)

;;Copy without selection
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
	  (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )

(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe 
     	 (lambda()
     	   (if (string= "shell-mode" major-mode)
	       (progn (comint-next-prompt 25535) (yank))
	     (progn (goto-char (mark)) (yank) )))))
    (if arg
	(if (= arg 1)
	    nil
	  (funcall pasteMe))
      (funcall pasteMe))
    ))



(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )

(global-set-key (kbd "C-c w")         (quote copy-word))

(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  ;(paste-to-mark arg)
  )

(global-set-key (kbd "C-c l")         (quote copy-line))

;; turn on automatic bracket insertion by pairs. New in emacs 24
(electric-pair-mode 1)

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode 1)

;; Set re-builder environment to string by default
(require 're-builder)
(setq reb-re-syntax 'string)

;; Turn on snippets
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode t)

;; Remove Yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;; Use spaces not tabs
(setq indent-tabs-mode nil)

(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)


;; Retain indent on paste
(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (member major-mode '(emacs-lisp-mode lisp-mode
                                                      js-mode    scheme-mode
                                                      haskell-mode    ruby-mode
                                                      rspec-mode      python-mode
                                                      c-mode          c++-mode
                                                      objc-mode       latex-mode
                                                      plain-tex-mode))
                 (let ((mark-even-if-inactive transient-mark-mode))
                   (indent-region (region-beginning) (region-end) nil))))))


;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("mustache" . "\\.html\\'"))
      )

;; Load auctex
(load "auctex.el" nil t t)
(load "auto-complete-auctex.el" nil t t)


;; store all backup and autosave files in tmp directory of os
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; jedi config
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional
;; Type:
;;     M-x el-get-install RET jedi RET
;;     M-x jedi:install-server RET
;; Then open Python file.

;; Yaxception. Typescript dependency
(require 'yaxception)

;; Typescript syntax check/completion
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(setq tss-implement-definition-key "C-c i")

;; Make config suit for you. About the config item, eval the following sexp.
;; (customize-group "tss")

;; Do setting recommemded configuration
(tss-config-default)


;; Markdown mode
