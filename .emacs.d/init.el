;;; .emacs --- Emacs initialization file -*- lexical-binding: t; -*-

;;; Commentary:

;; Welcome to Emacs (http://go/emacs).
;;
;; If you see this file, your homedir was just created on this workstation.
;; That means either you are new to Google (in that case, welcome!) or you
;; got yourself a faster machine.
;;
;; Either way, the main goal of this configuration is to help you be more
;; productive; if you have ideas, praise or complaints, direct them to
;; emacs-users@google.com (http://g/emacs-users).  We'd especially like to hear
;; from you if you can think of ways to make this configuration better for the
;; next Noogler.
;;
;; If you want to learn more about Emacs at Google, see http://go/emacs.

;;; Code:

;; Use the 'google' package by default.
;; My theme :)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'exec-path "/usr/bin" "/usr/local/bin")
(package-initialize)
(load-file "~/.emacs.d/guess-offset.el")

(load-theme 'monokai t)

;; Emacs color background transparent
(set-background-color "ARGBBB000000")

;; Save backup and temporary files in tmp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

(set-default 'truncate-lines t)
(setq isearch-allow-scroll t)
(require 'highlight-symbol)

(global-auto-revert-mode 1)
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer t t))


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

(defun paste-to-mark (&optional arg)
  "Paste things to mark, or to the prompt in shell-mode."
  (unless (eq arg 1)
    (if (string= "shell-mode" major-mode)
        (comint-next-prompt 25535)
      (goto-char (mark)))
    (yank)))

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

;;(setq x-select-enable-clipboard t)
(require 'xclip)
(xclip-mode 1)

(require 'bm)
(global-set-key (kbd "C-c b t") 'bm-toggle)
(global-set-key (kbd "C-c b n") 'bm-next)
(global-set-key (kbd "C-c b p") 'bm-previous)
(global-set-key (kbd "C-c b s") 'bm-show)

(setq smerge-command-prefix (kbd "C-c v"))

;; turn off whitespace checking:
(setq vdiff-diff-program "git diff")
(setq vdiff-diff-extra-args "diff --no-index --patience")

(global-set-key (kbd "M-b") 'bugreport)
(global-set-key (kbd "M-l") 'adb-logcat)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x G") 'magit-blame)
(global-set-key (kbd "C-x L") 'magit-log-buffer-file)
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-o") 'other-window)

(setq vc-handled-backends nil)
(setq magit-refresh-status-buffer nil)
(setq auto-revert-buffer-list-filter
      'magit-auto-revert-repository-buffers-p)

(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

;; I don't use the default binding of 'C-x 5', so use toggle-frame-split instead

(global-set-key (kbd "C-x 5") 'toggle-frame-split)

;; magit-find-file
(global-set-key (kbd "C-x f") 'counsel-fzf)
(global-set-key (kbd "C-x F") 'magit-find-file)
(global-set-key (kbd "C-x m") 'android-master)

(setq-default gdb-display-io-nopopup t)

(require 'iedit)
(global-set-key (kbd "C-c ;") 'iedit-mode)

;; Matches regex without need for escaping
(require 're-builder)
(setq reb-re-syntax 'string)

;; Show line numbers
(global-linum-mode t)

;; turn on automatic bracket insertion by pairs. New in emacs 24
(electric-pair-mode 1)

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 4)

(require 'vlf-setup)

;;disable splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; turn off as much chrome as possible...
(menu-bar-mode -1)
(if (and (boundp 'tool-bar-mode) tool-bar-mode) ;; conditional Mac OS X
    (tool-bar-mode -1))
(if (boundp 'toggle-scroll-bar) ;; conditional Mac OS X
    (toggle-scroll-bar -1))

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)))

;; turn on column numbers by default
(column-number-mode 1)

(global-column-enforce-mode t)
(setq column-enforce-column 100)

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))
(global-set-key (kbd "C-x t p")         (quote move-line-up))
(global-set-key (kbd "C-x t n")         (quote move-line-down))

;(Setq counsel-ag-base-command "ag --no-color -G 'frameworks/*|system/*|external/libfuse/*|bionic/*|art/*|libcore/*' --no-group %s -- .")
(setq counsel-ag-base-command "ag --no-color --ignore-dir={'out*','compatibility','sdk','pdk','platform_testing','hardware','prebuilts','device','kernel','log','test','tests','vendor','external','.repo'} --no-group %s -- .")

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-re-builders-alist
      '((t . ivy--regex)))
(global-set-key (kbd "C-r") 'swiper-isearch)
(global-set-key (kbd "C-s") 'ivy-isearch)
(global-set-key (kbd "C-x p") 'counsel-ag)
(global-set-key (kbd "C-x P") 'counsel-git-grep)
(global-set-key (kbd "M-x") 'counsel-M-x)
(setq ivy-height-alist
      '((t
         lambda (_caller)
         (/ (frame-height) 3))))
(defun ivy-isearch-function (s)
  (when (> (length s) 0)
    (let ((re (setq ivy--old-re (ivy--regex-plus s)))
          res)
      (with-ivy-window
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (push
           (propertize
            (buffer-substring-no-properties
             (line-beginning-position)
             (line-end-position))
            'pos (match-end 0))
           res))
        (nreverse res)))))

(defun ivy-isearch-action (x)
  (let ((pt (and x (get-text-property 0 'pos x))))
    (when pt
      (goto-char pt))))

(defun ivy--isearch-update-input ()
  (when ivy--old-re
    (swiper--cleanup)
    (with-ivy-window
      (ivy-isearch-action (ivy-state-current ivy-last))
      (swiper--add-overlays
       ivy--old-re
       (window-start)
       (window-end (selected-window) t)))))

(defun ivy-isearch ()
  (interactive)
  (ivy-read "Swiper: " #'ivy-isearch-function
            :dynamic-collection t
            :action #'ivy-isearch-action
            :update-fn #'ivy--isearch-update-input
            :unwind #'swiper--cleanup
            :preselect (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))))

(setq gc-cons-threshold 10000000)
;; ----bdc
(setq-default python-indent 2)

(setq c-electric-flag nil)

(jdecomp-mode 1)
(customize-set-variable 'jdecomp-decompiler-paths
                        '((cfr . "~/Downloads/cfr-0.149.jar")))

(c-add-style "android-java" '("java"
			      (c-basic-offset . 4)
			      (c-offsets-alist . ((arglist-intro  . ++)
                                                  (case-label            . +)
						  (statement-case-intro  . +)
						  (statement-cont        . ++)
                                                  (topmost-intro . 0)
						  (topmost-intro-cont    . ++)
						  (arglist-cont-nonempty . ++)
                                                  (access-label   . 0)
						  (func-decl-cont        . ++)))))
(c-add-style "android-c"    '("gnu"
			      (c-basic-offset . 4)
			      (c-offsets-alist . ((case-label            . +)
						  (statement-case-intro  . +)
						  (statement-cont        . ++)
						  (topmost-intro-cont    . ++)
						  ;;(arglist-cont-nonempty . ++)
						  (func-decl-cont        . ++)))))

(setq c-default-style '((java-mode) . "android-java"))
(setq c-default-style '((c-mode) . "android-c"))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq indent-tabs-mode nil)
(setq whitespace-style '(tabs newline))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq show-trailing-whitespace 'trailing-whitespace)
(setq indicate-empty-lines t)

(defun android-aosp ()
    "Open Android AOSP root dir"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/aosp"))

(defun android-master ()
    "Open Android master root dir"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/master"))

(defun android-r ()
    "Open Android R root dir"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/rvc-dev"))

(defun android-master-system-core ()
    "Open Android master system core"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/master/system/core"))

(defun android-master-system-sepolicy ()
    "Open Android master system sepolicy"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/master/system/sepolicy"))

(defun android-master-system-vold ()
    "Open Android master system vold"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/master/system/vold"))

(defun android-master-frameworks-base ()
    "Open Android master frameworks base"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/master/frameworks/base"))

(defun android-master-frameworks-native ()
    "Open Android master frameworks native"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/master/frameworks/native"))

(defun android-master-packages-providers-mediaprovider ()
    "Open Android master MediaProvider"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/master/packages/providers/MediaProvider"))

(defun android-master-cts ()
    "Open Android master CTS"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/master/cts"))

(defun android-qtdev ()
    "Open Android qt-dev root dir"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/qt-dev"))

(defun android-kernel-wahoo ()
    "Open Android kernel-wahoo root dir"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/kernel-wahoo/private/msm-google"))

(defun android-kernel-bluecross ()
    "Open Android kernel-bluecross root dir"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/kernel-bluecross/private/msm-google"))

(defun android-kernel-torvalds ()
    "Open Android kernel-torvalds root dir"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/kernel-torvalds"))

(defun android-libfuse ()
    "Open Android libfuse root dir"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/master/external/libfuse"))

(defun android-sqlite ()
    "Open Android sqlite root dir"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/master/external/sqlite"))

(defun android-art ()
    "Open Android art root dir"
    (interactive)
    (find-file "/usr/local/ssd/extra/android/master/art"))

;; Logcat highlight
(defface logcat-error
  '((t :foreground "#FF6B68" ))
  "Face for global variables."
  :group 'logcat )
(defvar logcat-error 'logcat-error)

(defface logcat-warn
  '((t :foreground "#BBB529" ))
  "Face for global variables."
  :group 'logcat )
(defvar logcat-warn 'logcat-warn)

(defface logcat-info
  '((t :foreground "#6A8759" ))
  "Face for global variables."
  :group 'logcat)
(defvar logcat-info 'logcat-info)

(defface logcat-debug
  '((t :foreground "#6897BB" ))
  "Face for global variables."
  :group 'logcat )
(defvar logcat-debug 'logcat-debug)

;; (setq logcat-highlights
;;       '(("^.* (E|c0) .*$" . logcat-error)
;;         ("^.* (W|c1) .*$" . logcat-warn)
;;         ("^.* (I|c2) .*$" . logcat-info)
;;         ("^.* (D|c3) .*$" . logcat-debug)))

(setq logcat-highlights
      '(("^.* E .*$" . logcat-error)
        ("^.* W .*$" . logcat-warn)
        ("^.* I .*$" . logcat-info)
        ("^.* D .*$" . logcat-debug)))

(setq comint-buffer-maximum-size 1000)
(define-derived-mode adb-logcat comint-mode "logcat"
  "major mode for adb logcat"
  (setq font-lock-defaults '(logcat-highlights))
  (unless (comint-check-proc (current-buffer))
    (let ((fake-proc
           (start-process "adb-logcat" (current-buffer) "cat")))
      (set-process-query-on-exit-flag fake-proc nil)
      (set-marker (process-mark fake-proc) (point)))))

(add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
(setq comint-buffer-maximum-size 5000)

(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

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
;; ----bdc


(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)
(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)

(global-unset-key (kbd "C-t"))

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
;; Enable helm-gtags-mode in Dired so you can jump to any tag
;; when navigate project tree with Dired
(add-hook 'dired-mode-hook 'helm-gtags-mode)

(setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)
(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "<f5>") 'helm-gtags-find-tag-from-here))

(require 'helm-gtags)
;;(require 'gtags)

(set-default 'semantic-case-fold t)

(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun bugreport ()
  (interactive)
  (fundamental-mode)
  (buffer-disable-undo)
  (linum-mode -1)
  (font-lock-mode -1)
  (setq buffer-read-only t))

(require 'which-key)
(which-key-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   (quote
    ((java-mode . "android-java")
     (awk-mode . "awk")
     (other . "android-c"))))
 '(compile-command "~/Dev/build_android.sh")
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(explicit-bash-args (quote ("--noediting" "-il")))
 '(indent-tabs-mode nil)
 '(magit-log-section-arguments (quote ("--graph" "--decorate" "-n256")))
 '(package-selected-packages
   (quote
    (jdecomp realgud helm-fuzzy-find fzf hl-anything function-args helm-cscope swiper-helm helm-ag helm-gtags go meson-mode kotlin-mode column-enforce-mode xclip rainbow-delimiters monokai-theme highlight-symbol solarized-theme company vlf iedit which-key magit wgrep counsel all-the-icons-ivy swiper ivy)))
 '(safe-local-variable-values
   (quote
    ((c-file-offsets
      (block-close . 0)
      (brace-list-close . 0)
      (brace-list-entry . 0)
      (brace-list-intro . +)
      (case-label . 0)
      (class-close . 0)
      (defun-block-intro . +)
      (defun-close . 0)
      (defun-open . 0)
      (else-clause . 0)
      (inclass . +)
      (label . 0)
      (statement . 0)
      (statement-block-intro . +)
      (statement-case-intro . +)
      (statement-cont . +)
      (substatement . +)
      (topmost-intro . 0))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(semantic-unmatched-syntax-face ((t nil)))
 '(swiper-match-face-2 ((t (:background "dim gray")))))
(put 'narrow-to-page 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
