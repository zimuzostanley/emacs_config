;;; Using Emacs on Android
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://android-factory.corp.google.com/docs/development-environment/using-emacs.html
;;(require 'google-jdb)

(setq-default python-indent 2)

(setq c-electric-flag nil)

(c-add-style "android-java" '("java" 
			      (c-basic-offset . 4)
			      (c-offsets-alist . ((case-label            . +)
						  (statement-case-intro  . +)
						  (statement-cont        . ++)
						  (topmost-intro-cont    . ++)
						  ;;(arglist-cont-nonempty . ++)
						  (func-decl-cont        . ++)))))
(c-add-style "android-c"    '("gnu"  
			      (c-basic-offset . 4)
			      (c-offsets-alist . ((case-label            . +)
						  (statement-case-intro  . +)
						  (statement-cont        . ++)
						  (topmost-intro-cont    . ++)
						  ;;(arglist-cont-nonempty . ++)
						  (func-decl-cont        . ++)))))

(setq c-default-style '((java-mode . "android-java")
			(awk-mode  . "awk")
			(other     . "android-c")))

(defun android-c-mode-common-hook ()
  (setq indent-tabs-mode nil)
  (setq dabbrev-case-replace nil)
  (setq show-trailing-whitespace 'trailing-whitespace)
  (add-hook 'write-contents-functions 'android-write-contents-function))

(add-hook 'c-mode-common-hook 'android-c-mode-common-hook)

(defvar android-contents-checks nil)
(setq android-contents-checks '())
(setq android-contents-checks
      '(
        ("\t" 
         "tab")
        (" +$" 
         "trailing whitespace")
        ;; ("^ \\{1,3\\}\\(    \\)*\\(}\\|if\\|while\\|for\\|repeat\\|break\\|continue\\|public \\|private \\|protected \\|static \\|native \\|//\\|/\\*\\)" 
        ;;  "non-multple of 4 indent")
        ("^ \\{1,3\\}\\(    \\)*  \\(case\\|default\\)  " 
         "case labels need half-indent")
        ("^ +else" 
         "need } before else")
        ;;("^ +catch" 
	;; "need } before catch")
        ;; ("\\([-+%^]\\|[^*][/]\\|[+][+]|--\\|&&\\[|][|]\\)$" 
	;;  "operator at EOL")
        ("^.\\{101\\}" 
         "line longer than 100 characters")
        ))

(defun android-contents-check ()
  (let ((bof (point-min))
        (eof (point-max))
        (errors nil))
    (mapc (lambda (check)
            (save-excursion
              (goto-char bof)
              (let ((check-regexp  (car  check))
                    (check-message (cadr check)))
                (while (not (= (point) eof))
                  (let ((found (re-search-forward check-regexp eof 'limit)))
                    (cond (found
                           (setq errors (cons (list check-message (point-marker))
                                              errors)))))))))
          android-contents-checks)
    (reverse errors)))

;; called on save to check formatting
(defun android-write-contents-function ()
  (let ((errors (android-contents-check))
        (skip-save nil))
    (cond (errors
           (let* ((result  (car errors))
                  (message (car result))
                  (marker  (cadr result)))
             (save-excursion
               (goto-char (marker-position marker))
               (cond ((not (y-or-n-p (format "%s on line %d. Save anyway "
                                             message 
					     (line-number-at-pos (marker-position marker)))))
                      (setq skip-save marker)))))))
    (cond (skip-save (goto-char (marker-position skip-save)))
	  (t (android-contents-check-interactive-reset)))
    skip-save))

(defvar android-contents-check-interactive-error 0)
(defvar android-contents-check-interactive-errors nil)

(defun android-contents-check-interactive-reset ()
  (setq android-contents-check-interactive-error  0)
  (setq android-contents-check-interactive-errors nil))

(defun android-contents-check-interactive ()
  (interactive)
  ;; if we don't have errors or they are for a different buffer, regenerate errors
  (cond ((not (and android-contents-check-interactive-errors
		   (eq (marker-buffer (car (cdr (car android-contents-check-interactive-errors))))
		       (current-buffer))))
	 (setq android-contents-check-interactive-error  0)
	 (setq android-contents-check-interactive-errors (android-contents-check))))
  ;; if we have some errors, lets look at them
  (cond (android-contents-check-interactive-errors
	 (let* ((result  (elt android-contents-check-interactive-errors
			      android-contents-check-interactive-error))
		(message (car result))
		(marker  (cadr result)))
	   (goto-char (marker-position marker))
	   (message message)
	   (setq android-contents-check-interactive-error
		 (+ android-contents-check-interactive-error 1))
	   ;; if we are at the end, reset to force a regeneration
	   (cond ((= android-contents-check-interactive-error
		     (length android-contents-check-interactive-errors))
		  (android-contents-check-interactive-reset)))))
	(t
	 (message "Looking good!"))))

(global-set-key '[f9] 'android-contents-check-interactive)

(defun android-logcat ()
  (interactive)
  (shell-command "adb logcat&" "*logcat"))
