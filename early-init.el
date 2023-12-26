;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold (* 500 1024 1024)
      reac-process-output-max (* 1024 1024)
      inhibit-startup-message t
      native-compile-prune-cache t
      native-comp-async-report-warnings-errors nil
      visible-bell nil
      user-emacs-directory (expand-file-name "~/.emacs.d/.cache/")
      package-native-compile t
      package-enable-at-startup nil)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;;; Common hooks 
(dolist (mode '(term-mode-hook
                helpful-mode-hook
                help-mode-hook
                Man-mode-hook
                Info-mode-hook
                vterm-mode-hook
                dashboard-mode-hook
                messages-buffer-mode-hook
                ielm-mode-hook
                dictionary-mode-hook
                image-mode-hook
                lsp-help-mode-hook
                symbols-outline-mode-hook
                nov-mode-hook
                TeX-special-mode-hook
                ibuffer-mode-hook
                pdf-outline-buffer-mode-hook
                shell-mode-hook
                inferior-ess-r-mode-hook
		sage-shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda ()
                   (progn
                     (setq word-wrap t)
                     (display-line-numbers-mode 0)
                     (hide-mode-line-mode 1)
                     ))))

;;; Major mode specific
;; Visual Fill Modes
(dolist (mode '(help-mode-hook
                Man-mode-hook
                Info-mode-hook
		helpful-mode-hook))
  (add-hook mode (lambda ()
                   (progn
                     (visual-fill-column-mode 1)
                     ))))
(add-hook 'Info-mode-hook
	  (lambda ()
	    (setq-local visual-fill-column-width 90)))


;;; prog-mode hooks
(add-hook 'prog-mode-hook (lambda ()
			    (progn
			      (setq word-wrap t)
			      )))
