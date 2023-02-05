;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold (* 50 1024 1024)
      inhibit-startup-message t
      native-compile-prune-cache t
      native-comp-async-report-warnings-errors nil
      visible-bell nil
      user-emacs-directory (expand-file-name "~/.emacs.d/.cache/")
      package-native-compile t
      package-enable-at-startup nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode t)
(menu-bar--display-line-numbers-mode-relative)
;; Set up the visible bell
(setq visible-bell t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(dolist (mode '(term-mode-hook
                helpful-mode-hook
                vterm-mode-hook
                dashboard-mode-hook
                messages-buffer-mode-hook
                ielm-mode-hook
                dictonary-mode-hook
                image-mode-hook
                ibuffer-mode-hook
                pdf-outline-buffer-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda ()
                   (progn
                     (display-line-numbers-mode 0)
                     (hide-mode-line-mode 1)
                     (undo-tree-mode 0)))))

;;; Pdf Mode hooks
(dolist (mode '(doc-view-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda ()
                   (progn
                     (display-line-numbers-mode 0)
                     (undo-tree-mode 0)))))
