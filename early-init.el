;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold (* 50 1024 1024)
      inhibit-startup-message t
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
;; (hs-minor-mode t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))
