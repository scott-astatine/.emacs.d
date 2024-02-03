;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 200 1024 1024)
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

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path "/home/scott/.emacs.d/lisp")
(require 'hide-mode-line)
