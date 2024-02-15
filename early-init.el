;; -*- lexical-binding: t; -*-

(setq user-emacs-directory (expand-file-name "~/.emacs.d/.cache/")
      native-comp-async-report-warnings-errors nil
      reac-process-output-max (* 1024 1024)
      gc-cons-threshold (* 400 1024 1024)
      package-enable-at-startup nil
      native-compile-prune-cache t
      inhibit-startup-message t
      package-native-compile t
      auto-save-default nil
      make-backup-files nil
      create-lockfiles nil
      visible-bell nil)

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
