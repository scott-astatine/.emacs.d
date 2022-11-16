;; -*- lexical-binding: t; -*-

(setq inhibit-startup-message t)
(setq native-comp-async-report-warnings-errors nil)
(setq package-native-compile t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)
(setq visible-bell nil)

(column-number-mode)
(global-display-line-numbers-mode t)
(menu-bar--display-line-numbers-mode-relative)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
