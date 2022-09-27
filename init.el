  ;; -*- lexical-binding: t; -*-
  ;; The default is 800 kilobytes.  Measured in bytes.
  (setq gc-cons-threshold (* 50 1000 1000))

  ;; Profile emacs startup
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "*** Emacs loaded in %s seconds with %d garbage collections."
                       (emacs-init-time "%.2f")
                       gcs-done)))

(setq inhibit-startup-message t)

(setq package-native-compile t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)
(setq visible-bell nil)

  ;;; Line number config
(column-number-mode)
(global-display-line-numbers-mode t)
(menu-bar--display-line-numbers-mode-relative)

(dolist (mode '(term-mode-hook
                org-mode-hook
                helpful-mode-hook
                ibuffer-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq user-emacs-directory (expand-file-name "~/.emacs.d/.cache/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name ".cache/eln-cache/" user-emacs-directory))

  (defvar arko/default-font-size 95)

  (set-face-attribute 'default nil :font "JetBrains Mono" :height arko/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 93 :weight 'medium)
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 130 :weight 'regular)

  (custom-set-variables '(font-lock-support-mode 'tree-sitter-lock-mode))

  (defun arko/org-font-setup ()
    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "JetBrains Mono" :weight 'regular :height (cdr face)))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

  (use-package no-littering)

  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (when (file-directory-p "~/Exploits")
    (setq projectile-project-search-path '("~/Exploits")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package evil-magit
;;   :after magit)

(use-package evil-nerd-commenter
    :bind ("C-/". evilnc-comment-or-uncomment-lines))

(use-package phi-autopair
  :config
  (phi-autopair-global-mode))

;; (use-package forge)

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package emojify
    :hook (after-init . global-emojify-mode))

(use-package tree-sitter)
(use-package tree-sitter-langs)

(global-tree-sitter-mode)

  (use-package counsel
    :bind (("M-x" . counsel-M-x)
           ("C-x b" . counsel-ibuffer)
           ("C-x C-f" . counsel-find-file)
           :map minibuffer-local-map
           ("C-r" . 'counsel-minibuffer-history))
    :config
    (setq ivy-initial-inputs-alist nil)
    (setq counsel-describe-variable-function #'helpful-variable)
    (setq counsel-descbinds-function #'helpful-funciton))

(use-package ivy
    :diminish
    :bind (("C-s" . swiper)
        :map ivy-minibuffer-map
        ("RET" . ivy-alt-done)
        ("C-l" . ivy-alt-done)
        ("TAB" . ivy-next-line)
        ("C-j" . ivy-next-line)
        ("<backtab>" . ivy-previous-line)
        ("C-k" . ivy-previous-line)
        :map ivy-switch-buffer-map
        ("C-k" . ivy-previous-line)
        ("C-l" . ivy-done)
        ("C-d" . ivy-switch-buffer-kill)
        :map ivy-reverse-i-search-map
        ("C-k" . ivy-previous-line)
        ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))

(use-package ivy-rich
    :init
    (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
    :ensure t
    :init (all-the-icons-ivy-rich-mode 1))

;; Whether display the icons
(setq all-the-icons-ivy-rich-icon t)

;; Whether display the colorful icons.
;; It respects `all-the-icons-color-icons'.
(setq all-the-icons-ivy-rich-color-icon t)

;; The icon size
(setq all-the-icons-ivy-rich-icon-size 1.0)

;; Whether support project root
(setq all-the-icons-ivy-rich-project t)

;; Maximum truncation width of annotation fields.
;; This value is adjusted depending on the `window-width'.
(setq all-the-icons-ivy-rich-field-width 80)

;; Definitions for ivy-rich transformers.
;; See `ivy-rich-display-transformers-list' for details."
;; all-the-icons-ivy-rich-display-transformers-list

;; Slow Rendering
;; If you experience a slow down in performance when rendering multiple icons simultaneously,
;; you can try setting the following variable
(setq inhibit-compacting-font-caches t)

(use-package ivy-posframe
    :config
    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))
            ivy-posframe-min-width 130
            ivy-posframe-border-width 2
            ivy-posframe-max-height 30
            ivy-posframe-height 10
            ivy-posframe-max-width 220)
    (ivy-posframe-mode 1))

    (use-package treemacs
	:init
	:config
	(setq treemacs-deferred-git-apply-delay        0.5
	    treemacs-directory-name-transformer      #'identity
	    treemacs-display-in-side-window          t
	    treemacs-eldoc-display                   'simple
	    treemacs-file-event-delay                5000
	    treemacs-file-follow-delay               0.2
	    treemacs-file-name-transformer           #'identity
	    treemacs-follow-after-init               t
	    treemacs-expand-after-init               t
	    treemacs-is-never-other-window           nil
	    treemacs-missing-project-action          'remove
	    treemacs-move-forward-on-expand          nil
	    treemacs-position                        'left
	    treemacs-recenter-after-project-jump     'always
	    treemacs-recenter-after-project-expand   'on-distance
	    treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
	    treemacs-show-cursor                     nil
	    treemacs-sorting                         'alphabetic-asc
	    treemacs-select-when-already-in-treemacs 'move-back
	    treemacs-space-between-root-nodes        t
	    treemacs-tag-follow-cleanup              t
	    treemacs-tag-follow-delay                0.5
	    treemacs-wide-toggle-width               70
	    treemacs-width                           35
	    treemacs-width-increment                 1
	    treemacs-workspace-switch-cleanup        nil)
	(treemacs-load-theme "all-the-icons"))


(use-package treemacs-all-the-icons)
(require 'treemacs-all-the-icons)

(use-package all-the-icons
    :ensure t)

(use-package doom-themes
    :init (load-theme 'doom-one t))

;; (use-package modus-themes)

(use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)
    :custom
    (doom-modeline-height 28))

    (use-package rainbow-delimiters
      :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
    :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-funciton #'helpful-variable)
    :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-key] . helpful-key))

  (use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0.4))

;; Emacs mode for following modes
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-i-jump nil)

  :config
  (evil-mode 1)
  (setq evil-redo-function 'undo-redo)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "L" 'next-buffer)
  (evil-global-set-key 'motion "H" 'previous-buffer)
  (evil-global-set-key 'motion "E" 'evil-end-of-visual-line)
  (evil-global-set-key 'motion ";" 'counsel-M-x)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package general)

(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

(general-evil-setup)

(general-define-key
 :states 'normal
 "C-k" 'evil-scroll-line-up
 "C-j" 'evil-scroll-line-down)

(general-nmap
  :prefix "SPC"
  "tt" '(counsel-load-theme :which-key "choose theme")

  "bk" '(kill-this-buffer :which-key "Kill Buffer")
  "bf" '(counsel-switch-buffer :which-key "Switch Buffer")

  "p" '(projectile-command-map :which-key "Project")

  ;;; Keymaps for opening stuff
  "ot" '(vterm :which-key "Open Term")
  "oi" '(counsel-imenu :which-key "IMenu")
  "ob" '(eww :which-key "eww")

  ;; Code related keymaps
  "ce" '(eval-last-sexp :which-key "Eval last sexp")

  "w" '(evil-window-map :which-key "Window")

  "hf" '(counsel-describe-function :which-key "Describe Function")
  "hv" '(counsel-describe-variable :which-key "Describe Variable")

  "qq"'(save-buffers-kill-terminal :which-key "Exit Emacs")

  "fs" '(save-buffer :which-key "Save Buffer")
  "fo" '(counsel-find-file :which-key "Open File")
  "fr" '(counsel-recentf :which-key "Open File")

  "e" '(treemacs :which-key "Treemacs Toggle"))

;;; Emms Keymaps
(general-nmap
  :prefix "SPC m" :which-key "Emms"
  "n" '(emms-next :which-key "Next")
  "s" '(emms-stop :which-key "Next")
  "j" '(emms-toggle-random-playlist :which-key "Sufftle")
  "d" '(emms-play-directory :which-key "Play the dir")
  "p" '(emms-play-directory :which-key "Play the dir")
  "p" '(emms-previous :which-key "Previous")
  "rt" '(emms-toggle-repeat-track :which-key "Repeat Track")
  "rp" '(emms-toggle-repeat-playlist :which-key "Repeat Playlist")
  "SPC" '(emms-pause :which-key "Play/Pause"))

(when org-mode-hook
  (general-nmap
    :prefix "SPC oo"
    "v" '(org-display-inline-images :which-key "Display inline Images")))


;; (general-create-definer arko/leader-keys
;;   :keymaps '(normal insert visual emacs)
;;   :prefix "SPC"
;;   :global-prefix "M-SPC")

;; (arko/leader-keys
;;   "tt" '(counsel-load-theme :which-key "choose theme")
;;   "bk" '(kill-this-buffer :which-key "Kill Buffer")
;;   "bf" '(counsel-switch-buffer :which-key "Switch Buffer")
;;   "p" '(projectile-command-map :which-key "Project")
;;   "ot" '(vterm :which-key "Open Term")
;;   "ce" '(eval-last-sexp :which-key "Eval last sexp")
;;   "w" '(evil-window-map :which-key "Window")
;;   "hf" '(counsel-describe-function :which-key "Describe Function")
;;   "hv" '(counsel-describe-variable :which-key "Describe Variable")
;;   "qq"'(save-buffers-kill-terminal :which-key "Exit Emacs")
;;   "fs" '(save-buffer :which-key "Save Buffer")
;;   "fo" '(counsel-find-file :which-key "Open File")
;;   "fc" '(coun :which-key "Open Term")
;;   "e" '(treemacs :which-key "Treemacs Toggle"))



  (use-package hydra)

  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("f" nil "finished" :exit t))

  (general-nmap
    :prefix "SPC"
    "ts" '(hydra-text-scale/body :which-key "scale text"))
  ;;

(defun arko/lsp-mode-setup ()
(setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)))

(use-package lsp-mode
    :hook (lsp-mode . arko/lsp-mode-setup)
    :commands (lsp lsp-deferred)
    :init
    (setq lsp-keymap-prefix "C-l")
    :config
    (lsp-enable-which-key-integration t))

(use-package lsp-ui
    :after lsp-mode
    :config
    (lsp-ui-mode)
    (lsp-ui-doc-enable t)
    (setq lsp-ui-doc-delay 0.4
        lsp-ui-doc-position 'top
        lsp-ui-doc-max-height 12
        lsp-ui-doc-max-width 90
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t))

(use-package lsp-ivy)

(use-package lsp-treemacs
    :after lsp)

  (use-package company
      :bind
      (:map company-active-map
          ("<tab>" . company-complete-common-or-cycle)
          ("S-<tab>" . company-select-previous))
      (:map lsp-mode-map
          ("<tab>" . company-indent-or-complete-common)
          ("S-<tab>" . company-select-previous))
      :custom
      (company-minimum-prefix-length 1)
      (company-idle-delay 0.0))

  (global-company-mode)

  (use-package company-box
      :hook (company-mode . company-box-mode))

(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package ripgrep)

(use-package yasnippet)
(yas-global-mode 1)

(use-package rust-mode)

(add-hook 'rust-mode-hook
        (lambda () (setq indent-tabs-mode nil)))
(setq rust-format-on-save t)

(setq lsp-clangd-binary-path "/bin/clangd")

(use-package python-mode
  :ensure t)

(use-package jupyter
  :ensure t)
;; (use-package ein)
;; (setq ein:output-area-inlined-images t
;;     ob-ein-inline-images-directory "~/.emacs.d/ob-ein-images")

(use-package highlight-defined)
(use-package lispy)

(use-package slime)
(setq inferior-lisp-program "sbcl")

(use-package lua-mode)

(use-package nim-mode
    :ensure t
    :hook (nim-mode . lsp))

(use-package emmet-mode)

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-to-list 'emmet-jsx-major-modes 'your-jsx-major-mode)

 (use-package vterm)

(use-package emms
    :config
    (emms-all)
    (emms-default-players)
    (setq emms-source-file-default-directory "~/Music"
            emms-info-functions '(emms-info-tinytag)
            emms-playlist-buffer-name "Music"
            emms-mode-line-icon-color "#2c2fe9"
            emms-mode-line-icon-enabled-p nil
            emms-volume-amixer-card 1
            emms-mode-line-format "🎶 "))

(require 'emms-player-simple)
(require 'emms-source-file)
(require 'emms-source-playlist)
(setq emms-player-list '(emms-player-mpg321
                        emms-player-ogg123
                        emms-player-mpv
                        emms-player-mplayer
                        ))


(defun track-title-from-file-name (file)
    (with-temp-buffer
    (save-excursion (insert (file-name-nondirectory (directory-file-name file))))
    (ignore-error 'search-failed
        (search-forward-regexp (rx "." (+ alnum) eol))
        (delete-region (match-beginning 0) (match-end 0)))
    (buffer-string)))

(defun my-emms-track-description (track)
    (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title)))
    (cond ((and artist title)
            (concat artist " - " title))
            (title title)
            ((eq (emms-track-type track) 'file)
            (track-title-from-file-name (emms-track-name track)))
            (t (emms-track-simple-description track)))))

(setq emms-track-description-function 'my-emms-track-description)


(cond
 ;; test to see if D-Bus notifications are available
 ((if (and (require 'dbus nil t)
	   (dbus-ping :session "org.freedesktop.Notifications"))
      (progn
	(setq notify-method 'notify-via-dbus-notifications)
	(require 'notifications))))
 ;; could use the message system otherwise
 (t (setq notify-method 'notify-via-message)))

(defun notify-via-notifications (title msg icon)
  "Send notification with TITLE, MSG via `D-Bus'."
  (notifications-notify
   :title title
   :body msg
   :app-icon icon
   :urgency 'low))

(defun notify-via-messages (title msg)
  "Send notification with TITLE, MSG to message."
  (message "APPOINTMENT: %s" msg))

(defun emms-notifications-dbus (track-name)
  "Share track name via `D-Bus'."
  (let ((icon "/usr/share/icons/ePapirus-Dark/48x48/apps/multimedia.svg"))
    (notify-via-notifications "EMMS is now playing:" track-name icon)))

(defun emms-notifications-message (track-name)
  "Share track name via Emacs minibuffer."
  (message "EMMS is now playing: %s" track-name))

(setq emms-player-next-function 'emms-notify-and-next)

(defun emms-notify-and-next ()
  "Send a notification of track and start next."
  (emms-next-noerror)
  (let ((track-name (emms-track-description (emms-playlist-current-selected-track))))
    (cond
     ((eq notify-method 'notify-via-dbus-notifications)
      (emms-notifications-dbus track-name))
     (t (emms-notifications-message track-name)))))

(with-eval-after-load 'eww
  (setq-local endless/display-images t)
  (defun endless/toggle-image-display ()
    "Toggle images display on current buffer."
    (interactive)
    (setq endless/display-images
          (null endless/display-images))
    (endless/backup-display-property endless/display-images))

  (defun endless/backup-display-property (invert &optional object)
    "Move the 'display property at POS to 'display-backup.
     Only applies if display property is an image.
     If INVERT is non-nil, move from 'display-backup to 'display instead.
     Optional OBJECT specifies the string or buffer. Nil means current
     buffer."

    (let* ((inhibit-read-only t)
           (from (if invert 'display-backup 'display))
           (to (if invert 'display 'display-backup))
           (pos (point-min))
           left prop)
      (while (and pos (/= pos (point-max)))
        (if (get-text-property pos from object)
            (setq left pos)
          (setq left (next-single-property-change pos from object)))
        (if (or (null left) (= left (point-max)))
            (setq pos nil)
          (setq prop (get-text-property left from object))
          (setq pos (or (next-single-property-change left from object)
                        (point-max)))
          (when (eq (car prop) 'image)
            (add-text-properties left pos (list from nil to prop) object))))))


  (defun my/eww-toggle-images ()
    "Toggle whether images are loaded and reload the current page fro cache."
    (interactive)
    (setq-local shr-inhibit-images (not shr-inhibit-images))
    (eww-reload t)
    (message "Images are now %s"
             (if shr-inhibit-images "off" "on")))

  (define-key eww-mode-map (kbd "I") #'my/eww-toggle-images)
  (define-key eww-link-keymap (kbd "I") #'my/eww-toggle-images)

  ;; minimal rendering by default
  (setq-default shr-inhibit-images t)   ; toggle with `I`
  (setq-default shr-use-fonts nil))

  (defun arko/org-mode-setup ()
    (setq org-src-tab-acts-natively t
          org-src-tab-acts-natively     t
          org-src-preserve-indentation  t
          org-src-fontify-natively      t)
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1))

  (use-package org
    :hook (org-mode . arko/org-mode-setup)
    :config
    (setq org-ellipsis "👇"
          org-hide-emphasis-markers t
          org-agenda-files '("~/Exploits/docs/Tasks.org")
          org-agenda-start-with-log-mode t
          org-log-done 'time
          org-log-into-drawer t)

    (arko/org-font-setup))

  (defun arko/org-mode-visual-fill()
    (setq visual-fill-column-width 120
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package visual-fill-column
    :hook (org-mode . arko/org-mode-visual-fill))

  (use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("🌩" "🚀" "✿" "✸" "●" "◉")))

(org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
        (python . t)
        (jupyter . t)
        (julia . t)
        (lua . t)))
(setq org-startup-with-inline-images t)

(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                              (:session . "python")))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("jb" . "src jupyter"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("jl" . "src julia"))

(setq org-confirm-babel-evaluate nil)

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(use-package org-drill)

(defun arko/org-babel-tangle-config ()
  (interactive)
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'arko/org-babel-tangle-config)))
