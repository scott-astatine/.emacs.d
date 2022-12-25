(setq scroll-step 1
      scroll-margin 2
      pixel-scroll-precision-large-scroll-height 40.0
      url-history-file (expand-file-name "url/history" user-emacs-directory))


(dolist (mode '(term-mode-hook
                helpful-mode-hook
                vterm-mode-hook
                ielm-mode-hook
                ibuffer-mode-hook
                doc-view-mode-hook
                pdf-outline-buffer-hook
                pdf-view-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda ()
                   (progn
                     (display-line-numbers-mode 0)
                     (undo-tree-mode 0)))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-hl-line-mode t)
(set-face-background 'hl-line "#4f4f5f55")

;;(hs-minor-mode)

(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(defvar spacedefault-font-size 105)
(defvar spacedefault-code-font "JetBrains Mono")

(set-face-attribute 'default nil :font spacedefault-code-font :height spacedefault-font-size)
(set-face-attribute 'fixed-pitch nil :font spacedefault-code-font :height spacedefault-font-size :weight 'regular)

(set-face-attribute 'variable-pitch nil :font "Leckerli One" :height 155 :weight 'regular)

(variable-pitch-mode t)

(custom-set-variables '(font-lock-support-mode 'tree-sitter-lock-mode))

(defun spaceorg-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.14)
                  (org-level-3 . 1.07)
                  (org-level-4 . 1.04)
                  (org-level-5 . 1.02)
                  (org-level-6 . 1.02)
                  (org-level-7 . 1.02)
                  (org-level-8 . 1.02)))
    (set-face-attribute (car face) nil :font spacedefault-code-font :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(set-frame-parameter nil 'alpha '(100 . 100))
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(80 . 50) '(100 . 100)))))
(toggle-transparency)

(defun set-window-height (height)
  "Set the height of the current window to the specified HEIGHT."
  (interactive "nWindow height: ")
  (if (> height (window-total-height))
      (enlarge-window (- height (window-total-height)))
    (shrink-window (- (window-total-height) height))))

(defun split-window-below-with-height (height)
  "Split the current window horizontally and switch to the new window.
   The new window will be given the specified HEIGHT."
  (interactive "nWindow height: ")
  (split-window-below)
  (windmove-down)
  (set-window-height height))

(defun set-window-width (width)
  "Set the width of the current window to WIDTH."
  (interactive "nNew window width: ")
  (let ((window (get-buffer-window (current-buffer))))
    (when window
          (enlarge-window-horizontally width))))

(defun split-repl ()
  (interactive)
  (split-window-below-with-height 15)
  (ielm)
  (setq splitwin (selected-window))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (eq splitwin (selected-window))
                (delete-window (selected-window)))))
  )

(defun split-vterm (height)
  "Split vterm"
  (interactive "nWindow height: ")
  (split-window-below-with-height height)
  (multi-vterm)
  (setq splitwin (selected-window))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (eq splitwin (selected-window))
                (delete-window splitwin)))))
;;(general-def 'normal 'vterm-mode-map
;;  "C-c" 'vterm--self-insert
;;  "C-d" 'kill-this-buffer)

(defun split-h-vterm-window ()
  (interactive)
  (split-vterm 10))

(defvar project-run-cmds
  '((cargo . "cargo run")
    (npm . "npm start")
    (python . "python main.py")
    (pnpm . "pnpm run")
    (make . "make")))

;; Define a map that contains the package manager/build system config file names
(defvar project-config-files
  '((cargo . "Cargo.toml")
    (npm . "package.json")
    (python . "main.py")
    (pnpm . "pnpm-lock.json")
    (make . "Makefile")))

(defun run-current-project ()
  "Run the current project"
  (interactive)
  (let ((project-root default-directory)
        (runconf-file (locate-dominating-file default-directory ".runconf"))
        (run-command nil))
    (if runconf-file
        (with-temp-buffer
          (insert-file-contents (concat runconf-file ".runconf"))
          (setq run-command (buffer-string))))

    (if (not run-command)
        (let ((config-file nil))
          (dolist (file-map project-config-files)
            (progn
              (setq config-file-name (cdr file-map))
              (when (locate-dominating-file project-root config-file-name)
                (setq run-command (cdr (assq (car (rassoc config-file-name project-config-files)) project-run-cmds)))
                (write-region run-command nil (concat project-root ".runconf")))))))
    
    (if (and (not run-command) (not runconf-file))
        (write-region "" nil (concat project-root ".runconf")))
    
    (when run-command
      (set-frame-name "project-runner")
      (split-h-vterm-window)
      (general-define-key
       :keymaps 'local
       "C-c" 'vterm--self-insert
       "q" '(lambda () (interactive) (kill-this-buffer)))

      (vterm-send-string (concat "cd " project-root " && " run-command "\n"))
      (windmove-up))))



(defun insert-current-date ()
  (interactive)
  (insert (format-time-string "%b %d, %a")))

(defun insert-current-time ()
  (interactive)
  (insert (format-time-string "%H:%M")))

(defun org-wrap= ()
  (interactive)
  (if (use-region-p)
      (progn
        (kill-region (region-beginning) (region-end))
        (insert "=")
        (yank)
        (insert "="))
    (message "No region selected")))

;; Initialize package sources
;; (require 'package)
;; (eval-and-compile
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; ;; Initialize use-package on non-Linux platforms
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t))

(setq straight-repository-branch "develop"
      straight-enable-use-package-integration t
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-use-package-by-default t
      straight-cache-autoloads t
      straight-host-usernames '((github . "scott-astatine")
                                (gitlab . "scott-astatine")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  ;; (benchmark 1 `(load ,bootstrap-file nil 'nomessage))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-defer t))

;; demote installation errors to messages
;; this variable is no longer changed by straight
;; (advice-add use-package-ensure-function :around #'noct-use-package-ensure)
(when (bound-and-true-p noct-with-demoted-errors)
  (advice-add 'straight-use-package :around #'noct-inhibit-error-advice))
;; can test with something like this:
;; (use-package does-not-exist)

(use-package blackout
  :straight (blackout :host github :repo "raxod502/blackout")
  :demand t)

(use-package no-littering
  :ensure t)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.4))

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-C-d-scroll t
        evil-want-C-i-jump nil
        evil-move-cursor-back nil
        evil-move-beyond-eol t
        evil-ex-visual-char-range t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :demand t
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-escape
  :demand t
  :config
  (setq evil-escape-key-sequence "kj"
        evil-escape-delay 0.15)
  (evil-escape-mode 1))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package general
  :init
  (general-evil-setup)
  :demand t)
(general-def 'normal 'override
  "L" 'next-buffer
  "H" 'previous-buffer
  "E" 'evil-end-of-visual-line
  ";" 'counsel-M-x)
(general-def 'normal
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(general-def '(normal visual)
  "SPC" nil
  "m" nil)

(general-define-key
  :keymaps 'treemacs-mode-map
  "a" 'windmove-right
  "K" 'evil-scroll-line-up
  "J" 'evil-scroll-line-down
  "C-k" 'evil-scroll-line-up
  "C-j" 'evil-scroll-line-down)


(general-create-definer spaceleader-keys
  :keymaps '(override treemacs-mode)
  :states '(normal visual)
  :prefix "SPC")

(general-create-definer general-m
  :states 'normal
  :prefix "m")

(general-def 'normal 'override
 ;; "u" 'undo-tree-undo
 "K" 'lsp-describe-thing-at-point
 "g/" 'evilnc-comment-or-uncomment-lines
 "C-k" 'evil-scroll-line-up
 "C-j" 'evil-scroll-line-down)

(general-def 'insert
 "C-g" 'evil-normal-state
 "C-h" 'evil-delete-backward-char-and-join)

(defun mjort ()
  (interactive)
  (funcall major-mode))
(general-m
  "t" '(mjort :which-key "Toogle Major Mode"))
(spaceleader-keys
  "SPC" '(projectile-find-file :which-key "Find file in project")

  "w" '(evil-window-map :which-key "Window")
  "ww" '(set-window-width :which-key "Set Width")
  "wi" '(set-window-height :which-key "Set Height")
  "a" '(ace-select-window :which-key "Select Window")
  "qq"'(save-buffers-kill-terminal :which-key "Exit Emacs")
  "d"'(kill-this-buffer :which-key "Exit Emacs")
  "s"'(swiper :which-key "Exit Emacs")

  "e" '(treemacs-select-window :which-key "Treemacs Toggle"))

(spaceleader-keys
  :prefix "SPC t"
  "t" '(counsel-load-theme :which-key "choose theme")
  "s" '(hydra-text-scale/body :which-key "scale text")
  "w" '(toggle-transparency :which-key "scale text")
  "l" '(display-line-numbers-mode :which-key "Toogle line numbers")
  "b" '(display-battery-mode :which-key "Toogle Battery")
  "v" '(visual-fill-column-mode :which-key "Center Column")
  "d" '(elcord-mode :which-key "Discord status")
  "m" '(mjort :which-key "Toogle Major Mode"))

(spaceleader-keys
  :prefix "SPC f"
  "s" '(save-buffer :which-key "Save Buffer")
  "o" '(counsel-find-file :which-key "Open File")
  "f" '(projectile-find-file :which-key "Find file in project")
  "r" '(counsel-recentf :which-key "Open File"))

(spaceleader-keys
  :prefix "SPC c"
  "e" '(eval-last-sexp :which-key "Eval last sexp"))

(defun inspc ()
  (interactive)
  (insert " "))

(spaceleader-keys
  :prefix "SPC i"
  "d" '(insert-current-date :which-key "Insert Date")
  "SPC" '(inspc :which-key "Insert Date")
  "t" '(insert-current-time :which-key "Insert Time")
  "e" '(emoji-insert :which-key "Insert Emoji"))

(spaceleader-keys
  :prefix "SPC h"
  "f" '(counsel-describe-function :which-key "Describe Function")
  "v" '(counsel-describe-variable :which-key "Describe Variable"))

(spaceleader-keys
  :prefix "SPC o"
  "T" '(multi-vterm :which-key "Open Term")
  "t" '(split-h-vterm-window :which-key "Open Term")
  "i" '(counsel-imenu :which-key "IMenu")
  "j" '((lambda () (interactive) (find-file "/home/scott/Books/Personal/Journal.org")) :which-key "Open Journal")
  "r" '(split-repl :which-key "Elisp REPL")
  "b" '(eww :which-key "eww")
  "e" '(eshell :which-key "Eshell"))

(spaceleader-keys
  :prefix "SPC b"
  "l" '(evil-switch-to-windows-last-buffer :which-key "Kill Buffer")
  "k" '(kill-this-buffer :which-key "Kill Buffer")
  "f" '(counsel-switch-buffer :which-key "Switch Buffer")
  "d" '(kill-buffer :which-key "Find & Kill"))


(general-def 'normal emacs-lisp-mode-map 
  "K" 'elisp-slime-nav-describe-elisp-thing-at-point)

;; Appending projectile keymaps
(spaceleader-keys
  :prefix "SPC p"
  "r" '(run-current-project :which-key "Run Project")
  "e" '(treemacs-projectile :which-key "Treemacs Projectile")
  "o" '(counsel-projectile-switch-project :which-key "Open Project")
  "d" '(projectile-remove-known-project :which-key "Add Project")
  "a" '(projectile-add-known-project :which-key "Add Project"))

(use-package async)

(use-package all-the-icons
  :demand t)

(use-package projectile
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired)

  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package evil-magit
;;   :after magit)

(use-package evil-nerd-commenter
  :ensure t)

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :init
  (global-ligature-mode t))
(with-eval-after-load 'ligarure-mode
                      (ligature-set-ligatures t '("www" "..."))
                      (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
                      (ligature-set-ligatures
                       'prog-mode
                       '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
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
                         "?=" "?." "??"  ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                         "\\\\" "://")))

(use-package emojify
    :hook (after-init . global-emojify-mode))

(use-package elcord
  :config
  (elcord-mode t)
  (setq elcord-refresh-rate 5))

(use-package undo-tree
  :init
  (global-undo-tree-mode))

(use-package smartparens
  :demand t
  :config
  (smartparens-global-mode))

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
  (setq ivy-initial-inputs-alist nil
        counsel-describe-variable-function #'helpful-variable
        counsel-descbinds-function #'helpful-funciton)
  :init
  (counsel-mode))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

(use-package ivy
    :diminish
    :bind (
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
  :init 
  (all-the-icons-ivy-rich-mode 1)
  :config
  (setq all-the-icons-ivy-rich-icon t
        all-the-icons-ivy-rich-color-icon t
        all-the-icons-ivy-rich-icon-size 1.0
        all-the-icons-ivy-rich-project t
        all-the-icons-ivy-rich-field-width 80
        inhibit-compacting-font-caches t))

;; Whether display the icons

(use-package ivy-posframe
  :demand t
  :init
  (ivy-posframe-mode 1)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-min-width 130
        ivy-posframe-border-width 3
        ivy-posframe-max-height 60
        ivy-posframe-height 10
        ivy-posframe-mode t
        ivy-posframe-max-width 220))

(use-package treemacs
  :demand t
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
        treemacs-is-never-other-window           t
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
        treemacs-workspace-switch-cleanup        nil))

(use-package treemacs-projectile)

(use-package treemacs-all-the-icons
  :demand t
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-evil
  :demand t)

(use-package doom-themes
  :demand
  :init (load-theme 'doom-monokai-spectrum t))

(use-package doom-modeline
  :demand t
  :init
  (setq doom-modeline-height 27
        doom-modeline-buffer-encoding nil)
  (doom-modeline-mode 1))
(doom-modeline-def-modeline 'main
  '(bar window-number modals
	matches buffer-info
	remote-host checker
	parrot selection-info)
  '(objed-state
    persp-name
    battery grip
    irc mu4e
    gnus github
    buffer-position debug
    misc-info lsp
    minor-modes input-method
    indent-info buffer-encoding
    major-mode process vcs " "))

(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

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

(use-package dashboard
  :demand t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-banner-logo-title "")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                     :height 1.1
                                                     :v-adjust -0.05
                                                     :face 'font-lock-keyword-face))
  :config
  (dashboard-setup-startup-hook))

(dashboard-modify-heading-icons '((recents . "file-text")
                                  (bookmarks . "book")))

(defun spacelsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)))

(use-package lsp-mode
  :hook (lsp-mode . spacelsp-mode-setup)
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  (setq lsp-enable-completion-at-point t
        lsp-diagnostic-package :flycheck)

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

(use-package dap-mode)

(use-package lsp-treemacs
    :after lsp)

(use-package company
  :config
  (setq ispell-dictonary "en_US"
	company-ispell-dictonary ispell-dictonary)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-common-or-cycle)
        ("<backtab>" . company-select-previous)
        ("C-j" . company-complete-common-or-cycle)
        ("C-p" . company-select-previous))
  :custom
  (company-minimum-prefix-length 1)
  (add-to-list 'company-backends 'company-ispell)
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

(use-package rust-mode
  :ensure t
  :hook ((rust-mode . flycheck-mode)
	 (rust-mode . lsp-deferred))
  :config
  (setq rust-format-on-save t))

(add-hook 'rust-mode-hook
        (lambda () (setq indent-tabs-mode nil)))

(setq lsp-clangd-binary-path "/bin/clangd")
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(with-eval-after-load 'lsp-mode
  (require 'dap-cpptools))

(use-package glsl-mode)

(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package qml-mode)

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred))

;; adds syntax highlighting for reST (and epydoc) docstrings and makes filling
;; work as expected.(for all multi-line strings)
(use-package python-docstring
  :ghook 'python-mode-hook
  :blackout t)

(use-package julia-mode)

(use-package ein)
(setq ein:output-area-inlined-images t
    ob-ein-inline-image-directory "~/.emacs.d/.cache/ob-ein-images")

;; (general-m
;;   :keymaps ein:ipdb-mode-map
;;   "d" '(ein:worksheet-delete-cell :which-key "Delete Cell"))

(use-package jupyter
  :commands (jupyter-run-repl jupyter-connect-repl)
  ;; :init
  ;; (add-hook 'python-mode-hook #'jupyter-python-mode-hook)
  :config
  (setq jupyter-server-buffer-name "*jupyter-server*"))

(use-package highlight-defined)
(use-package lispy)
(use-package elisp-slime-nav)

(use-package slime)
(setq inferior-lisp-program "sbcl")

(use-package dart-mode
 :config
 :hook (dart-mode . lsp))

(use-package lsp-dart)

(use-package lua-mode)

(use-package nim-mode
    :ensure t
    :hook (nim-mode . lsp))

(use-package web-mode
  :ensure t
  :gfhook #'lsp
  :mode (("\\.[tj]sx\\'" . web-mode)
	 ("\\.[tj]s\\'" . web-mode)
	 ("\\.html\\'" . web-mode)))

(use-package lsp-tailwindcss
  :straight (:type git :host github :repo "merrickluo/lsp-tailwindcss"))

(use-package emmet-mode)

(use-package auctex
  :ensure t)

(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-auctex))

(defun latex-comp ()
  (interactive)
  (when (eq major-mode 'latex-mode)
    (TeX-command-run-all nil)))

(add-hook 'LaTeX-mode-hook (lambda () (add-hook 'after-save-hook #'latex-comp)))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))
(use-package multi-vterm
  :ensure t)

;; (use-package emms
;;     :config
;;     (emms-all)
;;     (emms-default-players)
;;     (setq emms-source-file-default-directory "~/Music"
;;             emms-info-functions '(emms-info-tinytag)
;;             emms-playlist-buffer-name "Music"
;;             emms-mode-line-icon-color "#cc3fc9"
;;             emms-mode-line-icon-enabled-p nil
;;             emms-volume-amixer-card 1
;;             emms-mode-line-format "🎶 "))

;; (require 'emms-player-simple)
;; (require 'emms-source-file)
;; (require 'emms-source-playlist)
;; (setq emms-player-list '(emms-player-mpg321
;;                         emms-player-ogg123
;;                         emms-player-mpv
;;                         emms-player-mplayer
;;                         ))


;; (defun track-title-from-file-name (file)
;;     (with-temp-buffer
;;     (save-excursion (insert (file-name-nondirectory (directory-file-name file))))
;;     (ignore-error 'search-failed
;;         (search-forward-regexp (rx "." (+ alnum) eol))
;;         (delete-region (match-beginning 0) (match-end 0)))
;;     (buffer-string)))

;; (defun my-emms-track-description (track)
;;     (let ((artist (emms-track-get track 'info-artist))
;;         (title (emms-track-get track 'info-title)))
;;     (cond ((and artist title)
;;             (concat artist " - " title))
;;             (title title)
;;             ((eq (emms-track-type track) 'file)
;;             (track-title-from-file-name (emms-track-name track)))
;;             (t (emms-track-simple-description track)))))

;; (setq emms-track-description-function 'my-emms-track-description)


;; (cond
;;  ;; test to see if D-Bus notifications are available
;;  ((if (and (require 'dbus nil t)
;; 	   (dbus-ping :session "org.freedesktop.Notifications"))
;;       (progn
;; 	(setq notify-method 'notify-via-dbus-notifications)
;; 	(require 'notifications))))
;;  ;; could use the message system otherwise
;;  (t (setq notify-method 'notify-via-message)))

;; (defun notify-via-notifications (title msg icon)
;;   "Send notification with TITLE, MSG via `D-Bus'."
;;   (notifications-notify
;;    :title title
;;    :body msg
;;    :app-icon icon
;;    :urgency 'low))

;; (defun notify-via-messages (title msg)
;;   "Send notification with TITLE, MSG to message."
;;   (message "APPOINTMENT: %s" msg))

;; (defun emms-notifications-dbus (track-name)
;;   "Share track name via `D-Bus'."
;;   (let ((icon "/usr/share/icons/breeze/apps/16@3x/umbrello.svg"))
;;     (notify-via-notifications "EMMS is now playing:" track-name icon)))

;; (defun emms-notifications-message (track-name)
;;   "Share track name via Emacs minibuffer."
;;   (message "EMMS is now playing: %s" track-name))

;; (setq emms-player-next-function 'emms-notify-and-next)

;; (defun emms-notify-and-next ()
;;   "Send a notification of track and start next."
;;   (emms-next-noerror)
;;   (let ((track-name (emms-track-description (emms-playlist-current-selected-track))))
;;     (cond
;;      ((eq notify-method 'notify-via-dbus-notifications)
;;       (emms-notifications-dbus track-name))
;;      (t (emms-notifications-message track-name)))))

;; (spaceleader-keys
;;   :prefix "SPC m" 
;;   "m" '(counsel-major :which-key "Major modes")
;;   "n" '(emms-next :which-key "Next")
;;   "s" '(emms-stop :which-key "Next")
;;   "h" '(emms-seek-backward :which-key "Seek backward")
;;   "l" '(emms-seek-forward :which-key "Seek forward")
;;   "j" '(emms-toggle-random-playlist :which-key "Sufftle")
;;   "d" '(emms-play-directory :which-key "Play the dir")
;;   "p" '(emms-play-directory :which-key "Play the dir")
;;   "p" '(emms-previous :which-key "Previous")
;;   "rt" '(emms-toggle-repeat-track :which-key "Repeat Track")
;;   "rp" '(emms-toggle-repeat-playlist :which-key "Repeat Playlist")
;;   "SPC" '(emms-pause :which-key "Play/Pause"))

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

(use-package pdf-tools
  :demand t
  :init
  (setq pdf-tools-installer-os "pacman")
  :config
  (pdf-tools-install))

(add-hook 'pdf-view-mode-hook
          (lambda ()
            (setq mode-line-format t)))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(defun spaceorg-mode-visual-fill()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . spaceorg-mode-visual-fill))

(defun spaceorg-mode-setup ()
  (setq org-src-tab-acts-natively t
        org-src-tab-acts-natively     t
        org-src-preserve-indentation  t
        org-src-fontify-natively      t)
  (org-indent-mode)
  (org-overview)
  (display-line-numbers-mode 0)
  (variable-pitch-mode t)
  (hs-minor-mode t)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . spaceorg-mode-setup)
  :config
  (setq org-ellipsis " ↴"
        org-hide-emphasis-markers t
        org-agenda-files '("~/Projects/docs/Tasks.org")
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t)
  (spaceorg-font-setup))

  (use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("💭" "🧿" "✿" "◉" "●" "◉")))

(defun org-run-code-block ()
  (interactive)
  (org-ctrl-c-ctrl-c)
  (org-mode))

(general-m
  :keymaps 'org-mode-map
  "r" '(org-run-code-block :which-key "Run Code block")
  "v" '(org-display-inline-images :which-key "Display inline Images")
  "i=" '(org-wrap= :which-key "Wrap =")
  "il" '(org-insert-link :which-key "Insert Link"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (jupyter . t)
   (ein . t)
   (julia . t)
   (lua . t)))

(setq org-startup-with-inline-images t)

(eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                     (:session . "python")
                                                     (:results . "both")
                                                     (:pandoc . "t")
                                                     (:exports . "both")
                                                     (:kernel . "python3")))

(setq org-babel-default-header-args:ein-python '((:session . "localhost:8888/emacsnotebook.ipynb")))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("ein" . "src ein-python"))
(add-to-list 'org-structure-template-alist '("jp" . "src jupyter-python"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("jl" . "src julia"))

(setq org-confirm-babel-evaluate nil)

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(defun spaceorg-babel-tangle-config ()
  (interactive)
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'spaceorg-babel-tangle-config)))
