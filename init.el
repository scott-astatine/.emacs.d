;;; -- Summary
;; This is my Emacs config. The init.el is probably not formated and commented properly because I
;; write all of the code with docs and in `Config.org` and tangle the code blocks to init.el

;;; -- Code
(setq native-comp-eln-load-path (list (expand-file-name "eln-cache/" user-emacs-directory)))

;;;; Default global variables
(setq scroll-step 1
      scroll-margin 2
      word-wrap t
      blink-cursor-mode t
      large-file-warning-threshold nil
      use-short-answers t
      yes-or-no-prompt "y/n"
      initial-major-mode 'org-mode
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      visible-bell nil)


;;;; Enable/disable builtin modes
(progn
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 1)
  (menu-bar-mode -1)
  (column-number-mode)
  (global-display-line-numbers-mode t)
  (global-prettify-symbols-mode)
  (menu-bar--display-line-numbers-mode-relative)
  (toggle-word-wrap 1)
  (recentf-mode 1)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil))

(add-to-list 'recentf-exclude '"~/.emacs.d/.cache/var/bookmark-default.el")
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . 'pdf-view-mode))

;;;; display-buffer-alist  directives for split windows
(defun popup-alist-item (regexp &optional dedicated right extra-props)
  "Returns a list for 'display-buffer-alist' with some default properties and optional EXTRA-PROPS
   if right is non-nil the popup window will on the right side of the window else on the bottom"

  (setq-local res-list
	      `(,regexp
		(display-buffer-reuse-window display-buffer-in-side-window)
		(window-height   . 0.38)
		(window-width    . 0.45)
		(side            . ,(if right 'right 'bottom))
		(dedicated       . ,(if dedicated t -1))
		(reusable-frames . -1)))

  (if extra-props
      (dolist (ind extra-props)
	(add-to-list 'res-list ind t)))
  res-list)

(defun set-display-buffer-alist (rightL &optional bottomL extraL)
  (setq-local tmp-display-buff-alist '())
  (if rightL
      (dolist (regexp rightL)
	(add-to-list 'tmp-display-buff-alist
		     (popup-alist-item regexp t t) t)))
  (if bottomL
      (dolist (regexp bottomL)
	(add-to-list 'tmp-display-buff-alist
		     (popup-alist-item regexp t t) t)))
  (if extraL
      (dolist (i extraL)
	(add-to-list 'tmp-display-buff-alist i t)))
  (setq display-buffer-alist tmp-display-buff-alist))


;;;; Bottom popup frames
(setq bottom-popup-buffers-alist
      `(,shell-command-buffer-name-async
	"^\\*R"
	"^\\*Flycheck"
	"^\\*Buffer"
	"^\\*Messages"
	"^\\*Warnings"
	"^\\*Backtrace"
	"*elpaca-manager[a-z]*"
	"*elpaca-log[a-z]*"
	"^\\*TeX Help"
	"*[a-z]term[a-z]*"))


;;;; Right Side popup frames
(setq right-popup-buffers-alist
      `("^\\*elpaca-info"
	"^\\*lsp-"
	"^\\magit"
	"^\\*Dictionary"
	"^\\*R Dired"
	"^\\*Process"
	"^\\*Help"))

(setq custom-display-bufffer-alist
      `(,(popup-alist-item "^\\*Go-Translate" t nil '((body-function . gts-buffer-hook)))
	))


(set-display-buffer-alist right-popup-buffers-alist bottom-popup-buffers-alist
			  custom-display-bufffer-alist)



(defun gts-buffer-hook (win)
  (aw-switch-to-window win)
  (general-def
    :keymaps 'local
    :states '(normal visual insert)
    "<escape>" 'kill-this-buffer
    "q" 'kill-this-buffer
    "s" 'gts-tts-speak-buffer-data
    "j" 'next-line
    "k" 'previous-line
    "l" 'forward-char
    "h" 'backward-char
    "v" 'evil-visual-char)
  (evil-insert-state)
  (hide-mode-line-mode 1))

;;; Emacs Hooks

;;;; Common hooks 
(defvar hooks-for-disabling-commonmodes
  '(term-mode-hook
    helpful-mode-hook
    fundamental-mode-hook
    help-mode-hook
    Man-mode-hook
    Info-mode-hook
    vterm-mode-hook
    dashboard-mode-hook

    elpaca-log-mode-hook
    elpaca-info-mode-hook
    elpaca-ui-mode-hook

    messages-buffer-mode-hook
    ielm-mode-hook
    dictionary-mode-hook
    image-mode-hook
    pdf-outline-buffer-mode-hook
    lsp-help-mode-hook
    shell-mode-hook
    inferior-ess-r-mode-hook
    sage-shell-mode-hook
    symbols-outline-mode-hook

    TeX-special-mode-hook
    ibuffer-mode-hook
    treemacs-mode-hook
    eshell-mode-hook))

(dolist (mode hooks-for-disabling-commonmodes)
  (add-hook mode (lambda ()
                   (progn
                     (setq word-wrap t)
                     (display-line-numbers-mode 0)
                     (hide-mode-line-mode 1)))))


;;;; Hooks specific to  Major mode
(add-hook 'Info-mode-hook
	  (lambda ()
	    (setq-local visual-fill-column-width 90)))

;; prog-mode hooks
(add-hook 'prog-mode-hook (lambda ()
			    (progn
			      (setq word-wrap t)
			      (outline-minor-mode 1)
			      )))

;;;; After init hooks
(add-hook 'after-init-hook
          (lambda ()
            (progn
              (persp-switch "main")
	      (kill-buffer "*Messages*")
	      (kill-buffer "*scratch*")
              (setq evil-normal-state-cursor 'box)
              (setq evil-visual-state-cursor 'hollow)
              (setq evil-replace-state-cursor 'hbar)
              (recentf-mode 1)
              (recentf-load-list)
              (arkomacs-font-config)
              (set-cursor-color "wheat"))))

;;;; Kill emacs hooks
(dolist (func '(save-pdf-themed--mode-state
                recentf-save-list
                ))
  (add-hook 'kill-emacs-hook func))

;;; Font Config
(progn
  (defvar arkomacs-font-size 130)
  (defvar arkomacs-variable-pich-font-size 180)
  (defvar arkomacs-code-font "JetBrains Mono")
  (defvar arkomacs-variable-pitch-font "Vollkorn")
  (defvar arkomacs-org-heading-font arkomacs-variable-pitch-font))

(defun arkomacs-font-config ()
  (interactive)

  (setq line-spacing 0.08)

  (set-face-attribute 'default nil
                      :font arkomacs-code-font
                      :height arkomacs-font-size
                      :weight 'normal)

  (set-face-attribute 'fixed-pitch nil
                      :font arkomacs-code-font
                      :height arkomacs-font-size
                      :weight 'medium
                      :slant 'normal)

  (set-face-attribute 'variable-pitch nil
                      :font arkomacs-variable-pitch-font
                      :height arkomacs-variable-pich-font-size
                      :weight 'regular)

  ;; Face font locks
  (set-face-attribute 'font-lock-comment-face nil
                      :weight 'semi-bold
                      :slant 'italic)

  (set-face-attribute 'font-lock-keyword-face nil
                      :weight 'bold)
  (set-face-attribute 'font-lock-function-name-face nil
                      :slant 'italic)
  (set-face-attribute 'font-lock-function-call-face nil
                      :slant 'italic)

  (set-face-attribute 'font-lock-type-face nil
                      :slant 'normal
                      :weight 'bold)
  (set-fontset-font
   t '(#x1f000 . #x1faff)
   (font-spec
    :family "Noto Color Emoji"
    :size 17
    :weight 'normal
    :width 'normal
    :slant 'normal
    ))


  (variable-pitch-mode t))

(arkomacs-font-config)

;;; Bootstrap 'Elpaca' pkg manager
(progn
  (defvar elpaca-installer-version 0.6)
  (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
  (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
  (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
  (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
				:ref nil
				:files (:defaults "elpaca-test.el" (:exclude "extensions"))
				:build (:not elpaca--activate-package)))
  (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
	 (build (expand-file-name "elpaca/" elpaca-builds-directory))
	 (order (cdr elpaca-order))
	 (default-directory repo))
    (add-to-list 'load-path (if (file-exists-p build) build repo))
    (unless (file-exists-p repo)
      (make-directory repo t)
      (when (< emacs-major-version 28) (require 'subr-x))
      (condition-case-unless-debug err
          (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                   ((zerop (call-process "git" nil buffer t "clone"
					 (plist-get order :repo) repo)))
                   ((zerop (call-process "git" nil buffer t "checkout"
					 (or (plist-get order :ref) "--"))))
                   (emacs (concat invocation-directory invocation-name))
                   ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
					 "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                   ((require 'elpaca))
                   ((elpaca-generate-autoloads "elpaca" repo)))
              (progn (message "%s" (buffer-string)) (kill-buffer buffer))
            (error "%s" (with-current-buffer buffer (buffer-string))))
	((error) (warn "%s" err) (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
      (require 'elpaca)
      (elpaca-generate-autoloads "elpaca" repo)
      (load "./elpaca-autoloads")))
  (add-hook 'after-init-hook #'elpaca-process-queues)
  (elpaca `(,@elpaca-order))

  ;; Elpaca 'use-package'

  (elpaca elpaca-use-package
    (elpaca-use-package-mode)
    (setq elpaca-use-package-by-default t))
  (elpaca-wait))

;;; Elpaca modes hooks
(add-hook 'elpaca-manager-mode-hook (lambda () (progn (setq-local evil-normal-state-cursor '(bar . 0)))))

(use-package blackout
  :demand t)

(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package ef-themes
  :demand t
  :config
  (setq doom-themes-padded-modeline t
	doom-themes-enable-bold t
	doom-themes-enable-italic t)
  :init
  (load-theme 'ef-tritanopia-dark t))

(use-package doom-modeline
  :demand t
  :init
  (setq doom-modeline-height                 32
        doom-modeline-buffer-file-name-style 'file-name
        doom-modeline--vcs-icon              " "
        display-time-format                  " %H:%M:%S "
        display-time-interval                1
        doom-modeline-icon                   t
        doom-modeline-mu4e                   t
        doom-modeline-buffer-encoding nil)
  (display-time-mode 1)
  (doom-modeline-mode 1)

  :config
  (doom-modeline-def-modeline 'main
    '(bar
      window-number modals
      matches buffer-info
      remote-host checker
      parrot selection-info
      buffer-position)
    '(objed-state
      persp-name
      battery
      irc mu4e
      gnus github
      debug
      misc-info lsp
      minor-modes input-method
      indent-info buffer-encoding
      major-mode vcs " ")))

(add-hook 'treemacs-mode-hook #'hide-mode-line-mode)

;;; Smooth-Scrolling config
(progn
  (pixel-scroll-mode)
  (pixel-scroll-precision-mode))

(use-package smooth-scroll
  :config
  (smooth-scroll-mode))

(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 0
      scroll-preserve-screen-position 1
      pixel-scroll-precision-large-scroll-height 40.0
      auto-window-vscroll nil
      mouse-wheel-progressive-speed t 
      jit-lock-defer-time 0)

(use-package nerd-icons
  :demand t)

(use-package nerd-icons-dired
  :demand t
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :demand t
  :after nerd-icons
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-ibuffer
  :after nerd-icons
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package which-key
  :demand t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.05
        which-key-add-column-padding 0)
  (which-key-mode))

(use-package which-key-posframe
  :demand t
  :after which-key
  :config
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-center
	which-key-posframe-border-width 2
	which-key-posframe-parameters '((left-fringe . 8)
					(right-fringe . 8))
	)

  (which-key-posframe-mode))

(use-package evil
  :demand t
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-C-d-scroll t)
  (evil-want-C-i-jump nil)
  (evil-move-cursor-back nil)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-move-beyond-eol t)
  (evil-ex-visual-char-range t)
  :init
  (setq evil-undo-system 'undo-fu)

  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-fu)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :demand t
  ;; :custom
  ;; (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-escape
  :demand t
  :config
  (setq evil-escape-key-sequence "kj"
        evil-escape-delay 0.09)
  (evil-escape-mode 1))

(use-package ace-window
  :config
  (ace-window-display-mode)
  (ace-window-posframe-mode 1))

(use-package hydra
  :demand t)

;; (defhydra hydra-text-scale (:timeout 4)
;;   "scale text"
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out")
;;   ("f" nil "finished" :exit t))

;;; General.el Keybindings
(use-package general
  :demand t
  :init
  (general-evil-setup)
  :config

;;;; Sanity

  (define-key evil-motion-state-map  "K" nil)
  (define-key evil-motion-state-map  "C-[" nil)
  (general-define-key
   :states '(normal visual)
   ;; Basic Navigation
   "j" 'next-line
   "k" 'previous-line
   "h" 'backward-char
   "l" 'forward-char

   ;; Other Stuff
   "gl" 'find-file-at-point
   "zw" '(count-words :wk "word-count"))

  (defun ex-M ()
    (interactive)
    (execute-extended-command nil))

  (general-define-key
   :states '(normal visual treemacs)
   :keymaps 'override
   "L" 'next-buffer
   "H" 'previous-buffer
   "]" 'evil-end-of-visual-line
   "[" 'evil-beginning-of-visual-line
   "E" 'evil-end-of-line
   "B" 'evil-beginning-of-line
   "P" 'evil-jump-item
   ";" 'ex-M
   "g/" 'evilnc-comment-or-uncomment-lines)

  (general-define-key
   :states '(normal visual treemacs)
   "]" 'evil-end-of-visual-line
   "[" 'evil-beginning-of-visual-line
   ";" 'ex-M)


;;;; CTRL Maps
  ;; Minibuffer
  (general-define-key
   :keymaps 'minibuffer-mode-map
   "C-k" 'previous-line-or-history-element
   "C-j" 'next-line-or-history-element 
   "C-l" 'forward-char
   "C-h" 'backward-char
   "C-p" 'previous-line-or-history-element
   "C-n" 'next-line-or-history-element)

  ;; Global Keymaps
  (general-define-key
   :keymaps '(override evil-treemacs-state-map)
   "C-x C-b" 'split-ibuffer
   "C-w" 'evil-window-map
   "C-o" 'toggle-transparency
   "C--" 'text-scale-decrease
   "C-=" 'text-scale-increase
   "C-." 'evil-window-increase-width
   "C-," 'evil-window-decrease-width
   "C->" 'evil-window-increase-height
   "C-<" 'evil-window-decrease-height
   "C-t" '(mtt :wk "Open Vterm")
   "M-x" 'ex-M
   )

  (general-define-key
   :states '(normal insert visual)
   :keymaps '(override evil-treemacs-state-map)
   "C-<tab>" 'consult-buffer
   "<f5>" '(run-current-project :wk "Run")
   "<f6>" '(run-project-in-term :wk "Run Project in term")
   "C-q" 'quit-win-and-kill-buff
   "C-j" 'pixel-scroll-up
   "C-k" 'pixel-scroll-down)

;;;; For navigation in insert mode
  (general-define-key
   :states 'insert
   :keymaps 'override
   "C-l" 'forward-char
   "C-h" nil
   "C-k" 'evil-previous-visual-line
   "C-j" 'evil-next-visual-line
   "C-]" 'evil-end-of-visual-line
   "C-[" 'evil-beginning-of-visual-line)

;;;; Elpaca maps
  (general-define-key
   :keymaps 'elpaca-manager-mode-map
   "q" 'kill-this-buffer)

  (general-define-key
   :states '(normal visual insert)
   "C-p" 'consult-yank-from-kill-ring)

  (general-def 'insert
    "C-g" 'evil-normal-state
    "C-h" nil))

(elpaca-wait)

(general-def '(normal visual)
  "SPC" nil
  "m" nil)

(general-create-definer leader-key-SPC
  :keymaps 'override
  :states '(normal visual treemacs)
  :prefix "SPC")

(general-create-definer leader-key-ctrl-b
  :keymaps 'override
  :states 'normal
  :prefix "C-b")

(general-create-definer leader-key-m
  :states 'normal
  :prefix "m")

(general-create-definer leader-key-ctrl-c
  :states '(visual normal insert)
  :prefix "C-c")

(defun reload-major-mode ()
  (interactive)
  (funcall major-mode))

(leader-key-ctrl-b
  :states '(normal visual)
  "t"  '(reload-major-mode :wk "Toogle Major Mode")
  "m"  '(hide-mode-line-mode :wk "Toogle Modeline"))

(leader-key-SPC
  "j"       '(consult-imenu :wk "IMenu")
  "v"       '(eval-expression :wk "Eval Exp")
  "x"       '(gts-do-translate :wk "Translate")
  "r"       '(eval-last-sexp :wk "Eval Last Sexp")
  "w"       '(evil-window-map :wk "Window")
  "ww"      '(set-window-width :wk "Set Width")
  "wm"      '(delete-window-and-kill-buffer :wk "Delete Window")
  "wi"      '(set-window-height :wk "Set Height")
  "w\\"     '(evil-window-set-width :wk "Set width full")
  "w C-\\"  '(evil-window-set-width :wk "Set width full")
  "w-"      '(evil-window-set-height :wk "Set height full")
  "w0"      '(balance-windows :wk "balance-windows")
  "w C-0"   '(balance-windows :wk "balance-windows")
  "w C--"   '(evil-window-set-height :wk "Set height full")
  "a"       '(ace-select-window :wk "Select Window")
  "qq"      '(save-buffers-kill-terminal :wk "Exit Emacs")
  "d"       '(kill-this-buffer :wk "Kill Buffer")
  "e"       '(treemacs-select-window :wk "Treemacs Toggle"))

(leader-key-SPC
  :prefix "SPC t"
  :wk "Toogle"
  "t"     '(consult-theme :wk "Choose theme")
  "a"     '(global-tabnine-mode :wk "Tabnine Completion")
  "i"     '(toogle-ispell-dict-lang :wk "Change Ispell dict")
  "c"     '(display-time-mode :wk "Display Time")
  "l"     '(display-line-numbers-mode :wk "Toogle line numbers")
  "h"     '(hl-line-mode :wk "Toogle line highlight")
  "b"     '(display-battery-mode :wk "Toogle Battery")
  "v"     '(visual-fill-column-mode :wk "Center Column")
  "d"     '(elcord-mode :wk "Discord status")
  "m"     '(hide-mode-line-mode :wk "Toogle Modeline"))

(defun open-books-from-books-dir ()
  (interactive)
  (consult-find "~/Bücher"))

(defun find-in-projects-dir ()
  (interactive)
  (consult-find "~/Projects"))

(leader-key-SPC
  :prefix "SPC f"
  :wk "File..."
  "s"     '(save-buffer :wk "Save Buffer")
  "/"    '(find-file-in-/ :wk "Find in '/'")
  "g"     '(sudo-find-file :wk "Sudo find")
  "e"     '(rename-file :wk "Rename File")
  "d"     '(delete-file :wk "Delete File")
  "o"     '(find-file :wk "Open File")
  "j"     '(dired-jump :wk "Open Dired")
  "w"     '(find-file-other-window :wk "Open File other in win")
  "t"     '(consult-ripgrep :wk "Find text in project")
  "p"     '(find-in-projects-dir :wk "Find projects")
  "b"     '(open-books-from-books-dir :wk "Open Books")
  "f"     '(project-find-file :wk "Find file in project")
  "r"     '(recentf :wk "Open Recent File"))

(leader-key-SPC
  :prefix "SPC g"
  :which-key "Magit..."
  "s"     '(magit-stage-modified :wk "Stage")
  "l"     '(magit-log :wk "Commig Log")
  "u"     '(magit-unstage :wk "Stage")
  "g"     '(magit :wk "Status")
  "d"     '(magit-diff :wk "Diff")
  "p"     '(magit-push :wk "Push")
  "P"     '(magit-pull :wk "Pull")
  "c"     '(magit-commit :wk "Commit"))

(leader-key-SPC
  :prefix "SPC s"
  "s"     '(swiper-isearch :wk "Search...")
  "w"     '(websearch-term :wk "Search on www...")
  "p"     '(websearch-region :wk "Point search on www...")
  "g"     '(google-translate-query-translate :wk "Google Translate...")
  "t"     '(gts-do-translate :wk "Translate")
  "d"     '(dictionary-search :wk "Search word..."))

(defun open-mu4e-inbox ()
  (interactive)
  (mu4e-update-index)
  (mu4e-search-maildir mu4e-inbox-folder))

(defun open-mu4e-refile ()
  (interactive)
  (mu4e-update-index)
  (mu4e-search-maildir mu4e-refile-folder))

(defun open-mu4e-entwürfe ()
  (interactive)
  (mu4e-update-index)
  (mu4e-search-maildir mu4e-drafts-folder))

(defun open-mu4e-trash ()
  (interactive)
  (mu4e-update-index)
  (mu4e-search-maildir mu4e-trash-folder))

(defun open-mu4e-wichtig ()
  (interactive)
  (mu4e-update-index)
  (mu4e-search-maildir mu4e-wichtig-folder))

(defun reload-emacs-config ()
  (interactive)
  (load-file user-init-file)
  (reload-emacs-config))

(leader-key-SPC
  :prefix "SPC c"
  "p" '(pomm :wk "Pomodoro")
  "m" nil
  "a" '(alarm-clock-set :wk "Timer")
  "t" '(telega :wk "Telegram"))

(leader-key-SPC
  :prefix "SPC cm"
  "a" '(open-mu4e-refile             :wk "Alle Nachrichten ")
  "c" '(mu4e-compose-new             :wk "Compose Mail ")
  "d" '(open-mu4e-entwürfe           :wk "Entwürfe ")
  "m" '(mu4e                         :wk "mu4e")
  "o" '(org-mime-org-buffer-htmlize  :wk "Send Org mail 📭")
  "b" '(org-mime-org-subtree-htmlize :wk "Send current heading 📭")
  "s" '(mu4e-context-switch          :wk "Konto Wechseln ")
  "i" '(open-mu4e-inbox              :wk "Posteingang ")
  "t" '(open-mu4e-trash              :wk "Papierkorb ")
  "w" '(open-mu4e-wichtig            :wk "Wichtig     "))

(defun insert-spc ()
  (interactive)
  (insert " "))

(leader-key-SPC
  :prefix "SPC i"
  "d"     '(insert-current-date :wk "Insert Date")
  "'"     '(wrap-quotes :wk "Wrap Quotes")
  "i"     '(nerd-icons-insert :wk "Insert nerd icon")
  "["     '(wrap-sb :wk "Wrap []")
  "\\"    '(wrap-latex-eq  :wk "Wrap in LaTeX equation")
  "9"     '(wrap-rb :wk "Wrap ()")
  "4"     '(wrap-dollar :wk "Wrap $")
  "]"     '(wrap-cb :wk "Wrap {}")
  "SPC"   '(insert-spc :wk "Insert Date")
  "t"     '(insert-current-time :wk "Insert Time")
  "e"     '(emoji-insert :wk "Insert Emoji"))

(leader-key-SPC
  :prefix "SPC h"
  "f"     '(describe-function :wk "Describe Function")
  "v"     '(describe-variable :wk "Describe Variable"))

(defun mtt ()
  (interactive)
  (multi-vterm)
  (hide-mode-line-mode))

(leader-key-SPC
  :prefix "SPC o"
  "j"      '((lambda () (interactive) (find-file "~/Bücher/Personal/Journal.org")) :wk "Open Journal")
  "c"      '((lambda () (interactive) (find-file "~/.emacs.d/Config.org")) :wk "Open Config")
  "b"      '(eww :wk "eww")
  "s"      '(arko-scratch-buffer :wk "Scratch buffer")
  "e"      '(eshell :wk "Eshell"))

(leader-key-SPC
  :prefix "SPC z"
  "t"      '(split-h-vterm        :wk "Open Term")
  "j"      '(split-thought-bubble :wk "Open Thought Bubble")
  "b"      '(split-scratch-buffer :wk "Split *scratch*")
  "e"      '(split-elisp-repl     :wk "Elisp REPL")
  "s"      '(split-sage-repl      :wk "Sage REPL"))

(leader-key-SPC
  :prefix "SPC b"
  :wk "Buffer"
  "l" '(evil-switch-to-windows-last-buffer :wk "Last Buffer")
  "b" '(split-ibuffer :wk "Last Buffer")
  "k" '(kill-this-buffer :wk "Kill Buffer")
  "f" '(switch-to-buffer :wk "Switch Buffer")
  "w" '(switch-to-buffer-other-window :wk "Switch Buffer in other win")
  "p" '(consult-project-buffer :wk "Project Buffers")
  "d" '(kill-buffer :wk "Find & Kill"))

(leader-key-SPC
  :prefix "SPC of"
  "a" '(bookmark-set :whick-key "Add Bookmark")
  "f" '(bookmark-jump :whick-key "Open Bookmark")
  "d" '(bookmark-delete :whick-key "Delete Bookmark"))

(leader-key-SPC
  :prefix "SPC p"
  "c" '(set-project-run-cmd :wk "Set run/build cmd")
  "b" '(project-switch-to-buffer :wk "Switch Buffer")
  "f" '(project-find-file :wk "Find file")
  "k" '(project-kill-buffers :wk "Kill Project Buffers")
  "p" '(project-switch-project :wk "Switch Project"))

(leader-key-SPC
  :prefix "SPC pm"
  "v" '(elpaca-visit :wk "Visit a pkg's local repo")
  "u" '(elpaca-fetch-all :wk "Update packages")
  "d" '(elpaca-delete :wk "Delete package")
  "i" '(elpaca-info :wk "Package info")
  "b" '(elpaca-browse :wk "Visit a pkg's remote repo")
  "m" '(elpaca-manager :wk "Elpaca"))

(general-define-key
 :keymaps 'swiper-isearch-map
 "C-j" 'next-line
 "C-k" 'previous-line
 "C-r" 'swiper-query-replace)

;;; Vertico Completion for the minibuffer
(progn
  (use-package vertico
    :demand t                             ; Otherwise won't get loaded immediately
    :custom
    (vertico-count 13)
    (vertico-resize t)
    (vertico-cycle t)
    :config
    (vertico-mode)
    ;; Extensions
    (vertico-multiform-mode)

    ;; Current selected candidate prompt
    (advice-add #'vertico--format-candidate :around
		(lambda (orig cand prefix suffix index _start)
                  (setq cand (funcall orig cand prefix suffix index _start))
                  (concat
                   (if (= vertico--index index)
                       (propertize "📍" 'face 'vertico-current)
                     "  ")
                   cand))))

  (use-package vertico-posframe
    :demand t
    :config
    (setq vertico-posframe-poshandler 'posframe-poshandler-frame-top-center)
    (setq vertico-posframe-parameters
          '((left-fringe . 6)
            (right-fringe . 6)
            (vertico-posframe-border-width . 2)))

    :init
    (setq vertico-multiform-commands
          '((consult-line
             posframe
             (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
             (vertico-posframe-border-width . 10)
             (vertico-posframe-fallback-mode . vertico-buffer-mode))
            (t posframe)))
    (vertico-posframe-mode 1)))

(general-define-key
 :keymaps 'vertico-map
 "<f1>" nil
 "<tab>"  #'vertico-next
 "<backtab>"  #'vertico-previous
 "C-j"  #'vertico-next
 "C-k"  #'vertico-previous
 "?" #'minibuffer-completion-help
 "C-M-n" #'vertico-next-group
 "C-M-p" #'vertico-previous-group
 ;; Multiform toggles
 "<backspace>" #'vertico-directory-delete-char
 "C-<backspace>" #'vertico-directory-delete-word
 "RET" #'vertico-directory-enter
 "C-i" #'vertico-insert
 "M-U" #'vertico-multiform-unobtrusive)

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package savehist
  :elpaca nil
  :demand t
  :init
  (savehist-mode 1))

(defun dw/get-project-root ()
  (when (fboundp '(project-root (project-current t)))
    (project-root (project-current t))))

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-i" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :config
  (setq consult-async-min-input 2)
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(elpaca-wait)

(use-package persp-mode
  :demand t
  :config
  (setq persp-nil-name "default")
  ;; (setq persp-add-buffer-on-after-change-major-mode t)
  (persp-mode))

;;;; Add buffer to current persp Hooks

(dolist (mode '(telega-root-mode-hook
		telega-chat-mode-hook
		mu4e-headers-mode-hook
		mu4e-main-mode-hook
		journalctl-mode-hook
		dired-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (if (not (eq persp-last-persp-name persp-nil-name))(persp-add-buffer (buffer-name))))))

(general-define-key
 :states '(normal visual insert)
 :keymap 'override
  "<f1>" 'persp-switch
  "C-a" 'persp-key-map
  "C-0" 'persp-next
  "C-9" 'persp-prev)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defun toggle-transparency (&optional alpha)
  "Toggle emacs window transparency"
  (interactive)
  (let ((current-alpha (or (frame-parameter nil 'alpha-background) 100)))
    (set-frame-parameter
     nil 'alpha-background
     (if (= current-alpha 100)
         (if alpha alpha 70)
       100))))


(toggle-transparency)

(set-fringe-style 1)
(setq window-divider-default-right-width 2)

;; (setq left-margin-width  4
;;       right-margin-width 4)

(defun set-window-height (height)
  "Set the height of the current window to the specified HEIGHT."
  (interactive "nSet window height: ")
  (if (> height (window-total-height))
      (enlarge-window (- height (window-total-height)))
    (shrink-window (- (window-total-height) height))))

(defun set-window-width (width)
  "Set the width of the current window to WIDTH."
  (interactive "nSet window width: ")
  (if (> width (window-width (selected-window)))
      (enlarge-window-horizontally (- width (window-width)))
    (shrink-window-horizontally (- (window-width) width))))

(defun delete-window-and-kill-buffer ()
  (interactive)
  (kill-this-buffer)
  (evil-window-delete))

(defvar arkomacs-split-popups-height 15)
(defvar arkomacs-split-popups-width 0.9)

(defun split-window-vertically-with-width (width)
  "Splits the current window vertically with the specified WIDTH."
  (interactive "nWindow height: ")
  (split-window-right)
  (windmove-right)
  (set-window-width width))

(defun split-window-below-with-height (height)
  "Splits the current window horizontally and switches to the new window.
     The new window will be given the specified HEIGHT."
  (interactive "nWindow height: ")
  (split-window-below)
  (windmove-down)
  (set-window-height height))

(defun arkomacs-split-popup-with-function (func &optional vertically)
  "Split a popup and call the FUNC in inside that window."
  (interactive "p")
  (if vertically
      (split-window-vertically-with-width )
    (split-window-below-with-height arkomacs-split-popups-height))
  (funcall func)
  (set-window-dedicated-p (selected-window) t))

(defun split-elisp-repl ()
  (interactive)
  (arkomacs-split-popup-with-function 'ielm))

(defun split-sage-repl ()
  (interactive)
  (split-window-below-with-height arkomacs-split-popups-height)
  (sage-shell:run-sage 'sage)
  (set-window-dedicated-p (selected-window) t))

(defun arko-scratch-buffer()
  (interactive)
  (let* ((base-name "*Scratch*")
         (n (length (seq-filter (lambda (buffer)
                                  (string-prefix-p base-name (buffer-name buffer)))
                                (buffer-list))))
         (name (if (= n 0) base-name (format "%s<%d>" base-name (1+ n)))))
    (switch-to-buffer (get-buffer-create name))
    (org-mode)
    (insert (format "* Org Scratch buffer %s\n\n" n))
    (if (not (eq persp-nil-name persp-last-persp-name))
	(persp-add-buffer (buffer-name)))))

(defun split-scratch-buffer ()
  "Split scratch buffer"
  (interactive)
  (arkomacs-split-popup-with-function 'arko-scratch-buffer)
  (hide-mode-line-mode))

(defun split-thought-bubble ()
  "Split thought bubble"
  (interactive)
  (arkomacs-split-popup-with-function (lambda () (find-file "~/Bücher/Personal/ThoughtBubble.org")))
  (hide-mode-line-mode))

(defun split-vterm (height)
  "Split vterm"
  (interactive "nWindow height: ")
  (split-window-below-with-height height)
  (multi-vterm)
  (set-window-dedicated-p (selected-window) t))

(defun split-h-vterm ()
  (interactive)
  (split-vterm arkomacs-split-popups-height)
  (hide-mode-line-mode))

(defun split-ibuffer ()
  (interactive)
  (arkomacs-split-popup-with-function 'ibuffer))

(defvar project-term-run-cmd nil)
(defun set-project-run-cmd ()
  "Set the project build/run command"
  (interactive)
  (setq project-term-run-cmd (compilation-read-command project-term-run-cmd)))

(defun run-project-in-term ()
  "Run current project in Vterm"
  (interactive)
  (setq compilation-window-width 80)
  (setq compilation-buffer-name "VTermCompilation")
  (setq compilation-project--root (project-root (project-current t)))
  (save-buffer)
  (if (get-buffer compilation-buffer-name)
      (kill-buffer compilation-buffer-name))
  (if (eql project-term-run-cmd nil)
      (set-project-run-cmd))

    ;;; TODO — Implement this...
  ;; (if (and (not (eql compilation-project--root default-directory))
  ;; 	   (not (eql project-term-run-cmd nil)))
  ;;     (set-project-run-cmd))

  (split-window-vertically-with-width compilation-window-width)
  (vterm)
  (vterm-send-string (concat project-term-run-cmd "\n"))
  (rename-buffer compilation-buffer-name)
  (set-window-dedicated-p (selected-window) t))

(defun quit-win-and-kill-buff ()
  "Quit browsing the outline buffer."
  (interactive)
  (let ((win (selected-window)))
    (evil-window-next nil)
    (quit-window t win)))

(defvar project-run-cmds
  '((cargo . "cargo run")
    (npm . "npm start")
    (python . "python main.py")
    (pnpm . "pnpm run")
    (flutter . "flutter run -d linux")
    (make . "make")))

;; Define a map that contains the package manager/build system config file names
(defvar project-config-files
  '((cargo . "Cargo.toml")
    (npm . "package.json")
    (python . "main.py")
    (pnpm . "pnpm-lock.json")
    (flutter . "pubspec.yaml")
    (make . "Makefile")))

(defun run-current-project ()
  "Run the current project with `run cmd`
First it looks for 'run.sh' file in the project root
if there is a file in the root folder then it reads it and runs term
with the contents of the file, If there is not file then it looks for
preconfigured project/package manager files, if the package manager config
is in the preconfigured list the it generates .runconfig file with the preconfigured
command and run the project."

  (interactive)
  (setq project-runner-wh 14)
  (setq current-project--root (project-root (project-current t)))
  (setq project-run-config-file "run.sh")

  (cd current-project--root)
  (let ((runconf-file-exists (file-exists-p project-run-config-file))
        (project-runner--buffername "Project Runner")
        (run-command nil))

      ;;; Set the run/build cmd
    (if (not runconf-file-exists)
	(let ((config-file nil))
	  (dolist (file-map project-config-files)
            (progn
              (setq config-file-name (cdr file-map))
              (when (locate-dominating-file current-project--root config-file-name)
                (setq run-command (cdr (assq (car (rassoc config-file-name project-config-files))
                                             project-run-cmds)))
                (write-region run-command nil (concat
                                               current-project--root
                                               project-run-config-file))

		(setq runconf-file-exists
		      (file-exists-p (concat current-project--root project-run-config-file)))
                
                (message (concat
                          "Written pre-configured run cmd to `"
                          current-project--root project-run-config-file
                          "` in project root")))))))

    (if (and (not run-command)
             (not runconf-file-exists))
	(progn
          (write-region "" nil (concat current-project--root project-run-config-file))
          (message
           (concat "No pre-configured package manager file found, generated `"
                   current-project--root project-run-config-file
                   "` in the project root."))))

      ;;; Split a popup and run the project
    (when runconf-file-exists
      (if (get-buffer project-runner--buffername)
          (kill-buffer project-runner--buffername))
      (save-buffer)
      (split-window-below-with-height  project-runner-wh)
      ;; (multi-vterm)
      ;; (vterm-send-string (concat "cd " current-project--root " && sh " project-run-config-file "\n"))
      (term (concat "cd " current-project--root " && sh " project-run-config-file "\n"))
      (general-def
        :keymaps 'local
        :states '(normal insert)
        "C-c" 'vterm--self-insert
        "<escape>" 'kill-this-buffer
        "C-k" 'kill-this-buffer)
      (rename-buffer project-runner--buffername)
      (set-window-dedicated-p (selected-window) t)
      (evil-normal-state)
      ;; (visual-fill-column-mode 2)
      )))

(defun current-filename ()
  "Current filename without extension."
  (file-name-sans-extension
   (file-name-nondirectory buffer-file-name)))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root using tramp"
  (interactive (list (read-file-name "file: " "/sudo::/")))
  (let ((tramp-file-name (expand-file-name file-name)))
    (find-file tramp-file-name)))

(defun find-file-in-/ (file-name)
  "Like find file, but opens the file in '/' folder"
  (interactive (list (read-file-name "file: " "/")))
  (let ((tramp-file-name (expand-file-name file-name)))
    (find-file tramp-file-name)))

(defun move-to-prev-window ()
  (interactive)
  (evil-window-prev 1))

;;; Font wrapping functions for org mode
(defun wrap-- (m1 &optional m2)
  (interactive "P")
  (if (use-region-p)
      (progn
        (kill-region (region-beginning) (region-end))
        (insert m1)
        (yank)
        (insert (if m2 m2 m1)))
    (message "No region selected")))

(defun arkomacs-org-wrap-verbatim ()
  (interactive)
  (wrap-- "="))

(defun arkomacs-org-wrap-code ()
  (interactive)
  (wrap-- "~"))

(defun arkomacs-org-wrap-strike ()
  (interactive)
  (wrap-- "+"))

(defun arkomacs-org-wrap-bold ()
  (interactive)
  (wrap-- "*"))

(defun arkomacs-org-wrap-italics ()
  (interactive)
  (wrap-- "/"))

(defun arkomacs-org-wrap-underline ()
  (interactive)
  (wrap-- "_"))

(defun insert-current-date ()
  (interactive)
  (insert (format-time-string "%b %d, %a")))

(defun insert-current-time ()
  (interactive)
  (insert (format-time-string "%H:%M")))

(defun wrap-quotes ()
  (interactive)
  (wrap-- "\""))

(defun wrap-sb ()
  (interactive)
  (wrap-- "[" "]"))

(defun wrap-dollar ()
  (interactive)
  (wrap-- "$"))

(defun wrap-latex-eq ()
  (interactive)
  (wrap-- "\\[" "\\]"))

(defun wrap-cb ()
  (interactive)
  (wrap-- "{" "}"))

(defun wrap-rb ()
  (interactive)
  (wrap-- "(" ")"))

(require 'treesit)
(setq treesit-language-source-alist
   '((bash            "https://github.com/tree-sitter/tree-sitter-bash")
     (org             "https://github.com/milisims/tree-sitter-org")
     (markdown        "https://github.com/ikatyang/tree-sitter-markdown")

     (css             "https://github.com/tree-sitter/tree-sitter-css")
     (html            "https://github.com/tree-sitter/tree-sitter-html")
     (tsx             "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript      "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (javascript      "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")

     (c               "https://github.com/tree-sitter/tree-sitter-c")
     (cpp             "https://github.com/tree-sitter/tree-sitter-cpp")
     (cmake           "https://github.com/uyha/tree-sitter-cmake")
     (make            "https://github.com/alemuller/tree-sitter-make")

     (elisp           "https://github.com/Wilfred/tree-sitter-elisp")
     (clojure         "https://github.com/sogaiu/tree-sitter-clojure")
     (python          "https://github.com/tree-sitter/tree-sitter-python")
     (rust            "https://github.com/tree-sitter/tree-sitter-rust")
     (go              "https://github.com/tree-sitter/tree-sitter-go")

     (json            "https://github.com/tree-sitter/tree-sitter-json")
     (toml            "https://github.com/tree-sitter/tree-sitter-toml")
     (yaml            "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
 '((yaml-mode             . yaml-ts-mode)
   (bash-mode             . bash-ts-mode)
   (go-mode               . go-ts-mode)
   (rust-mode             . rust-ts-mode)
   (shell-script-mode     . bash-ts-mode)
   ;; (js2-mode              . js-ts-mode)
   ;; (typescript-mode       . typescript-ts-mode)
   (json-mode             . json-ts-mode)
   (css-mode              . css-ts-mode)
   (python-mode           . python-ts-mode)))

(setq ispell-dict-toogle-state t)
(defun toogle-ispell-dict-lang ()
  (interactive)
  (if ispell-dict-toogle-state
      (progn
	(setq ispell-dictonary "en_US"
              ispell-alternate-dictionary (expand-file-name "~/.englisch_worte.txt")
              company-ispell-dictonary ispell-dictonary)
        (setq ispell-dict-toogle-state nil)
	(message "Switched dict lang to Englisch"))
    (progn
      (setq ispell-dictonary "de_DE"
            ispell-alternate-dictionary (expand-file-name "~/.deutsche-dict.txt")
            company-ispell-dictonary ispell-dictonary)
      (setq ispell-dict-toogle-state t)
      (message "Switched dict lang to Deutsch"))))

(toogle-ispell-dict-lang)

(use-package company
  :demand t
  :config
  (global-company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-common-or-cycle)
        ("<backtab>" . company-select-previous)
        ("C-j" . company-complete-common-or-cycle)
        ("C-p" . company-select-previous))
  :custom
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-idle-delay 0.0))


(use-package company-box
  :init
  (setq company-box-scrollbar nil
	company-box-tooltip-maximum-width 140
	company-box-icons-alist 'company-box-icons-idea
	company-box-backends-colors
	'((company-yasnippet :all "lime green" :selected
			     (:background "lime green" :foreground "black"))
	  (company-ispell :all "pink")))
  :hook (company-mode . company-box-mode))


(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package company-emoji)
(use-package company-math)

(setq company-backends
      '(company-bbdb
	company-semantic
	company-cmake
	company-capf
	company-clang
	company-files
	(company-dabbrev-code
	 company-gtags
	 company-etags
	 company-keywords)
	company-oddmuse
	company-dabbrev

	company-ispell
	company-emoji
	company-math-symbols-latex
	company-math-symbols-unicode
	company-yasnippet
	))

(dolist (mode
	 '(emacs-lisp-mode-hook
	   ielm-mode-hook
	   org-mode-hook))
  (add-hook mode (lambda ()
                   (progn
                     (make-local-variable 'company-backends)
                     (add-to-list 'company-backends 'company-elisp)))))

(use-package pfuture
  :demand t)
(use-package treemacs
  :demand t
  :config
  (setq treemacs-deferred-git-apply-delay        0.05
        treemacs-directory-name-transformer      #'identity
        treemacs-display-in-side-window          t
        treemacs-eldoc-display                   'simple
        treemacs-file-event-delay                2000
        treemacs-file-follow-delay               0.05
        treemacs-file-name-transformer           #'identity
        treemacs-follow-after-init               t
        treemacs-expand-after-init               t
        treemacs-is-never-other-window           t
        treemacs-missing-project-action          'remove
        treemacs-move-forward-on-expand          nil
        treemacs-position                        'right
        treemacs-recenter-after-project-jump     'always
        treemacs-recenter-after-project-expand   'on-distance
        treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
        treemacs-show-cursor                     nil
        treemacs-sorting                         'alphabetic-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes        t
        treemacs-tag-follow-cleanup              t
        treemacs-tag-follow-delay                0.05
        treemacs-wide-toggle-width               70
        treemacs-width                           35
        treemacs-width-increment                 1
        treemacs-workspace-switch-cleanup        nil))

(add-hook 'treemacs-mode-hook (lambda ()
				(progn
				  (setq-local left-fringe-width 10
					      right-fringe-width 10)
				  (treemacs-toggle-fixed-width))))

(use-package treemacs-nerd-icons
  :demand t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil
  :after treemacs
  :demand t)

(use-package project-treemacs
  :elpaca ( :host github :repo "scott-astatine/project-treemacs")
  :after treemacs
  :demand t
  :config
  (project-treemacs-mode 1)
  (treemacs-project-follow-mode 1)
  (setq treemacs--project-follow-delay 0.05))

(use-package lsp-treemacs
  :elpaca (:host github :repo "scott-astatine/lsp-treemacs")
  :after lsp
  :config
  (setq lsp-treemacs-theme "nerd-icons"
	lsp-treemacs-sync-mode t))

(general-define-key :keymaps 'treemacs-mode-map
  "a" 'windmove-left
  "K" 'evil-scroll-line-up
  "J" 'evil-scroll-line-down
  "C-k" 'evil-scroll-line-up
  "C-j" 'evil-scroll-line-down)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
    :bind
    ([remap describe-function] . helpful-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-key] . helpful-key))

;; (use-package dashboard
;;   :demand t
;;   :init
;;   (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
;;   (setq dashboard-banner-logo-title "")
;;   (setq dashboard-startup-banner 'logo)
;;   (setq dashboard-center-content t)
;;   (setq dashboard-show-shortcuts nil)
;;   (setq dashboard-items '((recents  . 5)
;;                           (projects . 5)))
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-set-init-info t)
;;   (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
;;                                                      :height 1.1
;;                                                      :v-adjust -0.05
;;                                                      :face 'font-lock-keyword-face))
;;   :config
;;   (dashboard-setup-startup-hook))

;; (dashboard-modify-heading-icons '((recents . "file-text")
;;                                   (bookmarks . "book")))

(use-package lsp-mode
  :hook
  (lsp-mode . outline-minor-mode)
  :commands
  (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
        lsp-headerline-arrow ""
        lsp-restart 'ignore
        lsp-enable-completion-at-point t
	lsp-log-io nil
        lsp-diagnostics-provider 'flycheck)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after lsp-mode
  :config
  (lsp-ui-mode)
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-mode)
  (setq lsp-ui-doc-delay 0.4
        lsp-ui-doc-position 'top
        lsp-ui-doc-max-height 12
        lsp-ui-doc-max-width 90
        lsp-ui-doc-show-with-cursor t

	lsp-ui-peek-fontify 'always

	lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-show-hover nil
	lsp-ui-sideline-show-code-actions nil
	lsp-ui-sideline-show-symbol nil
        lsp-ui-doc-show-with-mouse t))


(general-define-key
 :states 'normal
 :keymaps 'lsp-ui-peek-mode-map
 "")

(defun lsp-outline()
  "Display lsp outline for current file"
  (interactive)
  (if (eql major-mode 'dart-mode)
      (lsp-dart-show-flutter-outline nil)
    (symbols-outline-show)))


(general-define-key
 :keymaps 'lsp-mode-map
 "C-S-i" 'lsp-format-buffer
 "TAB"   nil
 "<f2>"  'lsp-rename
 "<f7>"  'lsp-clangd-find-other-file
 "C-l f" 'lsp-ui-doc-focus-frame
 "C-l o" 'lsp-outline
 "C-l u" 'lsp-ui-doc-unfocus-frame)


(general-define-key
 :keymaps 'lsp-mode-map
 :states '(normal visual)
 "gd" 'lsp-find-definition
 "gr" 'lsp-find-references
 "K"  'lsp-describe-thing-at-point)


(general-define-key
 :keymaps 'lsp-ui-doc-frame-mode-map
 :states 'override
 "q" 'lsp-ui-doc-unfocus-frame
 "<escape>" 'lsp-ui-doc-unfocus-frame)

(general-define-key
 :keymaps 'symbols-outline-mode-map
 :states 'normal
 "l" 'symbols-outline-click
 "h" 'symbols-outline-visit
 "j" 'symbols-outline-next
 "k" 'symbols-outline-prev
 "f" 'symbols-outline-click)

(use-package symbols-outline
  :config
  (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch)
  (setq symbols-outline-window-position 'right)
  (symbols-outline-follow-mode))

(add-hook 'symbols-outline-mode-hook (lambda ()
			    (progn
			      (setq-local evil-normal-state-cursor '(bar . 0))
			      (hl-line-mode))))

;; (use-package dap-mode
;;   :custom
;;   (lsp-enable-dap-auto-configure nil)
;;   :config
;;   (dap-ui-mode 1)
;;   :config
;;   ;; Set up Node debugging
;;   (require 'dap-node)
;;   (dap-node-setup) ;; Automatically installs Node debug adapter if needed

;;   ;; Bind `C-c l d` to `dap-hydra` for easy access
;;   (general-define-key
;;     :keymaps 'lsp-mode-map
;;     :prefix lsp-keymap-prefix
;;     "d" '(dap-hydra t :wk "debugger")))

(use-package flycheck)

;; (use-package ripgrep)

(use-package rust-mode
  :hook 
  (rust-ts-mode . lsp-deferred)
  :config
  (setq rust-format-on-save t
	lsp-rust-analyzer-proc-macro-enable t))


(add-hook 'rust-mode-hook
        (lambda () (setq indent-tabs-mode nil)))

(use-package rust-playground)

(setq lsp-clangd-binary-path "/bin/clangd")
(add-hook 'c++-mode-hook (lambda ()
			   (progn
			     (lsp)
			     (c++-ts-mode))))

(add-hook 'c-mode-hook (lambda ()
			   (progn
			     (lsp)
			     (c-ts-mode)))) 

(add-to-list 'auto-mode-alist '("\\CMakeLists\\'" . cmake-ts-mode))

(use-package go-mode
  :config
  (gofmt-before-save)
  :hook
  (go-mode . lsp-deferred))

(use-package go-playground
  :config
  (setq go-playground-basedir (concat user-emacs-directory "/var/go-playground"))
  :general
  (:keymaps 'go-playground-mode-map
	    "C-c C-k" #'go-playground-rm))

(use-package glsl-mode)

(use-package json-mode)

(use-package yaml-mode)

;; (use-package qml-mode)

(use-package dart-mode
 :config
 :hook (dart-mode . lsp))

;; (use-package lsp-dart)

(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "ipython")
  (dap-python-debugger 'debugpy)
  :config
  (setq lsp-pylsp-plugins-pycodestyle-enabled nil
        lsp-pylsp-plugins-pylint-enabled t)

  ;; (require 'dap-python)
  )

;; (use-package python-docstring
;;   :ghook 'python-mode-hook
;;   :blackout t)

(use-package ess)

;; (use-package gnuplot)

;; (use-package julia-mode)

(use-package ein
  :config
  (setq *ein:file-buffername-template* "%s"
	ein:tb-buffer-name-template "%s")
  (setq ein:output-area-inlined-images t
        ob-ein-inline-image-directory "~/.emacs.d/.cache/ob-ein-images"))

;; (leader-key-ctrl-b
;;   :keymaps ein:ipdb-mode-map
;;   "d" '(ein:worksheet-delete-cell :wk "Delete Cell"))

(use-package jupyter
  :commands (jupyter-run-repl jupyter-connect-repl)
  :config
  (setq jupyter-server-buffer-name "*jupyter-server*"))

(use-package highlight-defined)
(use-package lispy)
(use-package elisp-slime-nav)
;;(use-package cider)

(use-package slime)
(setq inferior-lisp-program "sbcl")

(use-package lua-mode)

(use-package web-mode
  :gfhook #'lsp
  :config
  (setq lsp-eslint-enable nil)
  (setq web-mode-markup-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-css-indent-offset 2)
  :mode (("\\.[tj]sx\\'" . web-mode)
         ("\\.[tj]s\\'"  . web-mode)
         ("\\.html\\'"   . web-mode)))

(use-package emmet-mode
  :hook
  (web-mode . emmet-mode))

(use-package prettier
  :config
  (setenv "NODE_PATH" "/usr/lib/node_modules")
  :hook
  (web-mode . prettier-mode))

(use-package lsp-tailwindcss)

(use-package auctex
  :elpaca
  (auctex
   :pre-build (("./autogen.sh")
               ("./configure"
                "--with-texmf-dir=$(dirname $(kpsexpand '$TEXMFHOME'))")
               ("make")))
  :init
  (require 'tex)
  :config
  (setq latex-delete-tex-log t)
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))
  (setq preview-auto-cache-preamble nil)
  :custom
  (flycheck-tex-lacheck-executable "/bin/lacheck")
  (TeX-source-correlate-method 'synctex)
  (TeX-clean-confirm nil)
  (TeX-source-correlate-start-server nil)
  :hook
  ((TeX-mode . prettify-symbols-mode)
   (TeX-mode . TeX-source-correlate-mode)
   (TeX-mode . yas-minor-mode)
   (TeX-mode   . lsp)))

(use-package company-auctex
  :elpaca (:host github :repo "scott-astatine/company-auctex")
  :after auctex
  :config
  (company-auctex-init))


(use-package lsp-latex
  :after '(auctex lsp))

;;; Latex compile functions
(defvar-local latex-compile-on-save t)
(defun toggle-latex-compile-on-save ()
  "Toggle the value of latex-compile-on-save."
  (interactive)
  (setq-local latex-compile-on-save (not latex-compile-on-save))
  (message (concat "'latex-compile-on-save' set to '" (if latex-compile-on-save "t" "nil") "'.")))

(defun compile-latex ()
  "Compile current latex file"
  (interactive)
  (when (eq major-mode 'latex-mode)
    (setq latex-pdf-file-name (concat (current-filename) ".pdf"))
    (TeX-command "LaTeX" #'current-filename nil)))

(defun refresh-pdfview-buffer ()
  "Refresh the pdfview buffer if it is opened."
  (interactive)
  (when (file-exists-p latex-pdf-file-name)
    (let ((pdf-buffer (get-file-buffer latex-pdf-file-name)))
      (when pdf-buffer
        (with-current-buffer pdf-buffer
          (pdf-view-revert-buffer nil t))))))

(defun open-latex-pdf ()
  "Open latex pdf in other window"
  (interactive)
  (if (eq major-mode 'latex-mode)
      (progn
	(save-buffer)
	(setq latex-pdf-file-name (concat (current-filename) ".pdf"))
	(if (get-buffer latex-pdf-file-name)
	    (kill-buffer latex-pdf-file-name))
	(compile-latex)
	(when (file-exists-p latex-pdf-file-name) 
	  (split-window-vertically-with-width 90)
	  (find-file latex-pdf-file-name)
	  (rename-buffer latex-pdf-file-name)
	  (general-def
	    :keymaps 'local
	    :states 'normal
	    "a" 'move-to-prev-window
	    "C-l" 'move-to-prev-window)
	  (set-window-dedicated-p (selected-window) t)))
    (message "Not a latex file!")))

(with-eval-after-load 'tex
  (progn
    (add-hook 'TeX-mode-hook
	      (lambda ()
		(add-hook 'after-save-hook
			  (lambda ()
			    (if latex-compile-on-save
				(compile-latex))) nil t)))
    (add-hook 'TeX-after-compilation-finished-functions
              (lambda (proc)
                (refresh-pdfview-buffer)))
    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))))

(use-package cdlatex
  :after org)

(defun arkomacs-org-latex-config ()
  (setq org-latex-compiler "lualatex")
  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc" t ("pdflatex")) ("T1" "fontenc" t ("pdflatex"))
          ("" "graphicx" t) ("" "longtable" nil) ("" "wrapfig" nil)
          ("" "rotating" nil) ("normalem" "ulem" t) ("" "amsmath, amsthm, amsfonts" t)
          ("" "amssymb" t) ("" "capt-of" nil) ("colorlinks=true" "hyperref" nil)
	  ("" "chemfig" t) ("" "unicode" t)))

  (setq org-latex-packages-alist '(("margin=1in" "geometry" t)))
  (setq org-babel-latex-htlatex-packages '("[usenames]{color}" "{tikz}" "{color}" "{listings}" "{amsmath}" "{chemfig}"))

  (setq org-highlight-latex-and-related '(native latex script entities)
	org-preview-latex-default-process 'dvisvgm)

  (setq org-format-latex-options
	'(:scale 2.0
		 :foreground default :background "Transparent"
		 :html-foreground "Black" :html-background "Transparent"
		 :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))

(use-package org-fragtog
  :after org
  :hook
  (org-mode . org-fragtog-mode))

(leader-key-m
 :keymaps 'TeX-mode-map
 :states '(normal visual)
 "c" '(compile-latex :wk "Compile Latex")
 "s" '(toggle-latex-compile-on-save :wk "Compile on save")
 "v" '(open-latex-pdf :wk "View pdf"))

(use-package lass
  :elpaca
  ( :package "laas"
    :fetcher github
    :repo "scott-astatine/LaTeX-auto-activating-snippets")
  :after auctex
  :hook
  (org-mode . laas-mode))

(use-package sage-shell-mode
  :custom
  ;; (sage-shell:use-prompt-toolkit nil)
  ;; (sage-shell:use-simple-prompt t)
  (sage-shell:set-ipython-version-on-startup nil)
  (sage-shell:check-ipython-version-on-startup nil))

(use-package ob-sagemath
  :after org
  :config
  (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)
  (setq sage-shell:input-history-cache-file
	(concat user-emacs-directory "var/sage_history")
	sage-shell:check-ipython-version-on-startup nil
	))

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode))

;; (use-package doom-snippets
;;   :after yasnippet)

(defun arkomacs-org-font-setup ()
  ;;; Replace list hyphen with dot for `lists`
  (interactive)
  (arkomacs-font-config)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;;; Org heading font scaling
  (dolist (face '((org-level-1 . 1.6)
                  (org-level-2 . 1.14)
                  (org-level-3 . 1.07)
                  (org-level-4 . 1.04)
                  (org-level-5 . 1.02)
                  (org-level-6 . 1.02)
                  (org-level-7 . 1.02)
                  (org-level-8 . 1.02)))
    (set-face-attribute (car face) nil :font arkomacs-org-heading-font :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code  nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun arkomacs-org-mode-setup ()
  (setq org-src-tab-acts-natively     t
        org-src-preserve-indentation  t
        org-pretty-entities           t
        org-src-fontify-natively      t)

  (setq org-display-remote-inline-images 'download
	org-image-align 'center)

  (arkomacs-org-latex-config)
  (org-indent-mode)
  (org-overview)
  (display-line-numbers-mode 0)
  (variable-pitch-mode t)
  (hs-minor-mode t)
  (yas-minor-mode)
  (visual-line-mode 1)
  (add-hook 'window-configuration-change-hook #'arkomacs-org-font-setup nil t))

(defun arkomacs-org-mode-visual-fill()
  (setq visual-fill-column-width 115
	visual-fill-column-enable-sensible-window-split t
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package org
  :elpaca
  ( :package "org"
    :local-repo "org"
    :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
    :pre-build (progn (require 'elpaca-menu-org)
                      (elpaca-menu-org--build))
    :autoloads "org-loaddefs.el"
    :build (:not elpaca--generate-autoloads-async)
    :files (:defaults ("etc/styles/"
                       "etc/styles/*" "doc/*.texi")))
  :demand t
  :hook ((org-mode . arkomacs-org-mode-setup)
	 (org-mode . arkomacs-org-mode-visual-fill))
  :config
  (setq org-ellipsis " ⤵"
        org-hide-emphasis-markers t
        org-agenda-files '("~/Bücher/Personal/Tasks.org")
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-imenu-depth 4
        org-startup-with-inline-images t
        org-startup-with-latex-preview t)
  (arkomacs-org-font-setup))

(use-package org-superstar
  :demand t
  :elpaca ( :package "org-superstar"
            :repo "https://github.com/integral-dw/org-superstar-mode.git"
            :local-repo "org-superstar")

  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  
  (setq org-superstar-cycle-headline-bullets nil)
  (setq org-superstar-headline-bullets-list
        '("◉" ("◈" ?◈) "○" "▷")))

(use-package org-modern
  :demand t
  :after org
  :hook
  ((org-mode                 . org-modern-mode)
   (org-agenda-finalize-hook . org-modern-agenda))
  :custom
  ((org-modern-todo t)
   (org-modern-table nil)
   (org-modern-list nil)
   (org-modern-star nil)
   (org-modern-variable-pitch nil)
   (org-modern-block-fringe nil))
  :commands
  (org-modern-mode org-modern-agenda)
  :init (global-org-modern-mode))

;;; Org Babel setup
(setq org-babel-C++-compiler "clang++"
      org-preview-latex-image-directory (concat user-emacs-directory "etc/org-latex/")
      org-confirm-babel-evaluate nil)

(with-eval-after-load 'org
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (R . t)
       (lisp . t)
       (shell . t)
       (latex . t)
       (sagemath . t)
       (julia . t)
       (jupyter . t)
       (C . t)
       (lua . t)))

    (require 'org-tempo)
    (progn
      (add-to-list 'org-structure-template-alist '("sh"  . "src shell"))
      (add-to-list 'org-structure-template-alist '("sm"  . "src sage"))
      (add-to-list 'org-structure-template-alist '("mp"  . "src sage :results file"))
      (add-to-list 'org-structure-template-alist '("py"  . "src jupyter-python"))
      (add-to-list 'org-structure-template-alist '("cpp"  . "src cpp"))
      (add-to-list 'org-structure-template-alist '("R"   . "src R"))
      (add-to-list 'org-structure-template-alist '("src" . "src"))
      (add-to-list 'org-structure-template-alist '("ein" . "src ein-python"))
      (add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
      (add-to-list 'org-structure-template-alist '("jl"  . "src julia"))

      (add-to-list 'org-src-lang-modes '("conf-unix" . conf-unix)))

    (add-hook 'org-babel-after-execute-hook
	      (lambda ()
		(interactive)
		(clear-image-cache)
		(org-display-inline-images)
		(org-latex-preview)))))

(setq org-babel-default-header-args:latex
      '((:results . "raw")
        (:exports . "results")
        ;; (:fit . t)
        (:imagemagick . t)
        ;; (:eval . "no-export")
        ))

(setq org-babel-default-header-args:sage '((:session . t)
					   (:async . "yes")
					   (:results . "drawer")))

(setq org-babel-default-header-args:jupyter-python
      '((:kernel . "python")
        (:results . "drawer")
        (:async . "yes")
        (:pandoc . "t")
        (:exports . "both")
        (:cache .   "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:eval . "never-export")))

(use-package org-mime)

(use-package org-roam
  :custom
  (org-roam-directory "~/Bücher/Notes/Org")
  (org-roam-node-default-sort 'file-atime)
  :config
  (setq org-roam-node-display-template
	(concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(leader-key-SPC
  :prefix "SPC n"
  ;; "l"  'org-roam-buffer-toggle
  "f"  'org-roam-node-find
  "g"  'org-roam-graph
  "i"  'org-roam-node-insert
  "c"  'org-roam-capture
  "j"  'org-roam-dailies-capture-today)

;;;; Org Ctrl-c keymaps
(leader-key-ctrl-c
  :keymaps 'org-mode-map
  :states '(visual normal insert)
  "C-i"     '(org-indent-block :wk "Org indent block")
  "C-x C-l" '(org-fragtog-mode :wk "Org LaTeX fagtog")
  "m"       '(org-toggle-heading :wk "Org toggle heading")
  "l"       '(org-cdlatex-mode :wk "Org Cdlatex")
  "i"       '(org-indent-block :wk "Org indent block"))

(general-define-key
 :keymaps 'org-mode-map
 ;; :states '(insert normal visual)
 "S-RET" 'org-meta-return
 "<f12>" 'org-meta-return)

(general-define-key
 :keymaps 'org-mode-map
 :states '(normal visual)
 "gj" 'outline-next-visible-heading
 "gk" 'outline-previous-heading)

;;;; SPC-m local keymaps for org-mode
(leader-key-m
  :states 'visual
  "c" '(arkomacs-org-wrap-code :wk "Wrap Code")
  "b" '(arkomacs-org-wrap-bold :wk "Wrap Bold")
  "i" '(arkomacs-org-wrap-italics :wk "Wrap italics")
  "u" '(arkomacs-org-wrap-underline :wk "Wrap Underline")
  "x" '(arkomacs-org-wrap-strike :wk "Stike Seletion")
  "v" '(arkomacs-org-wrap-verbatim :wk "Wrap Verbatim")) 

(leader-key-m
  :keymaps 'org-mode-map
  :states '(visual normal)
  "e" '(org-latex-export-to-pdf :wk "Export PDF")
  "r" '(org-ctrl-c-ctrl-c :wk "Run Code block")
  "s" '(org-edit-special :wk "Org Edit Special")
  "o" '(consult-org-heading :wk "Outline")
  "d" '(org-latex-preview :wk "Run Code block")
  "l" '(org-insert-link :wk "Insert Link"))

(leader-key-m
  :keymaps 'org-src-mode-map
  ;; :states 'normal
  "s" 'org-edit-src-exit
  "d" 'org-edit-src-abort)

(defun arkomacs-org-babel-tangle-config ()
  (interactive)
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'arkomacs-org-babel-tangle-config)))

(use-package org-present)

;;;;;; Pdf mode setup
(defvar pdf-themed--mode-state-file (concat user-emacs-directory "var/pdf-themed-mode-state"))
(defvar pdf-themed--mode nil)

(use-package pdf-tools
  :demand t
  :config
  (setq pdf-themed--mode (string-to-number (f-read pdf-themed--mode-state-file)))
  (pdf-tools-install))

(use-package saveplace-pdf-view
    :demand t
    :config
    (save-place-mode))
(elpaca-wait)

;;; pdfview-mode hooks
(dolist (mode '(doc-view-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda ()
                   (progn
                     (display-line-numbers-mode 0)
                     ))))


(add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)

(defun custom-pdf-view-mode-hook ()
  (set (make-local-variable 'evil-normal-state-cursor) (list nil))
    (if (= 1 pdf-themed--mode)
	(pdf-view-themed-minor-mode))
    (setq-local evil-normal-state-cursor (list nil))
    (add-hook 'window-configuration-change-hook #'pdf-view-center-in-window nil t))


(add-hook 'pdf-view-mode-hook #'custom-pdf-view-mode-hook)

(add-hook 'pdf-view-themed-minor-mode-hook
	  (lambda ()
	    (if pdf-view-themed-minor-mode
		(setq pdf-themed--mode 1)
	      (setq pdf-themed--mode -1))))

(defun save-pdf-themed--mode-state ()
  "Save pdf-themed--mode state"
  (interactive)
  (f-write (number-to-string pdf-themed--mode) nil pdf-themed--mode-state-file))

;;; -- PdfView-mode functions --
(add-hook 'pdf-outline-buffer-mode-hook
	  (lambda ()
	    (hl-line-mode)))

(defvar pdf-outline-buffer-exists nil)
(defun pdf-outlf ()
  (interactive)
  (setq pdf-outline-buffer-exists nil)
  (setq pdf-outline-buffer-name
        (format "*Outline %s*" (file-name-nondirectory buffer-file-name)))
;; Before opening the outline
  (dolist (buffer (buffer-list))
    (if (buffer-name buffer)
        (if (string-match "*Outline*" (buffer-name buffer))
            (progn
              (setq pdf-outline-buffer-exists t)
              (if (not (string= pdf-outline-buffer-name (buffer-name buffer)))
                  (progn
                    (setq pdf-outline-buffer-exists nil)
                    (kill-buffer (buffer-name buffer))))))))
  (pdf-outline)
  (pdf-outline-move-to-current-page)
;; After opening the outline
  (setq-local evil-normal-state-cursor (list nil))
  (if (not pdf-outline-buffer-exists)
      (set-window-width 50)))

(defun pdf-outl ()
  (interactive)
  (pdf-outlf)
  (evil-scroll-line-to-center nil)
  (set-window-dedicated-p (selected-window) t))

(defun pdf-outline-goto-link ()
  (interactive)
  (pdf-outline-display-link)
  (pdf-outline-select-pdf-window))

(defun pdf-outline-kill ()
  (interactive)
  (dolist (buffer (buffer-list))
    (if (buffer-name buffer)
	(if (string-match "*Outline*" (buffer-name buffer))
	    (and (kill-buffer (buffer-name buffer)) (pdf-view-center-in-window))))))

(defun open-thought-bubble ()
  (interactive)
  (split-window-horizontally)
  (windmove-right)
  (set-window-width 90)
  (find-file (expand-file-name "~/Bücher/Personal/ThoughtBubble.org")))

;; (setq pdf-annot-minor-mode-map-prefix "a")
(general-def 'normal 'pdf-view-mode-map
  "q" nil
  "C-o" 'toggle-transparency
  "c" 'pdf-view-center-in-window
  "d" 'pdf-view-scroll-up-or-next-page
  "u" 'pdf-view-scroll-down-or-previous-page
  "," 'pdf-view-scroll-up-or-next-page
  "." 'pdf-view-scroll-down-or-previous-page
  "f" 'isearch-forward
  "I" 'pdf-view-midnight-minor-mode
  "i" 'pdf-view-themed-minor-mode
  "C-j" 'pdf-view-next-line-or-next-page
  "C-k" 'pdf-view-next-line-or-next-page
  "J" 'pdf-view-next-page
  "K" 'pdf-view-previous-page
  "o" 'pdf-outl
  "gh" 'pdf-history-goto

  ;; Annotations
  "ah" 'pdf-annot-add-highlight-markup-annotation
  "ax" 'pdf-annot-add-strikeout-markup-annotation
  "au" 'pdf-annot-add-underline-markup-annotation
  "au" 'pdf-annot-add-squiggly-markup-annotation
  "ac" 'pdf-annot-add-markup-annotation
  "at" 'pdf-annot-add-text-annotation
  "al" 'pdf-annot-list-annotations

  "sj" 'open-thought-bubble
  "r" 'image-rotate
  "w" 'pdf-view-fit-width-to-window
  "x" 'pdf-outline-kill)

(general-def 'normal 'pdf-outline-buffer-mode-map
  "f" 'pdf-outline-goto-link
  "o" 'outline-toggle-children
  "l" 'outline-cycle
  "h" 'outline-up-heading
  "J" 'outline-next-heading
  "K" 'outline-previous-heading
  "q" 'pdf-outline-kill
  "a" 'pdf-outline-select-pdf-window
  "d" 'pdf-outline-display-link
  "e" 'pdf-outline-toggle-subtree
  "s" 'pdf-outline-follow-mode)

(general-def 'normal 'doc-view-mode-map
  "j" 'doc-view-next-line-or-next-page
  "J" 'doc-view-next-page
  "K" 'doc-view-previous-page
  "k" 'doc-view-previous-line-or-previous-page)

(use-package nov
  :init
  (setq nov-text-width nil
        visual-fill-column-center-text t
        nov-text-width nil
        nov-unzip-program (executable-find "bsdtar")
        nov-unzip-args '("-xC" directory "-f" filename))


  :config

  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(defun custom-nov-mode-hook ()
  (setq word-wrap t
        visual-fill-column-width 90
        ;; nov-text-width visual-fill-column-width
	)

  (display-line-numbers-mode 0)
  (visual-line-mode)
  (hl-line-mode)
  (visual-fill-column-mode)
  (add-hook 'window-configuration-change-hook #'nov-render-document nil t))

;;;;;; Hook
(add-hook 'nov-mode-hook #'custom-nov-mode-hook)

(progn
  (require 'justify-kp)

  (defun my-nov-window-configuration-change-hook ()
    (my-nov-post-html-render-hook)
    (remove-hook 'window-configuration-change-hook
		 'my-nov-window-configuration-change-hook
		 t)
    (nov-mode))

  (defun my-nov-post-html-render-hook ()
    (if (get-buffer-window)
	(let ((max-width (pj-line-width))
              buffer-read-only)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (not (looking-at "^[[:space:]]*$"))
		(goto-char (line-end-position))
		(when (> (shr-pixel-column) max-width)
                  (goto-char (line-beginning-position))
                  (pj-justify)))
              (forward-line 1))))
      (add-hook 'window-configuration-change-hook
		'my-nov-window-configuration-change-hook
		nil t)))

  (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook))

(general-define-key :states '(normal visual) :keymaps 'nov-mode-map
  "gh" 'nov-history-back
  "gf" 'nov-history-forward
  "d"  'evil-scroll-down
  "u"  'evil-scroll-up
  "h"  nil
  "l"  nil
  "h"  nil
  "K"  'nov-previous-document
  "J"  'nov-next-document)

(use-package pomm
  :defer 20)
(use-package alarm-clock
  :defer 20)

;;;;;; mu4e
  (defvar mu4e-inbox-folder nil)

  (defun generate-mu4e-context (email context-name root-folder user-name)
    "Generate a mu4e context for a given EMAIL account with specified FOLDER name."

    (let ((drafts-folder    (format "/%s/Entw&APw-rfe"        root-folder))
          (sent-folder      (format "/%s/Gesendet"            root-folder))
          (wichtig-folder   (format "/%s/Wichtig"             root-folder))
          (refile-folder    (format "/%s/Alle Nachrichten"    root-folder))
          (trash-folder     (format "/%s/Papierkorb"          root-folder)))
      (make-mu4e-context
       :name context-name
       :match-func `(lambda (msg)
                      (when msg
                        (string-prefix-p (format "/%s" ,root-folder) (mu4e-message-field msg :maildir))))
       :vars `((user-mail-address     .   ,email)
               (user-full-name        .   ,user-name)
               (message-signature     .   ,(concat "\nMit Freundlichen Grüßen\n\n\n" user-name "\n\n"))

               ;; SMTP config
               (smtpmail-smtp-server  .   "smtp.gmail.com")
               (smtpmail-smtp-user    .   ,email)

               (mu4e-wichtig-folder   .   ,wichtig-folder)
               (mu4e-refile-folder    .   ,refile-folder)
               (mu4e-drafts-folder    .   ,drafts-folder)
               (mu4e-sent-folder      .   ,sent-folder)
               (mu4e-trash-folder     .   ,trash-folder)))))

  (defun set-maildir-shortcuts-from-context ()
    "Set Maildir shortcuts based on the current mu4e context."
    (interactive)
    (setq-local current-context (mu4e-context-name mu4e--context-current))
    (setq mu4e-inbox-folder (format "/%s/Posteingang"  current-context))

    (setq mu4e-maildir-shortcuts
          `((,mu4e-inbox-folder    . ?i)
            (,mu4e-sent-folder     . ?s)
            (,mu4e-wichtig-folder  . ?w)
            (,mu4e-refile-folder   . ?a)
            (,mu4e-drafts-folder   . ?d)
            (,mu4e-trash-folder    . ?t))))



  (use-package mu4e
    :elpaca nil
    :defer 30
    :init
    (require 'mu4e)
    :config
    (setq message-send-mail-function          'smtpmail-send-it
          send-mail-function                  'smtpmail-send-it
          smtpmail-smtp-service               587
          smtpmail-debug-info                 t
          smtpmail-queue-dir                  "~/.mu4eMail/mailQueue"
          mu4e-compose-format-flowed          t
          smtpmail-stream-type                nil
          message-kill-buffer-on-exit         t)

    (setq epa-file-select-keys t)

    (setq mu4e-change-filenames-when-moving   t
          mu4e-confirm-quit                   nil
          mu4e-get-mail-command               "mbsync -a -c ~/Projects/Remote-Repos/dotfiles/mbsyncrc"
          mu4e-context-policy                 'pick-first
          mu4e-compose-context-policy         'ask-if-none
          mu4e-maildir                        "~/.mu4eMail"
          mu4e-update-interval                nil)

    (setq mu4e-contexts
          `(,(generate-mu4e-context
              "scottastatine@gmail.com"
              "scottastatine"
              "scottastatine/[Gmail]"
              "Scott Astatine")
            ,(generate-mu4e-context
              "alexastatine@gmail.com"
              "alexastatine"
              "alexastatine/[Gmail]"
              "Alex Astatine")))

    (setq mu4e-bookmarks
          '((:name "Unread messages"      :query "flag:unread AND NOT flag:trashed" :key ?u)
            (:name "Today's messages"     :query "date:today..now"                  :key ?t)
            (:name "Last 7 days"          :query "date:7d..now" :hide-unread t      :key ?w)
            (:name "Messages with images" :query "mime:image/*"                     :key ?p)))

    
    ;; Hooks
    (add-hook 'mu4e-context-changed-hook 'set-maildir-shortcuts-from-context)

    (add-hook 'mu4e-compose-pre-hook (lambda () (require 'smtpmail-async)))

    (add-hook 'mu4e-main-mode-hook
              (lambda ()
                (progn
                  (setq word-wrap t)
                  (display-line-numbers-mode 0))))

    (add-hook 'mu4e-headers-mode-hook
              (lambda ()
                (progn
                  (setq-local evil-normal-state-cursor '(bar . 0))
                  (display-line-numbers-mode 0))))

    (add-hook 'mu4e-view-mode-hook
              (lambda ()
                (progn
                  (setq word-wrap t)
                  (display-line-numbers-mode 0))))
    (mu4e t))

  (with-eval-after-load 'mu4e
    (set-maildir-shortcuts-from-context))

  (general-define-key
   :states '(normal visual)
   :keymaps 'mu4e-view-mode-map
   "C-n"   'mu4e-view-headers-next
   "C-p"   'mu4e-view-headers-prev)

  (general-define-key
   :states '(normal visual)
   :keymaps 'mu4e-main-mode-map
   "a"   'mu4e-context-switch)

  (general-define-key
   :states '(normal visual)
   :keymaps 'mu4e-headers-mode-map
   "C-j"   'mu4e-headers-next
   "C-k"   'mu4e-headers-prev)

(use-package gptel
  :config
  (setq-default gptel-model "gemini-pro" ;Pick your default model
		gptel-backend (gptel-make-gemini
			       "Gemini"
			       :stream t
			       :key (f-read (expand-file-name "~/.secretKeys/GoogleGeminiApi")))))

(general-define-key
 "<f8>" 'gptel-send)

(use-package telega
  :config
  (setq telega-server-libs-prefix "/opt/tdlib-tg"
	telega-translate-to-language-by-default "de"
	telega-translate-replace-content t
	telega-filter-default 'all))

(add-hook 'telega-chat-mode-hook
	  (lambda ()
	    (progn
	      (display-line-numbers-mode 0)
	      (tabnine-mode)
	      (setq-local visual-fill-column-width 70))))

(add-hook 'telega-root-mode-hook
	  (lambda ()
	    (progn
	      (telega-filter-by-folder "Personal")
	      (display-line-numbers-mode 0))))

(general-define-key
 :states 'normal
 :keymap 'telega-chat-mode-map
 "SPC" nil)

(use-package go-translate
  :demand t
  :config
  (setq gts-translate-list '(("de" "en") ("en" "de") ("fr" "en") ("en" "fr") ("es" "en") ("en" "es") ("ru" "en") ("en" "ru")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-google-engine))
         :render (gts-buffer-render))))

(use-package google-translate)

(use-package websearch
  :config
  (setq websearch-custom-default-engine "bing")
  (add-to-list 'websearch-custom-engines '("bing" 32 "www.bing.com/search?q=" (text generic))))

(use-package elcord
  :config
  (setq elcord-refresh-rate 5))

(use-package dictionary
  :config
  (setq dictionary-use-single-buffer t))

(general-define-key
 :keymaps 'dictionary-mode-map
 :states '(normal visual)
 "RET" 'link-mouse-click)

(use-package evil-multiedit)

(leader-key-ctrl-b
  :states '(visual normal)
  "n" '(evil-multiedit-match-and-next :wk "Mulitple Cursor match next")
  "p" '(evil-multiedit-match-and-prev :wk "Mulitple Cursor match next")
  "a" '(evil-multiedit-match-all :wk "Mulitple Cursor match next"))

(leader-key-m
  :states '(visual normal)
  "n" '(evil-multiedit-match-and-next :wk "Mulitple Cursor match next")
  "p" '(evil-multiedit-match-and-prev :wk "Mulitple Cursor match next")
  "a" '(evil-multiedit-match-all :wk "Mulitple Cursor match next"))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "fish"                       ;; Set this to customize the shell to launch
        vterm-max-scrollback 200000))
(use-package multi-vterm)

(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

(setq compilation-environment '("TERM=xterm-256color"))

(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

;;;;;; EWW
(with-eval-after-load 'eww
  (setq-local arkomacs/display-images t)
  (defun arkomacs/toggle-image-display ()
    "Toggle images display on current buffer."
    (interactive)
    (setq arkomacs/display-images
          (null arkomacs/display-images))
    (arkomacs/backup-display-property arkomacs/display-images))

  (defun arkomacs/backup-display-property (invert &optional object)
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

;;;;;; Dired
(use-package diredfl)

(use-package dired-filter
  :config
  (require 'dired-filter))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (progn
	      (display-line-numbers-mode -1)
	      (text-scale-increase +2)
	      (dired-hide-details-mode)
	      (dired-async-mode)
	      (diredfl-mode)
	      (dired-filter-mode)
	      (dired-filter-by-dot-files)
	      ;; (visual-fill-column-mode)
	      )))

(setq dired-kill-when-opening-new-dired-buffer t
      ibuffer-default-sorting-mode 'major-mode)

(general-define-key
 :keymaps 'dired-mode-map
 :states 'normal
 ;; Filter
 "ss" 'dired-filter-mode
 "h"  'dired-up-directory
 "l"  'dired-find-file
 "b" 'dired-up-directory
 "DEL" 'dired-up-directory)

(general-define-key
 :keymaps 'ibuffer-mode-map
 :states 'normal
 "C-r" 'ibuffer-redisplay)

(use-package visual-fill-column
  :config
  (setq visual-fill-column-width 160))

(dolist (mode
	 '(Man-mode-hook
	   Info-mode-hook))
  (add-hook mode
	    (lambda ()
	      (progn
		(visual-fill-column-mode 1)))))

(use-package tabnine
  :commands (tabnine-start-process)
  :diminish "⌬"
  :custom
  (tabnine-wait 1)
  (tabnine-minimum-prefix-length 0)
  :hook (kill-emacs . tabnine-kill-process)
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
  (tabnine-start-process)
  :bind
  (:map  tabnine-completion-map
	 ("<tab>" . tabnine-accept-completion)
	 ("TAB" . tabnine-accept-completion)
	 ("M-f" . tabnine-accept-completion-by-word)
	 ("M-<return>" . tabnine-accept-completion-by-line)
	 ("C-g" . tabnine-clear-overlay)
	 ("M-[" . tabnine-previous-completion)
	 ("M-]" . tabnine-next-completion)))

(use-package empv
  :elpaca (empv
           :fetcher github
           :repo "isamert/empv.el")
  :config
  (setq empv-invidious-instance "https://invidious.nerdvpn.de"
	empv-audio-dir "~/Musik"
	empv-video-dir "~/Videos")
  (setq empv-radio-channels
	'(("SomaFM - Groove Salad" . "http://www.somafm.com/groovesalad.pls")
          ("SomaFM - Drone Zone" . "http://www.somafm.com/dronezone.pls")
          ("SomaFM - Sonic Universe" . "https://somafm.com/sonicuniverse.pls")
          ("SomaFM - Metal" . "https://somafm.com/metal.pls")
          ("SomaFM - Vaporwaves" . "https://somafm.com/vaporwaves.pls")))

  (add-hook 'empv-init-hook #'empv-override-quit-key)
  (add-to-list 'empv-mpv-args "--save-position-on-quit")
  (add-to-list 'empv-mpv-args "--ytdl-format=best"))

(defhydra empv-hydra nil
  "Empv hydra controller\n"
  ("("  empv-chapter-prev "Chapter -")
  (")"  empv-chapter-next "Chapter +")
  ("0"  empv-volume-up "Vol +")
  ("9"  empv-volume-down "Vol -")
  ("C"  empv-playlist-clear "Clear Playlist")
  ("N"  empv-playlist-prev "Previous Playlist")
  ("R"  empv-play-random-channel "Random Channel")
  ("Y"  empv-youtube-last-results "Youtube Last Results")
  ("["  empv-playback-speed-down "<<<")
  ("]"  empv-playback-speed-up ">>>")
  ("-"  empv-toggle-video "Toggle Video")
  ("a"  empv-play-audio "Audio")
  ("c"  empv-copy-path "Copy Path")
  ("d"  empv-play-directory "Play Directory")
  ("f"  empv-play-file "Open File")
  ("i"  empv-display-current "Info")
  ("l"  empv-log-current-radio-song-name "Log")
  ("n"  empv-playlist-next "Next")
  ("o"  empv-play-or-enqueue "Play/Enqueue")
  ("p"  empv-playlist-select "Select")
  ("q"  hydra-keyboard-quit "Quit")
  ("r"  empv-play-radio "Radio")
  ("s"  empv-playlist-shuffle "Shuffle")
  ("t"  empv-toggle "/")
  ("SPC"  empv-toggle "/")
  ("v"  empv-play-video "Play Video")
  ("x"  empv-chapter-select "Select Chapter")
  ("y"  empv-youtube "Youtube"))


(leader-key-SPC
    :prefix "SPC m"
    "m"  'empv-hydra/body
    "("  'empv-chapter-prev               
    ")"  'empv-chapter-next
    "0"  'empv-volume-up
    "9"  'empv-volume-down
    "C"  'empv-playlist-clear
    "N"  'empv-playlist-prev
    "R"  'empv-play-random-channel
    "Y"  'empv-youtube-last-results
    "["  'empv-playback-speed-down
    "]"  'empv-playback-speed-up
    "_"  'empv-toggle-video
    "a"  'empv-play-audio
    "c"  'empv-copy-path
    "d"  'empv-play-directory
    "f"  'empv-play-file
    "i"  'empv-display-current
    "l"  'empv-log-current-radio-song-name
    "n"  'empv-playlist-next
    "o"  'empv-play-or-enqueue
    "p"  'empv-playlist-select
    "q"  'empv-exit
    "r"  'empv-play-radio
    "s"  'empv-playlist-shuffle
    "t"  'empv-toggle
    "SPC"  'empv-toggle
    "v"  'empv-play-video
    "x"  'empv-chapter-select
    "y"  'empv-youtube)

(use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))

(use-package async
  :defer t
  :init
  (dired-async-mode 1))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package evil-magit
;;   :after magit)

(use-package evil-nerd-commenter)

(use-package ligature
  :demand t
  :config
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures t
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
     "\\\\" "://"))
  :init
  (global-ligature-mode t))

(use-package unicode-fonts
   :config
    (unicode-fonts-setup))

(use-package emojify
  :config
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode)))

(use-package undo-fu
  :demand t)

(use-package undo-fu-session
  :demand t
  :config
  (global-undo-fu-session-mode))

(use-package smartparens
  :demand t
  :config
  (require 'smartparens-config)

  (sp-local-pair '(TeX-mode org-mode) "$" "$" :actions '(wrap insert autoskip navigate) :when
	   nil :unless '(sp-point-after-word-p) :pre-handlers nil
	   :post-handlers '(("[d1]" "SPC")))

  (sp-pair "'" "'" :actions '(wrap insert autoskip navigate) :when
           nil :unless '(sp-point-after-word-p) :pre-handlers nil
           :post-handlers '(("[d1]" "SPC")))

  (sp-pair "\\[" "\\]")
  (sp-pair "\\begin{align*}\n  " "\n\\end{align*}" :trigger "\\ba")
  (sp-local-pair 'TeX-mode "\\left(" "\\right)" :trigger "\\l(")
  (smartparens-global-mode))

(use-package outli
  :elpaca (:host github :repo "jdtsmith/outli")
  :hook ((prog-mode . outli-mode)))

(use-package journalctl-mode)

(use-package monkeytype)
