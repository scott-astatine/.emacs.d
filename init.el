(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(setq scroll-step 1
      scroll-margin 2
      large-file-warning-threshold nil
      pixel-scroll-precision-large-scroll-height 40.0
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      visible-bell t)

(load-file "~/.emacs.d/hide-mode-line.el")

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 1)
(menu-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode t)
(menu-bar--display-line-numbers-mode-relative)
;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line "#4f4f4f")
(blink-cursor-mode 1)
(recentf-mode 1)
(recentf-load-list)

(progn
  (defvar endless-font-size 120)
  (defvar endless-code-font "JetBrains Mono")
    ;;; Previous Font "Leckerli One" Princess Sofia
  (defvar endless-variable-pitch-font "Merriweather")

  (set-face-attribute 'default nil
		      :font endless-code-font
		      :height endless-font-size
		      :weight 'normal)
  (set-face-attribute 'fixed-pitch nil
		      :font endless-code-font
		      :height (- endless-font-size 6)
		      :weight 'medium :slant 'normal)
  (set-face-attribute 'variable-pitch nil :font endless-variable-pitch-font :height 155 :weight 'regular)

  (variable-pitch-mode t)
  (custom-set-variables '(font-lock-support-mode 'tree-sitter-lock-mode)))

(defun spaceorg-font-setup ()
  ;; Replace list hyphen with dot
  (interactive)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (dolist (face '((org-level-1 . 1.6)
                  (org-level-2 . 1.14)
                  (org-level-3 . 1.07)
                  (org-level-4 . 1.04)
                  (org-level-5 . 1.02)
                  (org-level-6 . 1.02)
                  (org-level-7 . 1.02)
                  (org-level-8 . 1.02)))
    (set-face-attribute (car face) nil :font "Salsa" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

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
;; (when (bound-and-true-p noct-with-demoted-errors)
;;   (advice-add 'straight-use-package :around #'noct-inhibit-error-advice))
;; can test with something like this:
;; (use-package does-not-exist)

(use-package blackout
  :straight (blackout :host github :repo "raxod502/blackout")
  :demand t)

(use-package no-littering
  :ensure t)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(set-frame-parameter nil 'alpha '(100 . 100))
;; (add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

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
         '(85 . 50) '(100 . 100)))))
(toggle-transparency)

(set-fringe-style 1)
(setq window-divider-default-right-width 2)

(defun set-window-height (height)
  "Set the height of the current window to the specified HEIGHT."
  (interactive "nSet window height: ")
  (if (> height (window-total-height))
      (enlarge-window (- height (window-total-height)))
    (shrink-window (- (window-total-height) height))))

(defun set-window-width (width)
  "Set the width of the current window to WIDTH."
  (interactive "nSet window width: ")
  (if (> width (window-width))
      (enlarge-window-horizontally (- width (window-width)))
    (shrink-window-horizontally (- (window-width) width))))

(defun split-window-vertically-with-width (width)
  "Splits the current window vertically and switches to the new window.
     The new window will be given the specified WIDTH."
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

(defun split-repl ()
  (interactive)
  (split-window-below-with-height 15)
  (ielm)
  (setq splitwin (selected-window))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (eq splitwin (selected-window))
                (delete-window (selected-window))))))

(defun quit-window-and-kill ()
  (interactive)
  (kill-this-buffer)
  (evil-window-delete))

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

(defun split-h-vterm ()
  (interactive)
  (split-vterm 10)
  (hide-mode-line-mode))

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
  (setq compilation-project--root doom-modeline--project-root)
  (save-buffer)
  (if (get-buffer compilation-buffer-name)
      (kill-buffer compilation-buffer-name))

  ;;; If `project-term-run-cmd` is null
  (if (eql project-term-run-cmd nil)
      (set-project-run-cmd))
  (if (and (not (eql compilation-project--root default-directory))
	   (not (eql project-term-run-cmd nil)))
      (set-project-run-cmd))

  ;;; If `project-term-run-cmd` is not null
  (split-window-vertically-with-width compilation-window-width)
  (setq compilation-splitwin (selected-window))
  (vterm)
  (vterm-send-string (concat project-term-run-cmd "\n"))
  (rename-buffer compilation-buffer-name)
  (add-hook 'kill-buffer-hook
            (lambda ()
	      (progn
		(when (eq compilation-splitwin (selected-window))
                  (delete-window compilation-splitwin)))))
  )


(defun run-current-project ()
  "Run the current project"
  (interactive)
  (let ((project-root doom-modeline--project-root)
        (runconf-file (locate-dominating-file default-directory ".runconf"))
        (run-command nil))
    ;; Get the contents of the file if it exists
    (if runconf-file
        (with-temp-buffer
          (insert-file-contents (concat runconf-file ".runconf"))
          (setq run-command (buffer-string))))

    ;;; Set the run/build cmd
    (if (not run-command)
        (let ((config-file nil))
          (dolist (file-map project-config-files)
            (progn
	      (setq config-file-name (cdr file-map))
	      (when (locate-dominating-file project-root config-file-name)
                (setq run-command (cdr (assq (car (rassoc config-file-name project-config-files)) project-run-cmds)))
                (write-region run-command nil (concat project-root ".runconf")))))))

    ;;; If no .runconf file is found then generate one
    (if (and (not run-command) (not runconf-file))
        (write-region "" nil (concat project-root ".runconf"))
      (message "No pre-configured build cmd package manager file found, generated `.runconf` in the project root."))

    ;;; Split a popup and run the project
    (when run-command
      (save-buffer)
      (split-window-below-with-height 14)
      (if (get-buffer "Runner")
          (kill-buffer "Runner"))

      (term (concat "cd " project-root " && "run-command "\n"))
      (general-def
	:keymaps 'local
	:states '(normal insert)
	"C-c" 'vterm--self-insert
	"C-d" '(lambda () (interactive) (kill-this-buffer))
	"q" '(lambda () (interactive) (kill-this-buffer)))
      (setq splitwin (selected-window))
      (rename-buffer "Runner")
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (eq splitwin (selected-window))
                    (delete-window splitwin))))
      (windmove-up)
      )))



(defun insert-current-date ()
  (interactive)
  (insert (format-time-string "%b %d, %a")))

(defun insert-current-time ()
  (interactive)
  (insert (format-time-string "%H:%M")))

(defun wrap-- (m1)
  (interactive)
  (if (use-region-p)
      (progn
        (kill-region (region-beginning) (region-end))
        (insert m1)
        (yank)
        (insert m1))
    (message "No region selected")))

(defun wrap-quotes ()
  (interactive)
  (wrap-- "\""))

(defun wrap-sb ()
  (interactive)
  (if (use-region-p)
      (progn
        (kill-region (region-beginning) (region-end))
        (insert "[")
        (yank)
        (insert "]"))
    (message "No region selected")))
(defun wrap-cb ()
  (interactive)
  (if (use-region-p)
      (progn
        (kill-region (region-beginning) (region-end))
        (insert "{")
        (yank)
        (insert "}"))
    (message "No region selected")))

(defun wrap-rb ()
  (interactive)
  (if (use-region-p)
      (progn
        (kill-region (region-beginning) (region-end))
        (insert "(")
        (yank)
        (insert ")"))
    (message "No region selected")))

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
        evil-want-Y-yank-to-eol nil
        evil-want-C-d-scroll t
        evil-want-C-i-jump nil
        evil-move-cursor-back nil
        evil-move-beyond-eol nil
        evil-ex-visual-char-range t)

  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
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

(general-def 'normal
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "zw" '(count-words :which-key "word-count")
  "K" 'lsp-describe-thing-at-point)

(defun ex-M ()
  (interactive)
  (execute-extended-command nil))

(general-def '(normal visual) 'override
  "L" 'next-buffer
  "H" 'previous-buffer
  "]" 'evil-end-of-visual-line
  "[" 'evil-beginning-of-visual-line
  "E" 'evil-end-of-line
  "B" 'evil-beginning-of-line
  "P" 'evil-jump-item
  "g/" 'evilnc-comment-or-uncomment-lines
  ";" 'ex-M)


;;; CTRL Maps

(general-def '(normal insert) 'override
  "C-<tab>" 'consult-buffer
  "C-w" 'evil-window-map
  "<f5>" '(run-current-project :which-key "Run Project")
  "<f6>" '(run-project-in-term :which-key "Run Project in term")
  "C-o" 'toggle-transparency
  "C-k" 'evil-scroll-line-up
  "C-j" 'evil-scroll-line-down
  "C--" 'text-scale-decrease
  "C-=" 'text-scale-increase
  "C-," 'evil-window-increase-width
  "C-." 'evil-window-decrease-width
  ;;; Open Terminal
  "C-t" '(vterm :which-key "Open Vterm")
  ;; "C-ts" '(split-h-vterm :which-key "Split Vterm horizontally")
  ;; "C-tv" '(split-h-vterm :which-key "Split Vterm vertically")
  )

(general-def 'insert
  "C-g" 'evil-normal-state
  "C-h" 'evil-delete-backward-char-and-join)


(general-def '(normal visual)
  "SPC" nil
  "m" nil)

(general-create-definer spaceleader-keys
  :keymaps 'override
  :states '(normal visual)
  :prefix "SPC")

(general-create-definer general-m
  :states 'normal
  :prefix "m")

(defun mjort ()
  (interactive)
  (funcall major-mode))

(general-m
  :keymaps 'override
  "t"  '(mjort :which-key "Toogle Major Mode")
  "m"  '(hide-mode-line-mode :which-key "Toogle Modeline"))

(spaceleader-keys
  "m"  '(consult-imenu :which-key "IMenu")
  "w"  '(evil-window-map :which-key "Window")
  "ww" '(set-window-width :which-key "Set Width")
  "wm" '(quit-window-and-kill :which-key "Set Width")
  "wi" '(set-window-height :which-key "Set Height")
  "a"  '(ace-select-window :which-key "Select Window")
  "qq" '(save-buffers-kill-terminal :which-key "Exit Emacs")
  "d"  '(kill-this-buffer :which-key "Kill Buffer")
  "e"  '(treemacs-select-window :which-key "Treemacs Toggle"))

(spaceleader-keys
  :prefix "SPC t"
  :wk "Toogle"
  "t" '(consult-theme :which-key "choose theme")
  "c" '(display-time-mode :which-key "Display Time")
  "u" '(undo-tree-mode :which-key "Display Time")
  "s" '(hydra-text-scale/body :which-key "scale text")
  "w" '(toggle-transparency :which-key "scale text")
  "l" '(display-line-numbers-mode :which-key "Toogle line numbers")
  "h" '(hl-line-mode :which-key "Toogle line highlight")
  "b" '(display-battery-mode :which-key "Toogle Battery")
  "v" '(visual-fill-column-mode :which-key "Center Column")
  "d" '(elcord-mode :which-key "Discord status")
  "m" '(hide-mode-line-mode :which-key "Toogle Modeline"))

(spaceleader-keys
  :prefix "SPC f"
  "s" '(save-buffer :which-key "Save Buffer")
  "e" '(rename-file :which-key "Rename File")
  "d" '(delete-file :which-key "Delete File")
  "o" '(find-file :which-key "Open File")
  "f" '(project-find-file :which-key "Find file in project")
  "r" '(consult-recent-file :which-key "Open Recent File"))

(spaceleader-keys
  :prefix "SPC s"
  "s"'(swiper-isearch :which-key "Search...")
  "t"'(gts-do-translate :which-key "Translate")
  "d"'(dictionary-search :which-key "Search word..."))

(spaceleader-keys
  :prefix "SPC c"
  "e" '(eval-last-sexp :which-key "Eval last sexp"))

(defun inspc ()
  (interactive)
  (insert " "))

(spaceleader-keys
  :prefix "SPC i"
  "d" '(insert-current-date :which-key "Insert Date")
  "'" '(wrap-quotes :which-key "Wrap Quotes")
  "[" '(wrap-sb :which-key "Wrap []")
  "9" '(wrap-rb :which-key "Wrap ()")
  "]" '(wrap-cb :which-key "Wrap {}")
  "SPC" '(inspc :which-key "Insert Date")
  "t" '(insert-current-time :which-key "Insert Time")
  "e" '(emoji-insert :which-key "Insert Emoji"))

(spaceleader-keys
  :prefix "SPC h"
  "f" '(describe-function :which-key "Describe Function")
  "v" '(describe-variable :which-key "Describe Variable"))

(defun mtt ()
  (interactive)
  (multi-vterm)
  (hide-mode-line-mode))

(spaceleader-keys
  :prefix "SPC o"
  "t" '(split-h-vterm :which-key "Open Term")
  "j" '((lambda () (interactive) (find-file "~/Bücher/Personal/Journal.org")) :which-key "Open Journal")
  "c" '((lambda () (interactive) (find-file "~/.emacs.d/Config.org")) :which-key "Open Config")
  "r" '(split-repl :which-key "Elisp REPL")
  "b" '(eww :which-key "eww")
  "s" '(scratch-buffer :which-key "Open Scratch buffer")
  "e" '(eshell :which-key "Eshell"))

(spaceleader-keys
  :prefix "SPC b"
  "l" '(evil-switch-to-windows-last-buffer :which-key "Kill Buffer")
  "k" '(kill-this-buffer :which-key "Kill Buffer")
  "f" '(switch-to-buffer :which-key "Switch Buffer")
  "d" '(kill-buffer :which-key "Find & Kill"))

(general-def '(normal insert) 'override
  "C-b a" '(bookmark-set :whick-key "Add Bookmark")
  "C-b f" '(bookmark-jump :whick-key "Open Bookmark")
  "C-b d" '(bookmark-delete :whick-key "Delete Bookmark"))

(spaceleader-keys
  :prefix "SPC p"
  "c" '(set-project-run-cmd :which-key "Set run/build cmd"))
;;   "e" '(treemacs-projectile :which-key "Treemacs Projectile")
;;   "o" '(projectile-switch-project :which-key "Open Project")
;;   "d" '(projectile-remove-known-project :which-key "Add Project")
;;   "a" '(projectile-add-known-project :which-key "Add Project"))

(use-package doom-themes
  :demand
  :init (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :demand t
  :init
  (setq doom-modeline-height 24
        doom-modeline-buffer-file-name-style 'truncate-from-project
        display-time-format " %H:%M:%S "
        display-time-interval 1
        doom-modeline-buffer-encoding nil)
  (display-time-mode 1)
  (doom-modeline-mode 1))

(doom-modeline-def-modeline 'main
  '(bar window-number modals
	matches buffer-info
	remote-host checker
	parrot selection-info
        buffer-position)
  '(objed-state
    persp-name
    battery grip
    irc mu4e
    gnus github
    debug
    misc-info lsp
    minor-modes input-method
    indent-info buffer-encoding
    major-mode process vcs " "))

(add-hook 'treemacs-mode-hook #'hide-mode-line-mode)

(use-package async
  :ensure t
  :defer t
  :init
  (dired-async-mode 1))

(use-package all-the-icons
  :demand t)
(use-package all-the-icons-dired
  :demand t)

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; (use-package projectile
;;   :init
;;   (when (file-directory-p "~/Projects")
;;     (setq projectile-project-search-path '("~/Projects")))
;;   (setq projectile-switch-project-action #'projectile-dired)

;;   :config
;;   ;; (setq projectile-completion-system 'vertico)
;;   (projectile-mode +1))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

(use-package evil-nerd-commenter
  :ensure t)

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
   :ensure t
   :config
    (unicode-fonts-setup))

(use-package emojify
    :hook (after-init . global-emojify-mode))

(use-package undo-tree
  :init
  (global-undo-tree-mode))

(use-package smartparens
  :demand t
  :config
  (smartparens-global-mode))

;; (use-package beacon
;;   :ensure t
;;   :init
;;   (beacon-mode 1))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package tree-sitter)
(use-package tree-sitter-langs)

(global-tree-sitter-mode)

(use-package persp-mode
  :ensure t
  :custom
  (persp-keymap-prefix (kbd "C-a"))
  :init
  (persp-mode))

;; (add-hook 'kill-emacs-hook '(lambda () (persp-state-save persp-state-default-file)))

(general-def '(normal visual insert) 'override
  "C-p" 'persp-switch
  "<f1>" 'persp-switch
  "C-a o" nil
  "C-0" 'persp-next
  "C-9" 'persp-prev)

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
  :init
  (setq company-box-scrollbar nil
	company-box-tooltip-maximum-width 140)
  :hook (company-mode . company-box-mode))

(use-package vertico
  :demand t                             ; Otherwise won't get loaded immediately
  :straight (vertico :files (:defaults "extensions/*") ; Special recipe to load extensions conveniently
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :general
  (:keymaps '(normal insert visual motion)
            "M-." #'vertico-repeat
            )
  (:keymaps 'vertico-map
            "<tab>"  #'vertico-next
            "<backtab>"  #'vertico-previous
            "?" #'minibuffer-completion-help
            "C-M-n" #'vertico-next-group
            "C-M-p" #'vertico-previous-group
            ;; Multiform toggles
            "<backspace>" #'vertico-directory-delete-char
            "C-w" #'vertico-directory-delete-word
            "C-<backspace>" #'vertico-directory-delete-word
            "RET" #'vertico-directory-enter
            "C-i" #'vertico-quick-insert
            ;; "C-o" #'vertico-quick-exit
            "M-G" #'vertico-multiform-grid
            "M-F" #'vertico-multiform-flat
            "M-R" #'vertico-multiform-reverse
            "M-U" #'vertico-multiform-unobtrusive
            )
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
         )
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle t)
  ;; Extensions
  (vertico-grid-separator "   |   ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  (vertico-multiform-categories
   '((file reverse)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
     (t reverse)
     ))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)
     ))
  :init
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))

  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  :config
  (vertico-mode)
  ;; Extensions
  (vertico-multiform-mode)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "📍" 'face 'vertico-current)
                   "  ")
                 cand)))
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package savehist
  :init
  (savehist-mode 1))

(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :straight t
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package vertico-posframe
  :init
  (setq vertico-multiform-commands
        '((consult-line
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
           (vertico-posframe-border-width . 10)
           ;; NOTE: This is useful when emacs is used in both in X and
           ;; terminal, for posframe do not work well in terminal, so
           ;; vertico-buffer-mode will be used as fallback at the
           ;; moment.
           (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (t posframe)))
  (vertico-posframe-mode 1))

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
        treemacs-position                        'right
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

;; (use-package treemacs-projectile)

(use-package treemacs-all-the-icons
  :demand t
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-evil
  :demand t)

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

;; (use-package centaur-tabs
;;   :demand t
;;   :config
;;   (setq centaur-tabs-style "rounded"
;;         centaur-tabs-height 26
;;         centaur-tabs-set-icons t
;;         centaur-tabs-set-modified-marker t
;;         centaur-tabs-show-navigation-buttons t
;;         centaur-tabs-set-bar 'under
;;         x-underline-at-descent-line t)
;;   (centaur-tabs-headline-match)
;;   ;; (setq centaur-tabs-gray-out-icons 'buffer)
;;   ;; (centaur-tabs-enable-buffer-reordering)
;;   ;; (setq centaur-tabs-adjust-buffer-order t)
;;   (setq centaur-tabs-set-bar 'under)
;;   ;; Note: If you're not using Spacmeacs, in order for the underline to display
;;   ;; correctly you must add the following line:
;;   (setq x-underline-at-descent-line t)
;;   :bind
;;   ("C-<tab>" . centaur-tabs-forward)
;;   ("C-<iso-lefttab>" . centaur-tabs-backward))

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

(defun spacelsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)))

(use-package lsp-mode
  :hook ((lsp-mode . spacelsp-mode-setup)
	 (lsp-mode . hs-minor-mode))
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

(defun lsp-outline()
  "Display lsp symbols for current file"
  (interactive)
  (if (eql major-mode 'dart-mode)
      (lsp-dart-show-flutter-outline nil))
  (if (or (eql major-mode 'c-mode) (eql major-mode 'c++-mode))
      (lsp-treemacs-symbols)))

(defun lsp-mode-custom-keymaps()
  (interactive)
  (general-def '(normal insert) 'override
    "C-S-i"   'lsp-format-buffer
    "TAB"   nil
    "<f2>"  'lsp-rename
    "<f7>"  'lsp-clangd-find-other-file
    "C-l f" 'lsp-ui-doc-focus-frame
    "C-l o" 'lsp-outline
    "C-l u" 'lsp-ui-doc-unfocus-frame))

(add-hook 'lsp-mode-hook #'lsp-mode-custom-keymaps)

(use-package dap-mode
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

(use-package lsp-treemacs
    :after lsp)

(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package ripgrep)

(use-package rust-mode
  :ensure t
  :hook 
  (rust-mode . lsp-deferred)
  :config
  (setq rust-format-on-save t))

(add-hook 'rust-mode-hook
        (lambda () (setq indent-tabs-mode nil)))

(setq lsp-clangd-binary-path "/bin/clangd")
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
;; (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

(with-eval-after-load 'lsp-mode
  (require 'dap-cpptools))
  
;; (use-package cmake-mode)

(use-package glsl-mode)

(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package qml-mode)

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "ipython")
  (dap-python-debugger 'debugpy)
  :config
  (setq lsp-pylsp-plugins-pycodestyle-enabled nil
        lsp-pylsp-plugins-pylint-enabled t)

  (require 'dap-python))

;; (use-package python-docstring
;;   :ghook 'python-mode-hook
;;   :blackout t)

(use-package julia-mode)

(use-package ein)
(setq ein:output-area-inlined-images t
    ob-ein-inline-image-directory "~/.emacs.d/.cache/ob-ein-images")

;; (general-m
;;   :keymaps ein:ipdb-mode-map
;;   "d" '(ein:worksheet-delete-cell :which-key "Delete Cell"))

;; (use-package jupyter
;;   :commands (jupyter-run-repl jupyter-connect-repl)
;;   :config
;;   (setq jupyter-server-buffer-name "*jupyter-server*"))

(general-def 'normal emacs-lisp-mode-map 
  "K" 'elisp-slime-nav-describe-elisp-thing-at-point)

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

;; (use-package company-auctex
;;   :ensure t
;;   :config
;;   (company-auctex-init))

;; (with-eval-after-load 'company
;;   (add-to-list 'company-backends 'company-auctex))

(defun latex-comp ()
  (interactive)
  (when (eq major-mode 'latex-mode)
    (TeX-command-run-all nil)))

(add-hook 'LaTeX-mode-hook (lambda () (add-hook 'after-save-hook #'latex-comp)))

(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))



(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             '("ethz"
               "\\documentclass[a4paper,11pt,titlepage]{memoir}
    \\usepackage[utf8]{inputenc}
    \\usepackage[T1]{fontenc}
    \\usepackage{fixltx2e}
    \\usepackage{graphicx}
    \\usepackage{longtable}
    \\usepackage{float}
    \\usepackage{wrapfig}
    \\usepackage{rotating}
    \\usepackage[normalem]{ulem}
    \\usepackage{amsmath}
    \\usepackage{textcomp}
    \\usepackage{marvosym}
    \\usepackage{wasysym}
    \\usepackage{amssymb}
    \\usepackage{hyperref}
    \\usepackage{mathpazo}
    \\usepackage{color}
    \\usepackage{enumerate}
    \\definecolor{bg}{rgb}{0.95,0.95,0.95}
    \\tolerance=1000
          [NO-DEFAULT-PACKAGES]
          [PACKAGES]
          [EXTRA]
    \\linespread{1.1}
    \\hypersetup{pdfborder=0 0 0}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[11pt,a4paper]{article}
    \\usepackage[utf8]{inputenc}
    \\usepackage[T1]{fontenc}
    \\usepackage{fixltx2e}
    \\usepackage{graphicx}
    \\usepackage{longtable}
    \\usepackage{float}
    \\usepackage{wrapfig}
    \\usepackage{rotating}
    \\usepackage[normalem]{ulem}
    \\usepackage{amsmath}
    \\usepackage{textcomp}
    \\usepackage{marvosym}
    \\usepackage{wasysym}
    \\usepackage{amssymb}
    \\usepackage{hyperref}
    \\usepackage{mathpazo}
    \\usepackage{color}
    \\usepackage{enumerate}
    \\definecolor{bg}{rgb}{0.95,0.95,0.95}
    \\tolerance=1000
          [NO-DEFAULT-PACKAGES]
          [PACKAGES]
          [EXTRA]
    \\linespread{1.1}
    \\hypersetup{pdfborder=0 0 0}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))


(add-to-list 'org-latex-classes '("ebook"
                                  "\\documentclass[11pt, oneside]{memoir}
    \\setstocksize{9in}{6in}
    \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
    \\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
    \\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
    \\checkandfixthelayout
    % Much more laTeX code omitted
    "
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

(defun spaceorg-mode-setup ()
  (setq org-src-tab-acts-natively     t
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
        org-agenda-files '("~/Bücher/Personal/Tasks.org")
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t)
  (spaceorg-font-setup))

(use-package org-modern
  :hook ((org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :custom ((org-modern-todo t)
           (org-modern-table nil)
           (org-modern-list nil)
           (org-modern-star nil)
           (org-modern-variable-pitch nil)
           (org-modern-block-fringe nil))
  :commands (org-modern-mode org-modern-agenda)
  :init (global-org-modern-mode))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "○" "◈" "◉" "◇" "✳")))

(defun org-wrap-verbatim ()
  (interactive)
  (wrap-- "="))

(defun org-wrap-code ()
  (interactive)
  (wrap-- "~"))

(defun org-wrap-strike ()
  (interactive)
  (wrap-- "+"))

(defun org-wrap-bold ()
  (interactive)
  (wrap-- "*"))

(defun org-wrap-italics ()
  (interactive)
  (wrap-- "/"))

(defun org-run-code-block ()
  (interactive)
  (org-ctrl-c-ctrl-c)
  (org-mode))

(general-m
  :keymaps 'org-mode-map
  :states '(visual normal)
  "r" '(org-run-code-block :which-key "Run Code block")
  "c" '(org-wrap-code :which-key "Wrap Code")
  "o" '(consult-org-heading :which-key "Outline")
  "b" '(org-wrap-bold :which-key "Wrap Bold")
  "i" '(org-wrap-italics :which-key "Wrap italics")
  "x" '(org-wrap-strike :which-key "Stike Seletion")
  "v" '(org-wrap-verbatim :which-key "Wrap Verbatim")
  "l" '(org-insert-link :which-key "Insert Link"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   ;; (jupyter . t)
   (ein . t)
   (julia . t)
   (lua . t)))

(setq org-startup-with-inline-images t)

(eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(setq org-babel-default-header-args:jupyter-python
      '((:results . "raw")
        (:session . "jupyter-python")
        (:kernel . "python3")
        (:async . "yes")
        (:pandoc . "t")
        (:exports . "both")
        (:cache .   "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:eval . "never-export")))

(setq org-babel-default-header-args:jupyter-julia
      '((:async . "yes")
        (:session . "jupyter-julia")
        (:kernel . "julia")
        (:exports . "both")
        (:eval . "never-export")))

;; (add-to-list 'org-src-lang-modes '("jupyter-python" . python))
;; (add-to-list 'org-src-lang-modes '("jupyter-julia" . julia))
;; (add-to-list 'org-src-lang-modes '("jupyter-R" . R))

(setq org-babel-default-header-args:ein-python '((:session . "localhost:8888/emacsnotebook.ipynb")))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("ein" . "src ein-python"))
;; (add-to-list 'org-structure-template-alist '("jp" . "src jupyter-python"))
;; (add-to-list 'org-structure-template-alist '("jpn" . "src jupyter-python :results none"))
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

(use-package go-translate
  :config
  (setq gts-translate-list '(("de" "en") ("en" "de")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-google-engine))
         :render (gts-buffer-render))))

(use-package elcord
  :config
  (setq elcord-refresh-rate 5))

(use-package pdf-tools
  :demand t
  :config
  (defun hide-cursor ()
    (interactive)
    (setq cursor-type nil))

  ;;; Hooks
  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)

  (pdf-tools-install))

(use-package saveplace-pdf-view
  :demand t
  :config
  (save-place-mode 1))

(defun pdf-outlf ()
  (interactive)
  (pdf-outline)
  (pdf-outline-move-to-current-page)
  (set-window-width 50))

(defun pdf-outl ()
  (interactive)
  (pdf-outlf)
  (set-window-dedicated-p (selected-window) t))


(defun fds-pdf-outline ()
  (interactive)
  (pdf-outline-display-link)
  (pdf-outline-select-pdf-window))

(defun pdf-outline-qui ()
  (interactive)
  (kill-this-buffer)
  (quit-window))

(defun poutkill ()
  (interactive)
  (pdf-outline)
  (pdf-outline-quit-and-kill))

(defun open-thought-bubble ()
  (interactive)
  (split-window-horizontally)
  (windmove-right)
  (set-window-width 90)
  (find-file "~/Bücher/Personal/ThoughtBubble.org"))

;; (setq pdf-annot-minor-mode-map-prefix "a")

(general-def 'normal 'pdf-view-mode-map
  "q" nil
  "c" 'pdf-view-center-in-window
  "d" 'pdf-view-scroll-up-or-next-page
  "u" 'pdf-view-scroll-down-or-previous-page
  "," 'pdf-view-scroll-up-or-next-page
  "." 'pdf-view-scroll-down-or-previous-page
  "f" 'isearch-forward
  "i" 'pdf-view-midnight-minor-mode
  "I" 'pdf-view-themed-minor-mode
  "J" 'pdf-view-next-page
  "K" 'pdf-view-previous-page
  "v" 'pdf-outlf
  "o" 'pdf-outl
  "b" 'pdf-outline
  "gh" 'pdf-history-goto

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
  "x" 'poutkill)


(general-def 'normal 'pdf-outline-buffer-mode-map
  "f" 'fds-pdf-outline
  "o" 'outline-toggle-children
  "q" 'pdf-outline-quit-and-kill
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
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'hl-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  (setq nov-text-width nil)
  (setq nov-unzip-program (executable-find "bsdtar")
        nov-unzip-args '("-xC" directory "-f" filename))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(general-def 'normal 'nov-mode-map
  "K" 'nov-previous-document
  "J" 'nov-next-document)

(use-package evil-multiedit)

(general-m
  :states '(visual normal)
  "n" '(evil-multiedit-match-and-next :which-key "Mulitple Cursor match next")
  "p" '(evil-multiedit-match-and-previous :which-key "Mulitple Cursor match next")
  "a" '(evil-multiedit-match-all :which-key "Mulitple Cursor match next"))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "fish")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 200000))
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
;;   :prefix "SPC k" 
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

(defun spaceorg-mode-visual-fill()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . spaceorg-mode-visual-fill))

(kill-buffer "*Messages*")
(recentf-mode 1)
(recentf-load-list)
