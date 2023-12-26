;;; This is my Emacs config. The init.el is probably not formated and commented properly because I
;;; write all of the code with docs and in `Config.org` and tangle the code blocks to init.el

(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;;; Default global variables
(setq scroll-step 1
      scroll-margin 2
      word-wrap t
      large-file-warning-threshold nil
      use-short-answers t
      yes-or-no-prompt "y/n"
      pixel-scroll-precision-large-scroll-height 40.0
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      visible-bell nil)

(setq display-buffer-alist
      `(("^\\*R Dired"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . -1)
         (window-width . 0.33)
         (reusable-frames . nil))

        ("^\\*R"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (window-height . 0.38)
         (reusable-frames . nil))

        ("^\\magit"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (window-height . 0.38)
         (window-width . 0.45)
         (side . right)
         (reusable-frames . nil))

        ("^\\*TeX Help"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (window-height . 0.38)
         (reusable-frames . nil))

        ("^\\*vterminal"
         (display-buffer-reuse-window display-buffer-in-side-window)
	 (reusable-frames . nil))

        ("^\\*Go-Translate"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (window-height . 0.30)
	 (reusable-frames . nil))

        ("^\\*Help"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . 1)
         (window-width . 0.40)
	 (reusable-frames . nil))))

;;; Enable/disable builtin modes
(progn (scroll-bar-mode -1)
       (tool-bar-mode -1)
       (tooltip-mode -1)
       (set-fringe-mode 1)
       (menu-bar-mode -1)
       (column-number-mode)
       (global-display-line-numbers-mode t)
       (global-prettify-symbols-mode)
       (menu-bar--display-line-numbers-mode-relative)
       (blink-cursor-mode 1)
       (toggle-word-wrap 1)
       (recentf-mode 1)
       (put 'upcase-region 'disabled nil)
       (put 'downcase-region 'disabled nil)
       )

(add-to-list 'recentf-exclude '"~/.emacs.d/.cache/var/bookmark-default.el")

(load-file "~/.emacs.d/hide-mode-line.el")
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(add-hook 'after-init-hook
	  (lambda ()
	    (progn
	      (persp-switch "main")
	      (setq evil-normal-state-cursor 'box)
	      (setq evil-visual-state-cursor 'hollow)
	      (setq evil-replace-state-cursor 'hbar)
	      (recentf-mode 1)
	      (recentf-load-list)
	      (set-cursor-color "wheat")
	      (kill-buffer "*Async-native-compile-log*"))))

(dolist (func '(save-pdf-themed--mode-state
		))
  (add-hook 'kill-emacs-hook func))

(defun arkomacs-font-config ()
  (interactive "P")
  (defvar endless-font-size 130)
  (defvar endless-code-font "JetBrains Mono")
    ;;; Previous Font "Leckerli One" Princess Sofia
  (defvar endless-variable-pitch-font "Liberation Serif")

  (set-face-attribute 'default nil
		      :font endless-code-font
		      :height endless-font-size
		      :weight 'normal)

  (set-face-attribute 'fixed-pitch nil
		      :font endless-code-font
		      :height endless-font-size
		      :weight 'medium :slant 'normal)

  (set-face-attribute 'variable-pitch nil :font endless-variable-pitch-font :height 160 :weight 'regular)

  (variable-pitch-mode t)
  (custom-set-variables '(font-lock-support-mode 'tree-sitter-lock-mode)))

(arkomacs-font-config)

(defun arkomacs-org-font-setup ()
  ;;; Replace list hyphen with dot for `lists`
  (interactive)
  ;;; Org heading font scaling
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

(use-package blackout
  :straight (blackout :host github :repo "raxod502/blackout")
  :demand t)

(use-package no-littering
  :straight t)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;; Previous theme --> doom-oksolar-dark, 
(use-package doom-themes
  :straight t
  :demand t
  :init (load-theme 'modus-vivendi-deuteranopia t))

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

(defun split-elisp-repl ()
  (interactive)
  (split-window-below-with-height 15)
  (ielm)
  (set-window-dedicated-p (selected-window) t))

(defun split-sage-repl ()
  (interactive)
  (split-window-below-with-height 15)
  (sage-shell:run-sage "sage")
  (set-window-dedicated-p (selected-window) t))

(defun delete-window-and-kill-buffer ()
  (interactive)
  (kill-this-buffer)
  (evil-window-delete))

(defun split-vterm (height)
  "Split vterm"
  (interactive "nWindow height: ")
  (split-window-below-with-height height)
  (multi-vterm)
  (set-window-dedicated-p (selected-window) t))

(defun split-h-vterm ()
  (interactive)
  (split-vterm 10)
  (hide-mode-line-mode))

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
      (visual-fill-column-mode 2)
      )))

(defun insert-current-date ()
  (interactive)
  (insert (format-time-string "%b %d, %a")))

(defun insert-current-time ()
  (interactive)
  (insert (format-time-string "%H:%M")))

(defun wrap-- (m1)
  (interactive "P")
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
(defun wrap-dollar ()
  (interactive)
  (if (use-region-p)
      (progn
        (kill-region (region-beginning) (region-end))
        (insert "$")
        (yank)
        (insert "$"))
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

(defun current-filename ()
  "Current filename without extension."
  (file-name-sans-extension
   (file-name-nondirectory buffer-file-name)))


(defun sudo-find-file (file-name)
  "like find file, but opens the file as root using tramp"
  (interactive (list (read-file-name "file: " "/sudo::/")))
  (let ((tramp-file-name (expand-file-name file-name)))
    (find-file tramp-file-name)))
(defun move-to-prev-window ()
  (interactive)
  (evil-window-prev 1))

(use-package which-key
  :demand t
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.4))

(use-package which-key-posframe
  :demand t
  :after which-key
  :config
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-center)
  (which-key-posframe-mode))

(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol nil)
  (evil-want-C-d-scroll t)
  (evil-want-C-i-jump nil)
  (evil-move-cursor-back nil)
  (evil-move-beyond-eol nil)
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

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package general
  :demand t
  :init
  (general-evil-setup))

(general-define-key :states 'normal
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "zw" '(count-words :which-key "word-count"))

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
 "g/" 'evilnc-comment-or-uncomment-lines
 ";" 'ex-M)


;;; CTRL Maps
;; Global Keymaps
(general-define-key
 :keymaps '(override evil-treemacs-state-map)
 "C-w" 'evil-window-map
 "C-o" 'toggle-transparency
 "C--" 'text-scale-decrease
 "C-=" 'text-scale-increase
 "C-." 'evil-window-increase-width
 "C-," 'evil-window-decrease-width
 "C->" 'evil-window-increase-height
 "C-<" 'evil-window-decrease-height
 "C-t" '(mtt :which-key "Open Vterm")
 "M-x" 'ex-M
 )

(general-define-key
 :states '(normal insert visual)
 :keymaps '(override evil-treemacs-state-map)
 "C-<tab>" 'consult-buffer
 "<f5>" '(run-current-project :which-key "Run")
 "<f6>" '(run-project-in-term :which-key "Run Project in term")
 "C-q" 'quit-win-and-kill-buff
 "C-k" 'evil-scroll-line-up
 "C-j" 'evil-scroll-line-down
 "C-p" 'consult-yank-from-kill-ring)

(general-define-key
 :states 'insert
 :keymaps 'global
 "C-]" 'evil-end-of-visual-line
 "C-[" 'evil-beginning-of-visual-line)

(general-def 'insert
  "C-g" 'evil-normal-state
  "C-h" nil)

(general-def '(normal visual)
  "SPC" nil
  "m" nil)

(general-create-definer leader-key-SPC
  :keymaps '(override evil-treemacs-state-map)
  :states '(normal visual)
  :prefix "SPC")

(general-create-definer leader-key-m
  :states 'normal
  :prefix "m")

(general-create-definer leader-key-ctrl-c
  :states '(visual normal insert)
  :prefix "C-c")

(defun mjort ()
  (interactive)
  (funcall major-mode))

(leader-key-m
  :states '(normal visual)
  :keymaps 'override
  "t"  '(mjort :which-key "Toogle Major Mode")
  "m"  '(hide-mode-line-mode :which-key "Toogle Modeline"))

(leader-key-SPC
  "m"  '(consult-imenu :which-key "IMenu")
  "w"  '(evil-window-map :which-key "Window")
  "ww" '(set-window-width :which-key "Set Width")
  "wm" '(delete-window-and-kill-buffer :which-key "Set Width")
  "wi" '(set-window-height :which-key "Set Height")
  "w\\" '(evil-window-set-width :which-key "Set width full")
  "w-" '(evil-window-set-height :which-key "Set height full")
  "w C--" '(evil-window-set-height :which-key "Set height full")
  "a"  '(ace-select-window :which-key "Select Window")
  "qq" '(save-buffers-kill-terminal :which-key "Exit Emacs")
  "d"  '(kill-this-buffer :which-key "Kill Buffer")
  "e"  '(treemacs-select-window :which-key "Treemacs Toggle"))

(leader-key-SPC
  :prefix "SPC t"
  :wk "Toogle"
  "t" '(consult-theme :which-key "Choose theme")
  "c" '(display-time-mode :which-key "Display Time")
  "l" '(display-line-numbers-mode :which-key "Toogle line numbers")
  "h" '(hl-line-mode :which-key "Toogle line highlight")
  "b" '(display-battery-mode :which-key "Toogle Battery")
  "v" '(visual-fill-column-mode :which-key "Center Column")
  "d" '(elcord-mode :which-key "Discord status")
  "m" '(hide-mode-line-mode :which-key "Toogle Modeline"))

(defun open-books-from-books-dir ()
  (interactive)
  (consult-find "~/Bücher"))

(defun find-in-projects-dir ()
  (interactive)
  (consult-find "~/Projects"))

(leader-key-SPC
  :prefix "SPC f"
  :wk "File..."
  "s" '(save-buffer :which-key "Save Buffer")
  "g" '(sudo-find-file :which-key "Sudo find")
  "e" '(rename-file :which-key "Rename File")
  "d" '(delete-file :which-key "Delete File")
  "o" '(find-file :which-key "Open File")
  "w" '(find-file-other-window :which-key "Open File other in win")
  "t" '(consult-ripgrep :which-key "Find text in project")
  "p" '(find-in-projects-dir :which-key "Find projects")
  "b" '(open-books-from-books-dir :which-key "Open Books")
  "f" '(project-find-file :which-key "Find file in project")
  "r" '(recentf :which-key "Open Recent File"))

(leader-key-SPC
  :prefix "SPC g"
  :wk "Magit..."
  "s" '(magit-stage :which-key "Stage")
  "u" '(magit-unstage :which-key "Stage")
  "g" '(magit :which-key "Status")
  "d" '(magit-diff :which-key "Diff")
  "p" '(magit-push :which-key "Push")
  "P" '(magit-pull :which-key "Push")
  "c" '(magit-commit :which-key "Commit"))

(leader-key-SPC
  :prefix "SPC s"
  "s"'(swiper-isearch :which-key "Search...")
  "e"'(websearch :which-key "Websearch...")
  "g"'(google-translate-query-translate :which-key "Google Translate...")
  "t"'(gts-do-translate :which-key "Translate")
  "d"'(dictionary-search :which-key "Search word..."))

(leader-key-SPC
  :prefix "SPC c"
  "e" '(eval-last-sexp :which-key "Eval last sexp"))

(defun inspc ()
  (interactive)
  (insert " "))

(leader-key-SPC
  :prefix "SPC i"
  "d" '(insert-current-date :which-key "Insert Date")
  "'" '(wrap-quotes :which-key "Wrap Quotes")
  "[" '(wrap-sb :which-key "Wrap []")
  "9" '(wrap-rb :which-key "Wrap ()")
  "4" '(wrap-dollar :which-key "Wrap $")
  "]" '(wrap-cb :which-key "Wrap {}")
  "SPC" '(inspc :which-key "Insert Date")
  "t" '(insert-current-time :which-key "Insert Time")
  "e" '(emoji-insert :which-key "Insert Emoji"))

(leader-key-SPC
  :prefix "SPC h"
  "f" '(describe-function :which-key "Describe Function")
  "v" '(describe-variable :which-key "Describe Variable"))

(defun mtt ()
  (interactive)
  (multi-vterm)
  (hide-mode-line-mode))

(leader-key-SPC
  :prefix "SPC o"
  "t" '(split-h-vterm :which-key "Open Term")
  "j" '((lambda () (interactive) (find-file "~/Bücher/Personal/Journal.org")) :which-key "Open Journal")
  "c" '((lambda () (interactive) (find-file "~/.emacs.d/Config.org")) :which-key "Open Config")
  "b" '(eww :which-key "eww")
  "s" '(scratch-buffer :which-key "Open Scratch buffer")
  "e" '(eshell :which-key "Eshell"))

(leader-key-SPC
  :prefix "SPC or"
  "e" '(split-elisp-repl :which-key "Elisp REPL")
  "s" '(split-sage-repl :which-key "Sage REPL"))

(leader-key-SPC
  :prefix "SPC b"
  :which-key "Buffer"
  "l" '(evil-switch-to-windows-last-buffer :which-key "Last Buffer")
  "k" '(kill-this-buffer :which-key "Kill Buffer")
  "f" '(switch-to-buffer :which-key "Switch Buffer")
  "w" '(switch-to-buffer-other-window :which-key "Switch Buffer in other win")
  "p" '(consult-project-buffer :which-key "Project Buffers")
  "d" '(kill-buffer :which-key "Find & Kill"))

(general-def '(normal insert) 'override
  "C-b a" '(bookmark-set :whick-key "Add Bookmark")
  "C-b f" '(bookmark-jump :whick-key "Open Bookmark")
  "C-b d" '(bookmark-delete :whick-key "Delete Bookmark"))

(leader-key-SPC
  :prefix "SPC p"
  "f" '(project-find-file :which-key "Find file")
  "b" '(project-switch-to-buffer :which-key "Switch Buffer")
  "p" '(project-switch-project :which-key "Switch Project")
  "k" '(project-kill-buffers :which-key "Kill Project Buffers")
  "c" '(set-project-run-cmd :which-key "Set run/build cmd"))

(use-package doom-modeline
  :demand t
  :init
  (setq doom-modeline-height                 26
        doom-modeline-buffer-file-name-style 'truncate-from-project
	doom-modeline--vcs-icon              " "
        display-time-format                  " %H:%M:%S "
        display-time-interval                1
	doom-modeline-icon                   t
        doom-modeline-buffer-encoding nil)
  (display-time-mode 1)
  (doom-modeline-mode 1))

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
    major-mode vcs " "))

(add-hook 'treemacs-mode-hook #'hide-mode-line-mode)

(use-package async
  :straight t
  :defer t
  :init
  (dired-async-mode 1))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

(use-package evil-nerd-commenter
  :straight t)

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

;; (use-package emojify
;;     :hook (after-init . global-emojify-mode))

(use-package undo-fu
  :demand t)

(use-package undo-fu-session
  :demand t
  :config
  (global-undo-fu-session-mode))

(use-package smartparens
  :demand t
  :config
  (smartparens-global-mode))

;; (use-package beacon
;;   :straight t
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

;; (use-package tree-sitter
;;   :config
;;   (global-tree-sitter-mode))

;; (use-package tree-sitter-langs)

(use-package persp-mode
  :demand t
  :config
  ;; (setq persp-keymap-prefix "C-a")
  (persp-mode))

(general-define-key
 ;; :states '(normal visual insert)
 :keymap 'override
  "<f1>" 'persp-switch
  "C-a" 'persp-key-map
  "C-0" 'persp-next
  "C-9" 'persp-prev)

(use-package company
  :demand t
  :config
  (global-company-mode)
  (setq ispell-dictonary "en_US"
	company-ispell-dictonary ispell-dictonary)
  (add-to-list 'company-backends 'company-ispell)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-common-or-cycle)
        ("<backtab>" . company-select-previous)
        ("C-j" . company-complete-common-or-cycle)
        ("C-p" . company-select-previous))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))


(use-package company-box
  :init
  (setq company-box-scrollbar nil
	company-box-tooltip-maximum-width 140)
  :hook (company-mode . company-box-mode))

(use-package company-auctex
  :config
  (company-auctex-init))

;; (use-package company-org-block
;;   :custom
;;   (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
;;   :hook ((org-mode . (lambda ()
;;                        (setq-local company-backends '(company-org-block))
;;                        (company-mode +1)))))

(defun kb/basic-remote-try-completion (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-try-completion string table pred point)))

(defun kb/basic-remote-all-completions (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-all-completions string table pred point)))

(use-package vertico
  :demand t                             ; Otherwise won't get loaded immediately
  :straight (vertico :files (:defaults "extensions/*") ; Special recipe to load extensions conveniently
                     :includes
		     (vertico-indexed
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
  (:keymaps 'vertico-map
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
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
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
                 cand)))
  (use-package vertico-posframe
    :config
    (setq vertico-posframe-poshandler 'posframe-poshandler-frame-top-center)
    :init
    (setq vertico-multiform-commands
          '((consult-line
             posframe
             (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
             (vertico-posframe-border-width . 10)
             (vertico-posframe-fallback-mode . vertico-buffer-mode))
            (t posframe)))
    (vertico-posframe-mode 1)))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package savehist
  :demand t
  :init
  (savehist-mode 1))

(defun dw/get-project-root ()
  (when (fboundp '(project-root (project-current t)))
    (project-root (project-current t))))

(use-package consult
  :straight t
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-i" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package treemacs
  :demand t
  :config
  (setq treemacs-deferred-git-apply-delay        0.5
        treemacs-directory-name-transformer      #'identity
        treemacs-display-in-side-window          t
        treemacs-eldoc-display                   'simple
        treemacs-file-event-delay                5000
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
        treemacs-tag-follow-delay                0.1
        treemacs-wide-toggle-width               70
        treemacs-width                           35
        treemacs-width-increment                 1
        treemacs-workspace-switch-cleanup        nil))

;; (use-package treemacs-projectile)

(use-package treemacs-nerd-icons
  :demand t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil
  :demand t)

(use-package project-treemacs
  :demand t
  :config
  (project-treemacs-mode 1)
  (treemacs-project-follow-mode 1)
  (setq treemacs--project-follow-delay 0.5))

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
	lsp-diagnostics-provider 'flycheck)
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
  :keymaps 'lsp-ui-doc-frame-mode-map
  :states 'override
  "q" 'lsp-ui-doc-unfocus-frame
  "<escape>" 'lsp-ui-doc-unfocus-frame)

(general-define-key
 :keymaps 'lsp-mode-map
 :states 'normal
 "gd" 'lsp-find-definition
 "gr" 'lsp-find-references
 "K"     'lsp-describe-thing-at-point)

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
  :straight t
  :init
  (global-flycheck-mode))

(use-package ripgrep)

(use-package rust-mode
  :straight t
  :hook 
  (rust-mode . lsp-deferred)
  :config
  (setq rust-format-on-save t
	lsp-rust-analyzer-proc-macro-enable t))

(add-hook 'rust-mode-hook
        (lambda () (setq indent-tabs-mode nil)))

(use-package rust-playground)

(use-package modern-cpp-font-lock)

(setq lsp-clangd-binary-path "/bin/clangd")
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
(add-hook 'c++-mode-hook 'lsp)

(with-eval-after-load 'lsp-mode
  (require 'dap-cpptools))

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

(use-package json-mode
  :straight t)

(use-package yaml-mode)

(use-package qml-mode)

(use-package dart-mode
 :config
 :hook (dart-mode . lsp))

(use-package lsp-dart)

(use-package python-mode
  :straight t
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

(use-package ess
  :straight t)

(use-package julia-mode)

(use-package ein
  :config
  (setq *ein:file-buffername-template* "%s"
	ein:tb-buffer-name-template "%s")
  )

(setq ein:output-area-inlined-images t
    ob-ein-inline-image-directory "~/.emacs.d/.cache/ob-ein-images")

;; (leader-key-m
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

(use-package lua-mode)

(use-package nim-mode
    :straight t
    :hook (nim-mode . lsp))

(use-package web-mode
  :straight t
  :gfhook #'lsp
  :mode (("\\.[tj]sx\\'" . web-mode)
         ("\\.[tj]s\\'" . web-mode)
         ("\\.html\\'" . web-mode)))

(use-package lsp-tailwindcss
  :straight (:type git :host github :repo "merrickluo/lsp-tailwindcss"))

(use-package emmet-mode)

(use-package auctex
  :custom
  (flycheck-tex-lacheck-executable "/bin/lacheck")
  (TeX-source-correlate-method 'synctex)
  (TeX-clean-confirm nil)
  (TeX-source-correlate-start-server nil)
  :hook
  ((LaTeX-mode . prettify-symbols-mode)
   (LaTeX-mode . TeX-source-correlate-mode)
   (TeX-mode   . lsp)))

(use-package lsp-latex)

(use-package cdlatex
  :hook
  (org-mode . org-cdlatex-mode))
(setq latex-delete-tex-log t)

;;; TODO: This doesn't work properly --> It looks for the pdf first and then compiles
;;; probably because `TeX-command` is async or it's using something executes async function
;;; and for some reason it runs after the pdf is opened
;;; I have to recompile it twice to sync the pdfview.
;;; How it should:
;;; Compile the latex to pdf -> look if pdf file is present -> if pdf file is opened in then sync the pdf.
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
				(compile-latex))))))

    (add-hook 'TeX-after-compilation-finished-functions
              (lambda (proc)
		(refresh-pdfview-buffer)))

    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))))

(setq org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}
\\usepackage[margin=1in]{geometry}
\\usepackage{amsmath}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")

(setq org-highlight-latex-and-related '(native latex script entities)
      tempo-template-org-export-latex '("\\begin{LARGE}" nil '> n p n "\\end{LARGE}" >))

(leader-key-m
 :keymaps 'TeX-mode-map
 :states '(normal visual)
 "c" '(compile-latex :which-key "Compile Latex")
 "s" '(toggle-latex-compile-on-save :which-key "Compile on save")
 "v" '(open-latex-pdf :which-key "View pdf"))

(use-package ob-sagemath
  :config
  (setq sage-shell:input-history-cache-file
	(concat user-emacs-directory "var/sage_history")
	sage-shell:check-ipython-version-on-startup nil
	org-babel-default-header-args:sage '((:session . t)
					     (:results . "drawer"))))

(add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode))

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets
	     :type git
	     :host github
	     :repo "hlissner/doom-snippets"
	     :files ("*.el" "*")))

(defun arkomacs-org-mode-setup ()
  (setq org-src-tab-acts-natively     t
        org-src-preserve-indentation  t
	org-pretty-entities           t
        org-src-fontify-natively      t)
  (org-indent-mode)
  (org-overview)
  (display-line-numbers-mode 0)
  (variable-pitch-mode t)
  (hs-minor-mode t)
  (yas-minor-mode)
  (visual-line-mode 1))

(use-package org
  :demand
  :hook (org-mode . arkomacs-org-mode-setup)
  :config
  (setq org-ellipsis " ↴"
        org-hide-emphasis-markers t
        org-agenda-files '("~/Bücher/Personal/Tasks.org")
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
	org-imenu-depth 4

	org-startup-with-inline-images t
	org-startup-with-latex-preview t)

  (arkomacs-org-font-setup)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(use-package org-modern
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
  :commands (org-modern-mode org-modern-agenda)
  :init (global-org-modern-mode))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
 (setq org-superstar-cycle-headline-bullets nil)
 (setq org-superstar-headline-bullets-list
	'("◉" ("◈" ?◈) "○" "▷")))

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

(leader-key-ctrl-c
  :keymaps 'org-mode-map
  :states '(visual normal)
  "C-i" '(org-indent-block :which-key "Org indent block")
  "i" '(org-indent-block :which-key "Org indent block"))
(leader-key-m
  :keymaps 'org-mode-map
  :states 'visual
  "c" '(arkomacs-org-wrap-code :which-key "Wrap Code")
  "b" '(arkomacs-org-wrap-bold :which-key "Wrap Bold")
  "i" '(arkomacs-org-wrap-italics :which-key "Wrap italics")
  "x" '(arkomacs-org-wrap-strike :which-key "Stike Seletion")
  "v" '(arkomacs-org-wrap-verbatim :which-key "Wrap Verbatim")) 

(leader-key-m
  :keymaps 'org-mode-map
  :states '(visual normal)
  "r" '(org-ctrl-c-ctrl-c :which-key "Run Code block")
  "o" '(consult-org-heading :which-key "Outline")
  "d" '(org-latex-preview :which-key "Run Code block")
  "l" '(org-insert-link :which-key "Insert Link"))

;;; Org Babel setup
(setq org-babel-C++-compiler "clang++"
      org-preview-latex-image-directory (concat user-emacs-directory "etc/org-latex/")
      org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (ein . t)
   (lisp . t)
   (shell . t)
   (latex . t)
   (sagemath . t)
   (julia . t)
   (C . t)
   (lua . t)))


(require 'org-tempo)
(progn
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("sm" . "src sage"))
  (add-to-list 'org-structure-template-alist '("la" . "src latex"))
  (add-to-list 'org-structure-template-alist '("mp" . "src sage :results file"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("ein" . "src ein-python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("jl" . "src julia"))
  (add-to-list 'org-src-lang-modes '("conf-unix" . conf-unix)))

(progn
  (add-hook 'org-babel-after-execute-hook
	    (lambda ()
              (interactive)
              (clear-image-cache)
              (org-display-inline-images))))

(setq org-babel-default-header-args:latex
      '((:results . "raw")
        (:exports . "results")
        ;; (:fit . t)
        (:imagemagick . t)
        ;; (:eval . "no-export")
        ;; (:headers . ("\\usepackage{\\string~/.emacs.d/common}"))
        ))

;; (setq org-babel-default-header-args:python
;;       '((:kernel . "ipython")
;;         (:results . "raw")
;;         (:async . "yes")
;;         (:pandoc . "t")
;;         (:exports . "both")
;;         (:cache .   "no")
;;         (:noweb . "no")
;;         (:hlines . "no")
;;         (:tangle . "no")
;;         (:eval . "never-export")))

;; (setq org-babel-default-header-args:jupyter-julia
;;       '((:async . "yes")
;;         (:session . "jupyter-julia")
;;         (:kernel . "julia")
;;         (:exports . "both")
;;         (:eval . "never-export")))

(defun arkomacs-org-babel-tangle-config ()
  (interactive)
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda ()
			   (add-hook 'after-save-hook
				     #'arkomacs-org-babel-tangle-config)))

(use-package go-translate
  :config
  (setq gts-translate-list '(("de" "en") ("en" "de")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-google-engine))
         :render (gts-buffer-render))))

(use-package google-translate)

(use-package websearch)

(use-package elcord
  :config
  (setq elcord-refresh-rate 5))

(defvar pdf-themed--mode-state-file
  (concat user-emacs-directory
	  "var/pdf-themed-mode-state"))

(defvar pdf-themed--mode nil)
(use-package pdf-tools
  :demand t
  :config
  (use-package saveplace-pdf-view
    :demand t
    :config
    (save-place-mode 1))
  (setq pdf-themed--mode (string-to-number (f-read pdf-themed--mode-state-file)))
  (pdf-tools-install))

;;; pdfview-mode hooks

(dolist (mode '(doc-view-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda ()
                   (progn
                     (display-line-numbers-mode 0)
                     ))))


(add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)
(add-hook 'pdf-view-mode-hook
	  (lambda ()
	    (progn
	      (if (= 1 pdf-themed--mode)
		  (pdf-view-midnight-minor-mode))
	      (blink-cursor-mode -1)
	      (setq-local evil-normal-state-cursor '(bar . 0)
			  visible-cursor nil))))

(add-hook 'pdf-view-midnight-minor-mode-hook
	  (lambda ()
	    (progn
	      (if pdf-view-midnight-minor-mode
		  (setq pdf-themed--mode 1)
		(setq pdf-themed--mode -1)))))

(defun save-pdf-themed--mode-state ()
  "Save pdf-themed--mode state"
  (interactive)
  (f-write (number-to-string pdf-themed--mode) nil pdf-themed--mode-state-file))

;;; --- PdfView-mode functions ---
(add-hook 'pdf-outline-buffer-mode-hook
	  (lambda ()
	    (hl-line-mode)))

(defun pdf-outlf ()
  (interactive)
  (setq pdf-outline-buffer-exists nil)
  (setq pdf-outline-buffer-name
	(format "*Outline %s*" (file-name-nondirectory buffer-file-name)))

  ;;; Before opening the outline

  (dolist (buffer (buffer-list))
    (if (buffer-name buffer)
	(if (string-match "*Outline*" (buffer-name buffer))
	    (progn
	      (setq pdf-outline-buffer-exists t)
	      (if (not (string= pdf-outline-buffer-name (buffer-name buffer)))
		  (progn
		    (setq pdf-outline-buffer-exists nil)
		    (kill-buffer (buffer-name buffer))))))))

    ;;; 

  (pdf-outline)
  (pdf-outline-move-to-current-page)

    ;;; After opening the outline
  (setq cursor-type '(bar . 0))
  (setq-local evil-normal-state-cursor '(bar . 0))
  (if (not pdf-outline-buffer-exists) (set-window-width 50)))


(defun pdf-outl ()
  (interactive)
  (pdf-outlf)
  (evil-scroll-line-to-center nil)
  (set-window-dedicated-p (selected-window) t))

(defun follow-current-pdf-outline ()
  (interactive)
  (pdf-outline-display-link)
  (pdf-outline-select-pdf-window))

(defun pdf-outline-qui ()
  (interactive)
  (kill-this-buffer)
  (quit-window))

(defun pdf-outline-kill ()
  (interactive)
  (dolist (buffer (buffer-list))
    (if (buffer-name buffer)
	(if (string-match "*Outline*" (buffer-name buffer))
	    (kill-buffer (buffer-name buffer))))))

(defun open-thought-bubble ()
  (interactive)
  (split-window-horizontally)
  (windmove-right)
  (set-window-width 90)
  (find-file "~/Bücher/Personal/ThoughtBubble.org"))

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
  "x" 'pdf-outline-kill)

(general-def 'normal 'pdf-outline-buffer-mode-map
  "f" 'follow-current-pdf-outline
  "o" 'outline-toggle-children
  "l" 'outline-cycle
  "h" 'outline-up-heading
  "J" 'outline-next-heading
  "K" 'outline-previous-heading
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

(use-package dictionary
  :config
  (setq dictionary-use-single-buffer t))

(use-package nov
  :init
  (setq nov-text-width t
        visual-fill-column-center-text t
        nov-text-width nil
        nov-unzip-program (executable-find "bsdtar")
	nov-unzip-args '("-xC" directory "-f" filename))

  (add-hook 'nov-mode-hook
	    (lambda ()
	      (setq-local visual-fill-column-width 90)
	      (visual-line-mode)
	      (hl-line-mode)
	      (visual-fill-column-mode)))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(general-def 'normal 'nov-mode-map
  "K" 'nov-previous-document
  "J" 'nov-next-document)

(use-package evil-multiedit)

(leader-key-m
  :states '(visual normal)
  "n" '(evil-multiedit-match-and-next :which-key "Mulitple Cursor match next")
  "p" '(evil-multiedit-match-and-prev :which-key "Mulitple Cursor match next")
  "a" '(evil-multiedit-match-all :which-key "Mulitple Cursor match next"))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "fish"                       ;; Set this to customize the shell to launch
        vterm-max-scrollback 200000))
(use-package multi-vterm
  :straight t)

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

;; (leader-key-SPC
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

(defun arkomacs-org-mode-visual-fill()
  (setq visual-fill-column-width 130
	visual-fill-column-enable-sensible-window-split t
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . arkomacs-org-mode-visual-fill))
