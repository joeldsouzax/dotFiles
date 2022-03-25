;; --------------------------
;; VARIABLE DECLARATION
;; --------------------------

(defvar jd/default-font-size 180)
(defvar jd/default-variable-font-size 210)


;; --------------------------
;; USER DECLARATION
;; --------------------------

(setq user-full-name "Joel D'Souza")
(setq user-mail-address "joeldsouzax@gmail.com")



;; --------------------------
;; LOAD EMACS CONFIG FOLDER SCRIPTS
;; --------------------------

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY"
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
	   (fullpath (concat directory "/" path))
	   (isdir (car (cdr element)))
	   (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
	(load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
	(load (file-name-sans-extension fullpath)))))))

(load-directory "~/.emacs.d/config")

;; --------------------------
;; NO LITTERING
;; --------------------------

(use-package no-littering)

;; --------------------------
;; FONT Definitions
;; --------------------------


(set-face-attribute 'default nil :font "Menlo" :height jd/default-font-size)

;;fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Menlo" :height jd/default-font-size)

;; set the variable-pitch-face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height jd/default-variable-font-size :weight 'regular)


;; --------------------------
;; BASIC CONFIG
;; --------------------------


(setq inhibit-startup-message t) ;; remove the default emacs startup message
(scroll-bar-mode -1)             ;; disablE visible scrollbar
(tool-bar-mode -1)               ;; disable the toolbar
(tooltip-mode -1)                ;; disable the tooltips
(set-fringe-mode 10)             ;; give some breathing room :D
(menu-bar-mode -1)               ;; disable the menu bar
(setq visible-bell 1)            ;; set up bthe visible alert bell
(global-hl-line-mode +1)
(save-place-mode t)
(global-auto-revert-mode t)

;; trying to disable the flickering

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(add-hook 'after-init-hook
          (lambda () (run-with-timer 5 nil #'set-cursor-color "SystemRedColor")))


;; i like to start my emacs with a view of my todos, org-agenda

(defun emacs-startup-screen ()
  "Display the weekly org-agenda and all todos."
  (org-agenda nil "d"))
(add-hook 'emacs-startup-hook #'emacs-startup-screen)


;; --------------------------
;; BACKUP CONFIG
;; --------------------------

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old
    )

;; --------------------------
;; PACKAGE CONFIG
;; --------------------------

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


(unless (package-installed-p 'use-package)
  (package-install 'use-package))                                                   ;; Initialize use-package on non linux platforms.

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t)




;; --------------------------
;; BENCHMARK STARTUP CONFIG
;; --------------------------

(defun jd/display-startup-time ()
  (message "⏱ Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'jd/display-startup-time)

;; --------------------------
;; GARBAGE COLLECTION CONFIG
;; --------------------------

(setq gc-cons-threshold 10000000)

;; restore after startup

(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold 10000000)
	    (message "gc-cons-threshold restored to %S"
		     gc-cons-threshold)))


;; --------------------------
;; BEACON MODE CONFIG
;; ----------------------

(use-package beacon
  :defer 2
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-color "#f222ff"))

;; --------------------------
;; ALL-ICONS CONFIG
;; ----------------------

(use-package all-the-icons)

;; --------------------------
;; MODELINE CONFIG
;; ----------------------

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

(setq doom-modeline-height 25)
(setq doom-modeine-bar-width 3)
(setq doom-modeline-lsp t)
(setq doom-modeline-project-detection 'project)

;; --------------------------
;; FULL-SCREEN CONFIG
;; --------------------------

(add-to-list 'default-frame-alist '(fullscreen . maximized))                      ;; default window of emacs would be fullscreen

;; --------------------------
;; YES-NO  CONFIG
;; --------------------------


(fset 'yes-or-no-p 'y-or-n-p)

;; --------------------------
;; MAC KEY REMAPPING CONFIG
;; --------------------------

(setq mac-command-modifier 'meta)                                                  ;; changing meta in mac to command key

;; --------------------------
;; LINE CONFIG
;; --------------------------

(column-number-mode)
(global-display-line-numbers-mode t)                                                ;; line numbers enabled.
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))                         ;; disable line numbers for some modes.

(setq-default highlight-tabs 1)                                                     ;; highlight tabs 


;; --------------------------
;; THEME CONFIG
;; --------------------------

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

;; --------------------------
;; SWIPER CONFIG
;; --------------------------

(use-package swiper)

;; --------------------------
;; IVY  CONFIG
;; --------------------------

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
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


;; --------------------------
;; COUNSEL  CONFIG
;; --------------------------

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;; --------------------------
;; RAINBOW-DELIMITERS CONFIG
;; --------------------------

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))                                      ;; rainbow delimiters only on programming modes.

;; --------------------------
;; WHICH KEY CONFIG
;; --------------------------

(use-package which-key
  :defer 0.1
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))


;; --------------------------
;; PROJECTILE CONFIG
;; --------------------------
;; for working on projects related to programming

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/dev/code")
    (setq projectile-project-search-path '("~/dev/code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; --------------------------
;; COUNSEL-PROJECTILE CONFIG
;; --------------------------

(use-package counsel-projectile
  :after projectile
  :bind (("C-M-p" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))

;; --------------------------
;; MAGIT CONFIG
;; --------------------------

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; --------------------------
;; EMOJIFY CONFIG
;; --------------------------

(use-package emojify
  :defer 2
  :config
  (global-emojify-mode))


;; --------------------------
;; ORG-MODE CONFIG
;; --------------------------


(defun jd/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (push '("[ ]" . "🔳") prettify-symbols-alist)
  (push '("[X]" . "☑") prettify-symbols-alist)
  (push '("[-]" . "◾") prettify-symbols-alist)
  (push '("#+BEGIN_SRC" . "λ") prettify-symbols-alist)
  (push '("#+END_SRC" . "λ") prettify-symbols-alist)
  (push '("#+begin_src" . "λ") prettify-symbols-alist)
  (push '("#+RESULTS:" . "▼▼") prettify-symbols-alist)
  (push '("#+results:" . "▼▼") prettify-symbols-alist)
  (push '("#+BEGIN_QUOTE" . "\"") prettify-symbols-alist)
  (push '("#+END_QUOTE" . "\"") prettify-symbols-alist)
  (push '("#+begin_quote" . "\"") prettify-symbols-alist)
  (push '("#+end_quote" . "\"") prettify-symbols-alist)
  (push '("#+end_src" . "λ") prettify-symbols-alist)
  (push '("#(ref:" . "(") prettify-symbols-alist)
  (prettify-symbols-mode))




(use-package org
  :hook (org-mode . jd/org-mode-setup)
  :config
  (setq org-agenda-start-with-log-mode t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-startup-with-inline-images t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  ;; config for agenda files
  (setq org-agenda-files
	'("~/dev/src/github.com/organize/tasks.org"
	  "~/dev/src/github.com/organize/habits.org"))
  
  (setq org-ellipsis "  ▼"
	org-hide-emphasis-markers t)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANCEL(k@)")))
  ;; manual tags list
  (setq org-tag-alist
	'((:startgroup)
	  ;; put mutually exclusive tags here.
	  (:endgroup)
	  ("@errand" . ?E)
	  ("@home" . ?H)
	  ("@work" . ?W)
	  ("agenda" . ?a)
	  ("planning" . ?a)
	  ("publish" . ?P)
	  ("batch" . ?b)
	  ("note" . ?n)
	  ("idea" . ?i)
	  ("thinking" . ?t)
	  ("recurring" . ?r)))
  ;; refile tasks around
  (setq org-refile-targets
	'(("archive.org" :maxlevel . 1)
	  ("tasks.org" :maxlevel . 1)))
  ;;save org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  ;; custom agenda views
  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
	    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
	  ("n" "Next Tasks"
	   ((todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))
	  ("W" "Work Tasks" tags-todo "+work")
	  ;; low effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))
	  ("w" "Workflow Status"
	   ((todo "WAIT"
		  ((org-agenda-overriding-header "Waiting on External")
		   (org-agenda-files org-agenda-files)))
	    (todo "REVIEW"
		  ((org-agenda-overriding-header "In Review")
		   (org-agenda-files org-agenda-files)))
	    (todo "PLAN"
		  ((org-agenda-overriding-header "In Planning")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "BACKLOG"
		  ((org-agenda-overriding-header "Project Backlog")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "READY"
		  ((org-agenda-overriding-header "Ready for Work")
		   (org-agenda-files org-agenda-files)))
	    (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "COMPLETED"
		  ((org-agenda-overriding-header "Completed Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "CANCEL"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-fiels)))))))
  (setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp "~/dev/src/github.com/organize/tasks.org" "Backlog")
	   "* TODO %?\n %U\n %a\n %i" :empty-lines 1)
	  ("j" "Journal Entries")
	  ("jj" "Journal" entry
	   (file+olp+datetree "~/dev/src/github.com/organize/journal.org")
	   "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
	   :clock-in :clock-resume
	   :empty-lines 1)
	  ("jm" "Meeting" entry
	   (file+olp+datetree "~/dev/src/github.com/organize/journal.org")
	   "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
	   :clock-in :clock-resume
	   :empty-lines 1)
	  ("w" "Workflows")
	  ("we" "Checking Email" entry (file+olp+datetree "~/dev/src/github.com/organize/journal.org")
	   "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1))))
  
	    

;; org bullets package to style the org heading, sub heading etc


(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("🚀" "☄" "🌝" "☀" "💻" "💣" "🏹" "🗡" "🛡")))

;; change font sizes according to the level of type
;; figure out the best one and change it later TODO:


(with-eval-after-load 'org-faces (dolist (face '((org-level-1 . 1.2)
						 (org-level-2 . 1.1)
						 (org-level-3 . 1.05)
						 (org-level-4 . 1.0)
						 (org-level-5 . 1.1)
						 (org-level-6 . 1.1)
						 (org-level-7 . 1.1)
						 (org-level-8 . 1.1)))
				   (set-face-attribute 'org-block-begin-line nil
						       :background "#141414"
						       :weight 'bold
						       :foreground "#d30cb8"
						       :slant 'italic
						       :extend t)
				   (set-face-attribute 'org-block-end-line nil
						       :background "#141414"
						       :foreground "#d30cb8"
						       :weight 'bold
						       :slant 'italic
						       :extend t)
				   (set-face-attribute 'org-block nil
						       :background "#050505"
						       :inherit 'fixed-pitch
						       :extend t)
				   (set-face-attribute 'org-code nil
						       :inherit '(shadow fixed-pitch))
				   (set-face-attribute 'org-special-keyword nil
						       :inherit '(font-lock-commenbt-face fixed-pitch))
				   (set-face-attribute 'org-table nil
						       :inherit '(shadow fixed-pitch))
				   (set-face-attribute 'org-formula nil
						       :inherit 'fixed-pitch)
				   (set-face-attribute 'org-verbatim nil
						       :inherit '(shadow fixed-pitch))
				   (set-face-attribute 'org-meta-line nil
						       :inherit '(font-lock-comment-face fixed-pitch))
				   (set-face-attribute 'org-checkbox nil
						       :inherit 'fixed-pitch)
				   (set-face-attribute 'line-number nil
						       :inherit 'fixed-pitch)
				   (set-face-attribute 'line-number-current-line nil
						       :inherit 'fixed-pitch)
				   (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))))


;; replace hyphen - with some thing better

(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "➥"))))))



(defface org-bold
  '((t :foreground "#d2268b"
       :background "#2e2e2e"
       :weight bold
       ))
  "Face for org-mode bold."
  :group 'org-faces )

(setq org-emphasis-alist
  '(("*" ;; (bold :foreground "Orange" )
     org-bold)
    ("/" (italic :foreground "#9B00E8"))
    ("_" underline)
    ("=" ;; (:background "maroon" :foreground "white")
     org-verbatim verbatim)
    ("~" ;;(:background "deep sky blue" :foreground "MidnightBlue")
     org-code verbatim)
    ("+" (:strike-through t))))




;; --------------------------
;; ORG LATEX CONFIG
;; --------------------------

(use-package auctex
  :after org-mode
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))



;; --------------------------
;; ORG ROAM CONFIG
;; --------------------------

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/dev/src/github.com/notes"))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
	 ("C-c n r" . org-roam-node-random)
	 (:map org-mode-map
	       (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))




;; org roam ui

(use-package org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; --------------------------
;; VISUAL FILL ORG  CONFIG
;; --------------------------


(defun jd/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . jd/org-mode-visual-fill))


;; --------------------------
;; ORG-BABEL CONFIG
;; --------------------------


;; setting up ditaa
(setq org-ditaa-jar-path "~/.emacs.d/ditaa0_9.jar")


;; org babel typescript

(use-package ob-typescript
  :after org)


(defun jd/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ditaa . t)
     (js . t)
     (C . t)
     (typescript . t)
     (org . t)
     (css . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  (add-hook 'org-babel-after-execute-hook 'jd/display-inline-images 'append)
  (setq org-edit-src-content-indentation 0
	org-src-tab-acts-natively t
	org-src-preserve-indentation t)
  (setq org-babel-results-keyword "results"))




;; --------------------------
;; ORG CODE TEMPLATE  CONFIG
;; --------------------------


(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src C++"))
  (add-to-list 'org-structure-template-alist '("css" . "src css"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("js" . "src js")))

;; --------------------------
;; ORG AUTO TANGLE CONFIG
;; --------------------------
;; Automatically tangle emacs.org config file when we save it.

(defun jd/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
		      (expand-file-name user-emacs-directory))
    ;; dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jd/org-babel-tangle-config)))


;; --------------------------
;; LSP MODE  CONFIG
;; --------------------------

;; lsp header breadcrumb

(defun jd/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))


(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . jd/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  :config
  (lsp-enable-which-key-integration t))

;; lsp ui config
(use-package lsp-ui
  :hook(lsp-mode . lsp-ui-mode))

;; lsp tree mode
(use-package lsp-treemacs
  :after lsp)

;; lsp ivy
(use-package lsp-ivy
  :after lsp)


;; --------------------------
;; TYPESCRIPT CONFIG
;; --------------------------

(use-package typescript-mode
  :mode "\\.\\(ts\\|js\\)\\'"
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode)))

(use-package tree-sitter
  :ensure t
  :hook ((typescript-mode . tree-sitter-hl-mode)
	 (typescript-tsx-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))


;; --------------------------
;; HASKELL CONFIG
;; --------------------------

;; (use-package lsp-haskell)

;; (require 'lsp)
;; (require 'lsp-haskell)
;; ;; Hooks so haskell and literate haskell major modes trigger LSP setup
;; (add-hook 'haskell-mode-hook #'lsp)
;; (add-hook 'haskell-literate-mode-hook #'lsp)

;; --------------------------
;; PROGRAMMING CONFIG
;; --------------------------

(use-package smartparens
  :diminish smartparens-mode ;; Do not show in modeline
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t) ;; These options can be t or nil.
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t)
  :custom-face
  (sp-show-pair-match-face ((t (:foreground "White"))))) ;; Could also have :background "Grey" for example.



(use-package rainbow-mode
  :after prog-mode
  :config
  (setq rainbow-x-colors nil))


(add-hook 'prog-mode 'electric-pair-mode)


;; --------------------------
;; LOCAL NODE-MODULES CONFIG
;; --------------------------


(use-package add-node-modules-path
  :defer t
  :hook (((typescript-mode json-mode js2-mode web-mode) . add-node-modules-path)))


;; --------------------------
;; PRETTIER CONFIG
;; --------------------------

(defun jd/use-prettier-if-config-exists-in-project-root ()
  (let* ((package-root (locate-dominating-file
                        (or (buffer-file-name) default-directory)
                        "package.json"))
         (package-file (and package-root (expand-file-name "package.json" package-root)))
         (grep-prettierrc (concat "grep prettier" package-file))
         ;; ↓ this needs to be fixed
         (prettierrc-embedded (not (string= "" (shell-command-to-string grep-prettierrc))))
         ;; ↑ this needs to be fixed
         (prettierrc (and package-root (file-exists-p (expand-file-name ".prettierrc" package-root))))
         (prettierrc-json (and package-root (file-exists-p (expand-file-name ".prettierrc.json" package-root))))
         (prettierrc-js (and package-root (file-exists-p (expand-file-name ".prettierrc.js" package-root))))
         (prettierrc-config-js (and package-root (file-exists-p (expand-file-name ".prettierrc.config.js" package-root))))
         (prettier-config-p (not (eq nil (or prettierrc-embedded prettierrc prettierrc-json prettierrc-js prettierrc-config-js)))))
    (when prettier-config-p (prettier-js-mode))))


(use-package prettier-js
  :defer t
  :diminish prettier-js-mode
  :hook (((typescript-mode json-mode ) . jd/use-prettier-if-config-exists-in-project-root)))


;; --------------------------
;; Company MODE CONFIG
;; --------------------------

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))


(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background , (color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit front-lock-constant-face))))))


;; --------------------------
;; COMPANY BOX CONFIG
;; --------------------------


(use-package company-box
  :after company
  :ensure t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t)
  (setq x-gtk-resize-child-frame 'hide))


;; --------------------------
;; SHELL ENVIRONENT CONFIG
;; --------------------------

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;; --------------------------
;; DIRED CONFIG
;; --------------------------

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")
	   (insert-directory-program "gls" dired-use-ls-dired t)))


(use-package dired-single
  :commands (dired dired-jump))


(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :init (setq all-the-icons-dired-monochrome nil))


(use-package dired-open
  :commands (dired dired-jump)  
  :config
  (setq dired-open-extension '(("png" . "feh")
			       ("mkv" . "mpv"))))


;; --------------------------
;; YARN.el CONFIG
;; --------------------------

(global-set-key (kbd "C-c y i") 'yarn-init)
(global-set-key (kbd "C-c y a") 'yarn-add)
(global-set-key (kbd "C-c y d") 'yarn-add-dev)


;; ------------------------------------------------------------------------------;;
;; AUTO CONFIG + AUTO GENERATED                                                  ;;
;; ------------------------------------------------------------------------------;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b77a00d5be78f21e46c80ce450e5821bdc4368abf4ffe2b77c5a66de1b648f10" default))
 '(org-ellipsis "  ▼")
 '(org-hide-emphasis-markers t)
 '(package-selected-packages
   '(haskell-mode auctex ob-typescript no-littering org-roam prettier-js add-node-modules-path rainbow-mode smartparens dap-chrome dap-chrome-setup benchmark-init tide beacon powerline all-the-icons-dired treemacs-magit treemacs-icons-dired treemacs-projectile lsp-ivy lsp-treemacs doom-modeline cyberpunk-theme color-theme-sanityinc-tomorrow ir-black-theme gruber-darker-theme seti-theme seti monokai-theme zenburn-theme exec-path-from-shell ts-ls typescript-mode lsp-mode visual-fill-column visual-fill visual-fill-mode org-bullets emojify magit counsel-projectile projectile which-key rainbow-delimiters ivy use-package))
 '(treemacs-project-follow-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#199919991999"))) t)
 '(company-scrollbar-fg ((t (:background "#0ccc0ccc0ccc"))) t)
 '(company-tooltip ((t (:inherit default :background "#051e051e051e"))))
 '(company-tooltip-common ((t (:inherit front-lock-constant-face))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#0ccc0ccc0ccc"))))
 '(company-tooltip-scrollbar-track ((t (:background "#199919991999"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(org-ellipsis ((t (:foreground "DeepPink3" :underline nil))))
 '(sp-show-pair-match-face ((t (:foreground "White")))))
(put 'upcase-region 'disabled nil)
