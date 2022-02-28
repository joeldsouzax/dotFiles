
;; test configuration
;; will need to change this later on.
;; use emacs org configuration



;; --------------------------
;; VARIABLE DECLARATION
;; --------------------------

(defvar jd/default-font-size 130)
(defvar jd/default-variable-font-size 160)


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
(scroll-bar-mode -1)             ;; disable visible scrollbar
(tool-bar-mode -1)               ;; disable the toolbar
(tooltip-mode -1)                ;; disable the tooltips
(set-fringe-mode 10)             ;; give some breathing room :D
(menu-bar-mode -1)               ;; disable the menu bar
(setq visible-bell 1)            ;; set up bthe visible alert bell


;; --------------------------
;; MODELINE CONFIG
;; ----------------------




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
;; TEMPORARY THEME CONFIG
;; --------------------------

(use-package zenburn-theme
  :config
  (setq zenburn-override-colors-alist
	'(("zenburn-bg" . "#000000")))
  (load-theme 'zenburn t))

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
  :init
  (ivy-mode 1))


;; --------------------------
;; RAINBOW-DELIMITERS CONFIG
;; --------------------------

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))                                      ;; rainbow delimiters only on programming modes.

;; --------------------------
;; WHICH KEY CONFIG
;; --------------------------

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


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
  (when (file-directory-p "~/dev/src/github.com")
    (setq projectile-project-search-path '("~/dev/src/github.com")))
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
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; --------------------------
;; EMOJIFY CONFIG
;; --------------------------

(use-package emojify
  :init
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
  (push '("#+end_src" . "λ") prettify-symbols-alist)
  (push '("#(ref:" . "(") prettify-symbols-alist)
  (prettify-symbols-mode))




(use-package org
  :hook (org-mode . jd/org-mode-setup)
  :config
  (setq org-agenda-start-with-log-mode t)
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
  :after org
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

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (css . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes))


;; --------------------------
;; ORG CODE TEMPLATE  CONFIG
;; --------------------------


(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("css" . "src css"))
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
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
	  treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)


    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)


;; lsp ivy

(use-package lsp-ivy)


;; typescript language server

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))





;; --------------------------
;; COMPANY MODE CONFIG
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
  :hook (company-mode . company-box-mode))


;; --------------------------
;; SHELL ENVIRONENT CONFIG
;; --------------------------

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


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
   '(treemacs-magit treemacs-icons-dired treemacs-projectile lsp-ivy lsp-treemacs doom-modeline cyberpunk-theme color-theme-sanityinc-tomorrow ir-black-theme gruber-darker-theme seti-theme seti monokai-theme zenburn-theme exec-path-from-shell ts-ls typescript-mode lsp-mode visual-fill-column visual-fill visual-fill-mode org-bullets emojify magit counsel-projectile projectile which-key rainbow-delimiters ivy use-package)))
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
 '(org-ellipsis ((t (:foreground "DeepPink3" :underline nil)))))
