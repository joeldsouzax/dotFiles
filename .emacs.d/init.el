
;; test configuration
;; will need to change this later on.
;; use emacs org configuration


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
		eshell-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))                         ;; disable line numbers for some modes.

(setq-default highlight-tabs 1)                                                     ;; highlight tabs 

;; --------------------------
;; FONT CONFIG
;; --------------------------

(set-face-attribute 'default nil :font "Menlo" :height 150)                         ;; set font type and size

;; --------------------------
;; TEMPORARY THEME CONFIG
;; --------------------------

(load-theme 'manoj-dark)                                                            ;; loaded basic built in theme

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
  :config (counsel-projectile-mode))

;; --------------------------
;; MAGIT CONFIG
;; --------------------------

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; --------------------------
;; EMOJIFY CONFIG
;; --------------------------
(add-hook 'after-init-hook #'global-emojify-mode)                                   ;; enable emojis globally :D

(use-package emojify
  :init
  (global-emojify-mode))


;; --------------------------
;; ORG-MODE CONFIG
;; --------------------------


(defun jd/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (push '("[ ]" . "🔳") prettify-symbols-alist)
  (push '("[X]" . "☑") prettify-symbols-alist)
  (push '("[-]" . "◾") prettify-symbols-alist)
  (push '("#+BEGIN_SRC" . "λ") prettify-symbols-alist)
  (push '("#+END_SRC" . "λ") prettify-symbols-alist)
  (prettify-symbols-mode))




(use-package org
  :hook (org-mode . jd/org-mode-setup)
  :config
  (setq org-ellipsis "  ▼"
	org-hide-emphasis-markers t))

;; org bullets package to style the org heading, sub heading etc


(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("🚀" "💃" "☄" "✏" "🕺" "💻" "💣" "🏹" "🗡" "🛡")))

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
				   (set-face-attribute (car face) nil :font "Menlo" :weight 'regular :height (cdr face))))


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


;; ------------------------------------------------------------------------------;;
;; AUTO CONFIG + AUTO GENERATED                                                  ;;
;; ------------------------------------------------------------------------------;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(visual-fill-column visual-fill visual-fill-mode org-bullets emojify magit counsel-projectile projectile which-key rainbow-delimiters ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-ellipsis ((t (:foreground "DeepPink3" :underline nil)))))
