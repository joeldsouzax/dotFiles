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


;; --------------------------
;; FONT CONFIG
;; --------------------------

(set-face-attribute 'default nil :font "Menlo" :height 150)                         ;; set font type and size

;; --------------------------
;; TEMPORARY THEME CONFIG
;; --------------------------

(load-theme 'tango-dark)                                                            ;; loaded basic built in theme

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
;; AUTO CONFIG
;; --------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(counsel-projectile projectile which-key rainbow-delimiters ivy use-package)))
(custom-set-facesc
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
