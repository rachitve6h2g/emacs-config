;; User interface

;; Set font
(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 120)

;; Misc
(setq inhibit-startup-message t)
(setq visible-bell t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; Initialize use-package
(require 'use-package)

;; COMPLETION
;; Use nerd-icons for completions
;; Set up vertico
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Use the command log mode to see the commands passed by keybindings/mousebindings/
(use-package command-log-mode)

;; Condiments

;; Doom Modeline setup
(use-package doom-modeline
  :init (doom-modeline-mode 1))
