;; Installing MELPA package archive
;;(require 'package)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(package-initialize)

;; Enable clipboard
(setq select-enable-clipboard t)
(setq select-enable-primary t)

(setq interprogram-cut-function
      (lambda (text)
	(let ((process-connection-type nil))
	  (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy")))
	    (process-send-string proc text)
	    (process-send-eof proc)))))
      

(setq interprogram-paste-function
      (lambda ()
	(shell-command-to-string "wl-paste -n || true")))

;; Treesitter setting.
(setq treesit-font-lock-level 4)
;;(setq major-mode-remap-alist
  ;;    '((nix-mode . nix-ts-mode)))

;; disable tool-bar
(tool-bar-mode -1)
(require 'use-package)
;; Set the font
(use-package nerd-icons)

;; Use the magit package
(use-package magit)

(set-face-attribute 'default nil :family "Iosevka Nerd Font" :height 135)
(set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font")
(set-face-attribute 'variable-pitch nil :font "Iosevka Nerd Font")

;; For ligature support
(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
   ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<*****>" "++" "+++"))
  (global-ligature-mode t)
  )

;; use org mode
(use-package org
  :defer t
  :hook ((org-mode . visual-line-mode)
	 (org-mode . org-indent-mode)))

;; Keys defined in org mode info page.
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

;;(use-package nix-ts-mode
;;:mode "\\.nix\\'")

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
		'(("Shell" (shfmt "-i" "4" "-ci"))
		  ("Nix" (nixfmt))
		  )))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width))

;; For envrc, direnv
(use-package envrc)
(envrc-global-mode)

;; Change the pdf-view-midnight-minor-mode colors.
(setq pdf-view-midnight-colors '("#cdd6f4" . "#1e1e2e"))

(add-hook 'pdf-view-mode-hook
  (lambda ()
    (display-line-numbers-mode -1)))

;; Save the place for pdf
(add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)

(save-place-mode 1)

;; Persist dark/light mode in emacs
(desktop-save-mode 1)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-nmode with concrete major-mode(e. g. python-mode)
         (nix-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; Catppuccin theme
(use-package catppuccin-theme
  :ensure t
  :init
  (setq catppuccin-flavor 'mocha)
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'catppuccin :no-confirm))

;; Setup gptel
(setq gptel-backend
      (gptel-make-ollama "ollama"
			 :host "localhost:11434"
			 :models '("qwen2.5:7b")))
(setq gptel-default-mode 'org-mod)

;; Set up dirvish
(use-package dired
  :config
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("s" "/ssh:my-remote-server"       "SSH server")
     ("e" "/sudo:root@localhost:/etc"  "Modify program settings")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  ;; (dirvish-peek-mode)             ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

;; Setup emms for mediaplayer
(require 'emms-setup)
(emms-all)
(setq emms-player-list '(emms-player-mpv)
      emms-info-functions '(emms-info-native))
(emms-add-dired)

;; Kdeconnect setup
(require 'kdeconnect)
(setq kdeconnect-devices '(("My Cellphone" . "a2af3bf8_67c3_4a4f_ba4e_295f0c437bc0")))
(setq kdeconnect-active-device '("My Cellphone" . "a2af3bf8_67c3_4a4f_ba4e_295f0c437bc0"))

;; Org-roam settings
(make-directory "~/org-roam")
(setq org-roam-directory (file-truename "~/org-roam"))
(setq find-file-visit-truename t)
(org-roam-db-autosync-mode)
