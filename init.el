;; melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-refresh-contents)
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; Using garbage magic hack.
 (use-package gcmh
   :config
   (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;; icons
(use-package all-the-icons)

;; startup screen
(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")  ;; use custom image 
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
			                              (bookmarks . "book"))))

;; select text and type to replace
(delete-selection-mode t)

;; emojis?
(use-package emojify
  :hook (after-init . global-emojify-mode))

;; file editing
(use-package recentf
  :config
  (recentf-mode))
(use-package sudo-edit) ;; Utilities for opening files with sudo


"Show the full path to the current file in the minibuffer."
(defun dt/show-buffer-path-name ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

;; gui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
(use-package doom-modeline)
(doom-modeline-mode 1)


;; neotree
;; Function for setting a fixed width for neotree.
;; Defaults to 25 but I make it a bit longer (35) in the 'use-package neotree'.
(defcustom neo-window-width 25
  "*Specifies the width of the NeoTree window."
  :type 'integer
  :group 'neotree)

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-window-width 30
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        ;;neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))


(column-number-mode 1)                            ; Show the column number
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)                               ; Show the parent(fset 'yes-or-no-p 'y-or-n-p) ; Replace yes/no prompts with y/n


(windmove-default-keybindings)

;; theme
(use-package doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-one t)



;; display key-bindings
(use-package which-key
  :init
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → " ))
(which-key-mode)


;; idk
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))


;; tab completion mini buffer
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

(use-package smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; lsp stuff
(use-package 'flycheck)
(global-flycheck-mode)

(use-package company)
(global-company-mode)
(global-hl-line-mode)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; git diffs on line
(use-package magit)









(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20" default))
 '(package-selected-packages
   '(magit company electric-pair doom-themes neotree doom-modeline sudo-edit emojify dashboard all-the-icons gcmh lsp-mode aggressive-indent focus rainbow-delimiters gruvbox-theme auto-package-update use-package yasnippet-snippets yaml-mode windresize which-key wgrep web-mode visual-regexp untitled-new-buffer unfill undo-fu smooth-scroll smex rotate poly-org poly-noweb poly-markdown poetry pdf-tools outline-magic multiple-cursors multi-term minions ivy-hydra htmlize howdoi flycheck flx-ido exec-path-from-shell eval-in-repl epc dumb-jump diff-hl counsel conda company-math company-auctex command-log-mode better-defaults anzu adaptive-wrap))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
