;; turn off compilation warnings
(setq byte-compile-warnings '(not WARNING))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-refresh-contents)
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; select text and type to replace
(delete-selection-mode t)

;; gui
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
(use-package doom-modeline :ensure t)
(doom-modeline-mode 1)
(column-number-mode 1)                            ; Show the column number
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)
(windmove-default-keybindings)

;; theme
(use-package doom-themes :ensure t)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-gruvbox t)

;; indent
(setq-default indent-tabs-mode nil) ; spaces
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; parantheses/brackets
(electric-pair-mode t)

;; display key-bindings
(use-package which-key
  :ensure t
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

;; cursor
(setq-default cursor-type 'box)

;; tab completion
(setq completion-cycle-threshold t)

;; using helm now...
;; (require 'ido)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode t)

(use-package flycheck :ensure t)
(global-flycheck-mode)
(setq-default flycheck-disabled-checker '(emacs-lisp-checkdoc))

(use-package company :ensure t)
(global-company-mode)
(global-hl-line-mode)

(helm-mode 1)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(use-package lsp-mode
  :ensure t
  :init
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ((c++-mode) . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
;;(use-package flycheck-google-cpplint :ensure t)
(use-package company-c-headers :ensure t)
(use-package company-ctags :ensure t)
(use-package lsp-treemacs :ensure t)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package helm-lsp :ensure t)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

;; TODO:
;; lsp mode hooks
(add-hook 'c-mode-hook #'lsp)

;; git diffs on line
(use-package diff-hl :ensure t)
(global-diff-hl-mode)

;; buffer list
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; lsp keybind
(setq lsp-keymap-prefix "C-c l")

;; no backup files
(setq make-backup-files nil)

;; perhaps I should remove these, add them to the config instead
;; TODO: cleanup
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(3 ((shift) . hscroll) ((meta)) ((control) . text-scale)))
 '(package-selected-packages
   '(helm-R lsp-ivy helm-lsp company-c-headers flycheck-clang-tidy lsp-ui magit company electric-pair doom-themes neotree doom-modeline sudo-edit emojify dashboard all-the-icons gcmh lsp-mode aggressive-indent focus rainbow-delimiters gruvbox-theme auto-package-update use-package yasnippet-snippets yaml-mode windresize which-key wgrep web-mode visual-regexp untitled-new-buffer unfill undo-fu smooth-scroll smex rotate poly-org poly-noweb poly-markdown poetry pdf-tools outline-magic multiple-cursors multi-term minions ivy-hydra htmlize howdoi flycheck flx-ido exec-path-from-shell eval-in-repl epc dumb-jump diff-hl counsel conda company-math company-auctex command-log-mode better-defaults anzu adaptive-wrap))
 '(warning-suppress-types '((use-package) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

