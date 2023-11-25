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
;; (use-package doom-modeline :ensure t)
;; (doom-modeline-mode 1)
(column-number-mode 1)                            ; Show the column number
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)
(windmove-default-keybindings)

;; indent
(setq-default indent-tabs-mode nil) ; spaces
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; parantheses/brackets
(electric-pair-mode t)

;; backspace no copy
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
 (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(global-set-key (kbd "C-<backspace>") 'backward-delete-word) 

;;; use command key for meta in mac
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

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
(blink-cursor-mode 0)

;; tab completion
(setq completion-cycle-threshold t)

(use-package flycheck :ensure t)
(global-flycheck-mode)
(setq-default flycheck-disabled-checker '(emacs-lisp-checkdoc))

(use-package company :ensure t)
(global-company-mode)
(global-hl-line-mode)

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ((c++-mode) . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
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
(use-package lsp-jedi :ensure t)

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

;; multiterm
(use-package multi-term :ensure t)
(setq multi-term-program "/bin/bash")

;; perhaps I should remove these, add them to the config instead
;; TODO: cleanup
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "c8b3d9364302b16318e0f231981e94cbe4806cb5cde5732c3e5c3e05e1472434" "b1acc21dcb556407306eccd73f90eb7d69664380483b18496d9c5ccc5968ab43" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd" "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434" "162201cf5b5899938cfaec99c8cb35a2f1bf0775fc9ccbf5e63130a1ea217213" "9e1cf0f16477d0da814691c1b9add22d7cb34e0bb3334db7822424a449d20078" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "c517e98fa036a0c21af481aadd2bdd6f44495be3d4ac2ce9d69201fcb2578533" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "70e7f094987e3c6226c54078dd986e11cab7246ea1c9e58a9907afa90f3c10c9" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" default))
 '(ido-cannot-complete-command 'ido-next-match)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(3 ((shift) . hscroll) ((meta)) ((control) . text-scale)))
 '(package-selected-packages
   '(lsp-jedi helm-R lsp-ivy helm-lsp company-c-headers flycheck-clang-tidy lsp-ui magit company electric-pair doom-themes neotree doom-modeline sudo-edit emojify dashboard all-the-icons gcmh lsp-mode aggressive-indent focus rainbow-delimiters gruvbox-theme auto-package-update use-package yasnippet-snippets yaml-mode windresize which-key wgrep web-mode visual-regexp untitled-new-buffer unfill undo-fu smooth-scroll smex rotate poly-org poly-noweb poly-markdown poetry pdf-tools outline-magic multiple-cursors multi-term minions ivy-hydra htmlize howdoi flycheck flx-ido exec-path-from-shell eval-in-repl epc dumb-jump diff-hl counsel conda company-math company-auctex command-log-mode better-defaults anzu adaptive-wrap))
 '(warning-suppress-types '((use-package) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

