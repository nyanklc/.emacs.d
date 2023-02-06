

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
(load-theme 'doom-monokai-pro)

;; indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; c++ indent?
(defun my-c++-mode-hook ()
  (setq c-basic-offset 2)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; parantheses/brackets
(electric-pair-mode t)

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


;; cursor
(setq-default cursor-type 'box)

;; idk
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))


;; tab completion mini buffer
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)


;; lsp stuff
(use-package flycheck)
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
(require 'lsp-mode)
(use-package lsp-ui)
(use-package flycheck-google-cpplint)
(use-package company-c-headers)
(use-package company-ctags)
(use-package lsp-treemacs)
(use-package helm-lsp)
;; (use-package dap-mode)

(add-hook 'XXX-mode-hook #'lsp) ;; this piece of shit doesn't work idk why

;; git diffs on line
(use-package diff-hl)
(global-diff-hl-mode)

;; no backup files
(setq make-backup-files nil)








(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "ce4234c32262924c1d2f43e6b61312634938777071f1129c7cde3ebd4a3028da" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "1aa4243143f6c9f2a51ff173221f4fd23a1719f4194df6cef8878e75d349613d" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "5586a5db9dadef93b6b6e72720205a4fa92fd60e4ccfd3a5fa389782eab2371b" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20" default))
 '(highlight-indent-guides-method 'bitmap)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(3 ((shift) . hscroll) ((meta)) ((control) . text-scale)))
 '(package-selected-packages
   '(highlight-indent-guides dap-mode lsp-ivy helm-lsp company-c-headers flycheck-clang-tidy lsp-ui magit company electric-pair doom-themes neotree doom-modeline sudo-edit emojify dashboard all-the-icons gcmh lsp-mode aggressive-indent focus rainbow-delimiters gruvbox-theme auto-package-update use-package yasnippet-snippets yaml-mode windresize which-key wgrep web-mode visual-regexp untitled-new-buffer unfill undo-fu smooth-scroll smex rotate poly-org poly-noweb poly-markdown poetry pdf-tools outline-magic multiple-cursors multi-term minions ivy-hydra htmlize howdoi flycheck flx-ido exec-path-from-shell eval-in-repl epc dumb-jump diff-hl counsel conda company-math company-auctex command-log-mode better-defaults anzu adaptive-wrap))
 '(warning-suppress-types '((use-package) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
