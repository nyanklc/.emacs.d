;; Make all commands of the “package” module present.
(require 'package)

;; Internet repositories for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))

;; Actually get “package” to work.
(package-initialize)
(package-refresh-contents)(setq inhibit-startup-message t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

(use-package auto-package-update
  :defer 10
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))

;; Making it easier to discover Emacs key presses.
(use-package which-key
  :diminish
  :defer 5
  :config (which-key-mode)
          (which-key-setup-side-window-bottom)
          (setq which-key-idle-delay 0.05))

;; Bottom of Emacs will show what branch you're on
;; and whether the local file is modified or not.
;; '?' shows keybindings
(use-package magit)

;; Library for working with system files;
;; e.g., f-delete, f-mkdir, f-move, f-exists?, f-hidden?
(use-package f)

;; idfk
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20" default))
 '(package-selected-packages
   '(aggressive-indent focus rainbow-delimiters gruvbox-theme auto-package-update use-package yasnippet-snippets yaml-mode windresize which-key wgrep web-mode visual-regexp untitled-new-buffer unfill undo-fu smooth-scroll smex rotate poly-org poly-noweb poly-markdown poetry pdf-tools outline-magic multiple-cursors multi-term minions magit ivy-hydra htmlize howdoi flycheck flx-ido exec-path-from-shell eval-in-repl epc dumb-jump diff-hl counsel conda company-math company-auctex command-log-mode better-defaults anzu adaptive-wrap)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; rainbow parantheses
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; disable gui thingies
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; M-x focus-change-thing changes focus space
(require 'focus)

;; indentation
(global-aggressive-indent-mode 1)

;; font
(set-frame-font "Ubuntu Mono" nil t)

(windmove-default-keybindings)

(setq-default cursor-type 'bar)

