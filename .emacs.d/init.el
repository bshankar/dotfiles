;; init.el --- user init file
;;; commentary:
;;; code:

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(setq package-enable-at-startup nil)
(package-initialize)

(defvar use-package-always-ensure)
(setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq load-prefer-newer t)

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

(global-hl-line-mode -1)

(show-paren-mode t)
(defvar show-paren-style)
(setq show-paren-style 'parenthesis)

(defvar compilation-scroll-output)
(setq compilation-scroll-output t)

(electric-pair-mode 1)
(delete-selection-mode 1)

(setq initial-major-mode 'org-mode)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(load "~/.emacs.d/fira.el")
(load "~/.emacs.d/org.el")

(defvar tramp-default-method)
(setq tramp-default-method "ssh")

(setq scroll-step 1)
(setq auto-window-vscroll nil)

(put 'dired-find-alternate-file 'disabled nil)

(setq make-backup-files nil)
(setq create-lockfiles nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package whitespace-mode
  :delight whitespace-mode
  :hook prog-mode
  :init
  (setq-default
   whitespace-line-column 80
   whitespace-style '(face lines-tail)))

(load-theme 'adwaita)

(use-package telephone-line
  :config
  (setq telephone-line-primary-right-separator 'telephone-line-halfcos-right
        telephone-line-secondary-right-separator 'telephone-line-halfcos-hollow-right
        telephone-line-primary-left-separator 'telephone-line-halfcos-left
        telephone-line-secondary-left-separator 'telephone-line-halfcos-hollow-left
        telephone-line-height 24)

  (setq telephone-line-lhs
        '((evil   . (telephone-line-xah-fly-keys-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))

  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil    . (telephone-line-airline-position-segment))))
  (telephone-line-mode 1))

(use-package delight
  :config
  :delight eldoc-mode)

(use-package try
  :defer 4)

(use-package helpful
  :defer 5
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-.") #'helpful-at-point))

(use-package xah-fly-keys
  :delight xah-fly-keys
  :config
  (xah-fly-keys-set-layout "qwerty")
  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
  (xah-fly-keys 1))

(use-package hideshow
  :hook ((prog-mode . hs-minor-mode))
  :config
  (defun toggle-fold ()
    (interactive)
    (indent-for-tab-command)
    (save-excursion (end-of-line) (hs-toggle-hiding)))
  (global-set-key (kbd "TAB") 'toggle-fold))

(use-package multiple-cursors
  :defer 2
  :config
  (define-key xah-fly-comma-keymap (kbd "d") 'mc/edit-lines)
  (define-key xah-fly-comma-keymap (kbd ";") 'mc/mark-next-like-this)
  (define-key xah-fly-comma-keymap (kbd "h") 'mc/mark-previous-like-this)
  (define-key xah-fly-comma-keymap (kbd "i") 'mc/mark-all-dwim))

(use-package which-key
  :delight
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

(use-package fortune-cookie
  :init
  (setq fortune-cookie-cowsay-args  "-f tux -s")
  (setq fortune-cookie-comment-start "# ")
  :config
  (fortune-cookie-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode))

(use-package counsel
  :delight ivy-mode
  :config
  (ivy-mode 1)

  (setq ivy-count-format ""
        ivy-display-style nil
        ivy-minibuffer-faces nil)

  (use-package flx)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)

  (defvar ivy-use-virtual-buffers)
  (setq ivy-use-virtual-buffers t)

  (defvar projectile-completion-system)
  (setq projectile-completion-system 'ivy)

  (define-key xah-fly-dot-keymap (kbd "a") 'counsel-rg)
  (define-key xah-fly-dot-keymap (kbd "f") 'counsel-fzf))

(use-package smex
  :config
  (smex-initialize))

(use-package dumb-jump
  :defer 3
  :config
  (define-key xah-fly-dot-keymap (kbd "b") 'dumb-jump-go)
  (define-key xah-fly-dot-keymap (kbd "c") 'dumb-jump-back))

(use-package abbrev
  :delight
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package yasnippet
  :delight yas-minor-mode
  :defer 2
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets))

(use-package highlight-indent-guides
  :delight
  :hook ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character))

(use-package aggressive-indent
  :delight aggressive-indent-mode
  :config
  (add-hook 'prog-mode-hook #'aggressive-indent-mode))

(use-package projectile
  :config
  (projectile-mode +1))

(use-package flycheck
  :defer 2.5
  :delight
  :config
  (global-flycheck-mode))

(use-package company
  :delight
  :defer 2.5
  :init
  (defvar company-dabbrev-downcase)
  (setq company-idle-delay 0
        company-require-match nil
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-tooltip-margin 1
        company-echo-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance)
        company-backends '(company-capf
                           (company-dabbrev company-dabbrev-code company-keywords)
                           company-yasnippet company-files))
  :config
  (progn
    (setq company-global-modes '(not gud-mode org-mode))
    (push (apply-partially #'cl-remove-if
                           (lambda (c) (string-match-p "\\`[0-9]+\\'" c)))
          company-transformers))
  (global-company-mode 1))

(use-package restclient
  :mode ("\\.rc\\'" . restclient-mode)
  :config
  (use-package company-restclient
    :config
    (add-to-list 'company-backends 'company-restclient)))

(use-package typescript-mode
  :mode ("\\.[jt]sx?\\'" . typescript-mode)
  :config
  (use-package tide
    :config
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (tide-hl-identifier-mode +1)
      (eldoc-mode +1)
      (setq typescript-indent-level 2)
      (company-mode +1))

    (setup-tide-mode)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq rust-format-on-save t)
  (use-package flycheck-rust
    :ensure
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

  (use-package racer
    :config
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode))

  (use-package cargo
    :config
    (add-hook 'rust-mode-hook 'cargo-minor-mode)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :config
  (setq js-indent-level 2)
  (setq json-reformat:indent-width 2))

(use-package yaml-mode
  :mode (("\\.yml'" . yaml-mode)
         ("\\.yaml'" . yaml-mode)))


(use-package vue-mode
  :mode ("\\.vue\\'" . vue-mode)
  :config
  (use-package vue-html-mode)
  (use-package emmet-mode
    :hook vue-mode))

(use-package git-gutter
  :config
  (defvar git-gutter:update-interval)
  (setq git-gutter:update-interval 2)
  (global-git-gutter-mode t))

(use-package magit
  :defer 4
  :delight auto-revert-mode
  :config
  (define-key xah-fly-leader-key-map (kbd "b") 'magit-status)
  (use-package gist)
  (use-package magithub
    :config
    (magithub-feature-autoinject t)
    (setq magithub-clone-default-directory "~/Documents/code/")))

(add-hook 'emacs-startup-hook
	        (lambda () (setq gc-cons-threshold 16777216
                           gc-cons-percentage 0.1)))

(provide 'init)

;;; init.el ends here
