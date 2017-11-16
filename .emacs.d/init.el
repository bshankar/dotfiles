;; init.el --- user init file      -*- no-byte-compile: t -*-
(let ((file-name-handler-alist nil))
  (setq-default gc-cons-threshold 100000000)
  
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;;; commentary:
;;; code:

  (setq package-enable-at-startup nil)
  (package-initialize)

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

  ;; remove the startup screen
  (setq inhibit-startup-screen t)

  ;; get rid of the annoying bell
  (setq ring-bell-function 'ignore)

  ;; indent with spaces
  (setq-default indent-tabs-mode nil)
  ;; set tab width to 4
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab)

  (setq load-prefer-newer t)
  
  ;; do not highlight current line
  (global-hl-line-mode -1)

  ;; highlight matching parenthesis
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis)

  ;; automatically scroll compilation output
  (setq compilation-scroll-output t)

  ;; Close parenthesis automatically
  (electric-pair-mode 1)

  ;; replace highlighted text with typed stuff
  (delete-selection-mode 1)
  
  ;; specify custom file
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

;; apropos sort by score
  (setq apropos-sort-by-scores t)

  ;; scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time
  
  (use-package auto-compile
    :config
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode))

  (use-package try
    :ensure t)
  
  (use-package xah-fly-keys
    :ensure t
    :diminish xah-fly-keys
    :config
    (xah-fly-keys-set-layout "qwerty") ; required if you use qwerty
    (xah-fly-keys 1))

  (use-package which-key
    :diminish which-key-mode
    :config
    (which-key-setup-side-window-bottom)
    (which-key-mode 1))

  (use-package tango-plus-theme
    :ensure t
    :config
    (load-theme 'tango-plus t))
  
  (use-package telephone-line
    :ensure t
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

  ;; load fira code symbols
  (load-file "~/.emacs.d/fira.el")

  (use-package counsel
    :ensure t
    :diminish ivy-mode
    :config
    (ivy-mode 1)
    
    ; Slim down ivy display
    (setq ivy-count-format ""
          ivy-display-style nil
          ivy-minibuffer-faces nil)

    ;; Let ivy use flx for fuzzy-matching
    (use-package flx
      :ensure t)
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

    ; Use Enter on a directory to navigate into the directory, not open it with dired.
    (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)

    (setq ivy-use-virtual-buffers t)
    
    ; Let projectile use ivy
    (setq projectile-completion-system 'ivy)

    ;; bind counsel-ripgrep
    (define-key xah-fly-dot-keymap (kbd "a") 'counsel-rg))
  
  (use-package smex
    :ensure t
    :config
    (smex-initialize))

  (use-package avy
    :ensure t
    :diminish avy-mode
    :config
    (define-key xah-fly-leader-key-map (kbd "z") 'avy-goto-char-timer))

  (use-package dumb-jump
    :ensure t
    :config
    (define-key xah-fly-dot-keymap (kbd "b") 'dumb-jump-go)
    (define-key xah-fly-dot-keymap (kbd "c") 'dumb-jump-back))
  
  (use-package abbrev
    :diminish abbrev-mode
    :config
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file))
    ;; silently save new abbrevations
    (setq save-abbrevs 'silently)
    ;; always enable abbrev mode
    (setq-default abbrev-mode t))

  ;; snippet manager
  (use-package yasnippet
    :ensure t
    :diminish yas-minor-mode
    :defer 2
    :diminish yas-mode
    :config
    (yas-global-mode 1))

  ;; enable flycheck globally
  (use-package flycheck
    :ensure t
    :diminish flycheck-mode
    :config
    (global-flycheck-mode)

    (flycheck-define-checker proselint
      "A linter for prose."
      :command ("proselint" source-inplace)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ": "
                (id (one-or-more (not (any " "))))
                (message (one-or-more not-newline)
                         (zero-or-more "\n" (any " ") (one-or-more not-newline)))
                line-end))
      :modes (text-mode markdown-mode gfm-mode org-mode))

    (add-to-list 'flycheck-checkers 'proselint)
    (add-hook 'text-mode-hook #'flycheck-mode)
    (add-hook 'org-mode-hook #'flycheck-mode))
  
  (use-package ledger-mode
    :ensure t
    :mode ("\\.ledger\\'" . ledger-mode)
    :config
    ;; flycheck-ledger
    (use-package flycheck-ledger
      :ensure t))

  ;; company mode
  (use-package company
    :ensure t
    :diminish company-mode
    :config
    (progn
      (setq company-global-modes '(not gud-mode))
      (add-hook 'org-mode-hook #'global-company-mode)
      (add-hook 'emacs-lisp-mode-hook #'company-mode)
      (bind-key "<tab>" 'company-complete-selection company-active-map)
      (setq-default pos-tip-background-color "khaki1")
      (bind-keys :map company-active-map
                 ("C-n" . company-select-nex)
                 ("C-p" . company-select-previous)
                 ("C-d" . company-show-doc-buffer)
                 ("<tab>" . company-complete))
      (setq company-idle-delay 0
            company-require-match nil
            ;;company-begin-commands '(self-insert-command)
            company-show-numbers t
            company-tooltip-align-annotations t
            company-tooltip-flip-when-above t
            company-tooltip-margin 1
            company-echo-delay 0
            company-dabbrev-downcase nil
            company-minimum-prefix-length 1
            company-selection-wrap-around t
            company-transformers '(company-sort-by-occurrence
                                   company-sort-by-backend-importance)
            company-backends '(company-capf
                               (company-dabbrev company-dabbrev-code company-keywords)
                               company-yasnippet))
      (push (apply-partially #'cl-remove-if
                             (lambda (c) (string-match-p "\\`[0-9]+\\'" c)))
            company-transformers)
      (add-to-list 'company-backends 'company-math-symbols-unicode))
    (global-company-mode 1))

  (use-package gnus
    :ensure t
    :defer t
    :config
    (setq user-full-name "Bhavani Shankar")
    (setq user-email-address "ebhavanishankar@gmail.com")
    
    (setq gnus-select-method '(nnnil))
    (setq gnus-secondary-select-methods '((nntp "news.gwene.org")))

    (setq gnus-select-method
          '(nnimap "gmail"
                   (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
                   (nnimap-server-port "imaps")
                   (nnimap-stream ssl)))

    (setq smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587
          gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"))
    
  (use-package cc-mode
    :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
           ("\\.m\\'"                   . c-mode)
           ("\\.mm\\'"                  . c++-mode))
    :config
    (use-package irony
      :ensure t
      :init
      (setq irony-additional-clang-options '("-std=c++17"))
      :config
      ;; Irony mode for c++
      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'c-mode-hook 'irony-mode)
      (add-hook 'objc-mode-hook 'irony-mode)

      ;; replace the `completion-at-point' and `complete-symbol' bindings in
      ;; irony-mode's buffers by irony-mode's function
      (defun my-irony-mode-hook ()
        "Recommended settings for irony-mode hook."
        (define-key irony-mode-map [remap completion-at-point]
          'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol]
          'irony-completion-at-point-async))
      (add-hook 'irony-mode-hook 'my-irony-mode-hook)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
      (add-hook 'irony-mode-hook 'irony-eldoc)

      ;; use comany backend for auto completion everywhere
      (add-hook 'after-init-hook 'global-company-mode))

    (use-package company-irony
      :ensure t
      :config
      (eval-after-load 'company
        '(add-to-list 'company-backends 'company-irony)))

    (use-package company-irony-c-headers
      :ensure t
      :config
      (eval-after-load 'company
        '(add-to-list
          'company-backends '(company-irony-c-headers company-irony))))

    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))

    (use-package flycheck-irony
      :ensure t
      :config
      (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

    (use-package irony-eldoc
      :ensure t)

    (use-package clang-format
      :ensure t
      :config
      (defun clang-format-buffer-on-save ()
        "Add auto-save hook for clang-format-buffer-smart."
        (add-hook 'before-save-hook 'clang-format-buffer nil t))
      (add-hook 'clang-format-buffer-on-save '(c-mode-hook c++-mode-hook))))

  ;; Enable elpy for python
  (use-package python
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python" . python-mode)
    :config
    (use-package elpy
      :ensure t
      :config
      (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode)
      (setq python-indent-offset 4)
      (highlight-indentation-mode nil)
      (elpy-enable)))

  (use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

  (use-package web-mode
    :ensure t
    :mode (("\\.phtml\\'" . web-mode)
           ("\\.tpl\\.php\\'" . web-mode)
           ("\\.[agj]sp\\'" . web-mode)
           ("\\.as[cp]x\\'" . web-mode)
           ("\\.erb\\'" . web-mode)
           ("\\.mustache\\'" . web-mode)
           ("\\.djhtml\\'" . web-mode)
           ("\\.html?\\'" . web-mode)
           ("\\.json\\'" . web-mode)
           ("\\.js\\'" . web-mode)
           ("\\.jsx\\'" . web-mode)
           ("\\.s?css'" . web-mode)
           ("\\.xml\\'" . web-mode))

    :init
    ;; disable jshint since we prefer eslint checking
    (setq flycheck-eslintrc "~/.eslintrc.json")
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))
    
    ;; use eslint with web-mode for jsx files
    (flycheck-add-mode 'javascript-eslint 'web-mode)

    (defun my-web-mode-hook ()
      "Hooks for Web mode. Set all indentations to 2 spaces"
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-css-indent-offset 2))

    (add-hook 'web-mode-hook  'my-web-mode-hook)
    
    :config
    (use-package tern
      :ensure t
      :init
      (add-hook 'web-mode-hook (lambda () (tern-mode t)))
      :config
      (setq tern-command (append tern-command '("--no-port-file")))
      (use-package company-tern
        :ensure t
        :config
        (add-to-list 'company-backends 'company-tern))))

  ;; clean language
  (use-package clean-mode
    :load-path "~/.emacs.d/elpa/clean-mode"
    :mode ("\\.cl\\'" . clean-mode))

  ;; ;; allow babel to run elisp, python and sh codes
  (use-package org
    :ensure t
    :init
    (setq org-export-with-timestamps nil)
    :config
    ;; open html exported file in firefox
    '(org-file-apps
      (quote
       ((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.html\\'" . "/usr/bin/firefox %s")
        ("\\.pdf\\'" . default))))

    ;; let babel execute python and sh in org documents
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       (dot . t)
       (sh . t)))

    ;; custom ellipsis for org-mode (...)
    (set-display-table-slot standard-display-table
                            'selective-display (string-to-vector " ⤵"))

    ;; fontify natively for org
    (setq org-src-fontify-natively t)

    ;; hide emphasis markers
    (setq org-hide-emphasis-markers t)

    ;; better bullets
    (font-lock-add-keywords 'org-mode
                            '(("^ +\\([-*]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;; larger headings
    (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                                 ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                                 ((x-list-fonts "Verdana")         '(:font "Verdana"))
                                 ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                                 (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
           (base-font-color     (face-foreground 'default nil 'default))
           (headline           `(:inherit default :weight bold)))

      (custom-theme-set-faces 'user
                              `(org-level-8 ((t (,@headline ,@variable-tuple))))
                              `(org-level-7 ((t (,@headline ,@variable-tuple))))
                              `(org-level-6 ((t (,@headline ,@variable-tuple))))
                              `(org-level-5 ((t (,@headline ,@variable-tuple))))
                              `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                              `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                              `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                              `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                              `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
    
    ;; preserve indentation
    (setq org-src-preserve-indentation t)
    ;; evaluate code without confirm
    (setq org-confirm-babel-evaluate nil)
    ;; enable spell checking for org-mode
    (add-hook 'org-mode-hook 'turn-on-flyspell)
    ;; warp paragraphs for text and org mode
    (add-hook 'text-mode-hook 'turn-on-auto-fill)
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    ;; disable text warping with hotkey
    (global-set-key (kbd "C-c q") 'auto-fill-mode)

    (setq org-todo-keywords
          '((sequence "NEXT(n)" "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)" "FAILED(f)")))

    ;; capture templates
    (setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Documents/org/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Documents/org/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))
    ;; refile targets
    (setq org-refile-targets '(("~/Documents/org/gtd/gtd.org" :maxlevel . 3)
                           ("~/Documents/org/gtd/someday.org" :level . 1)
                           ("~/Documents/org/gtd/tickler.org" :maxlevel . 2)))

    ;; agenda files
    (setq org-agenda-files '("~/Documents/org/gtd/inbox.org"
                         "~/Documents/org/gtd/gtd.org"
                         "~/Documents/org/gtd/tickler.org"))

    (define-key xah-fly-t-keymap (kbd "a") 'org-agenda-list)
    (define-key xah-fly-t-keymap (kbd "b") 'org-capture)
    
    ;; htmlize for syntax highlighting in exported html
    (use-package htmlize
      :ensure t
      :config
      ;; syntax highlight from a css file instead of copying
      (setq org-html-htmlize-output-type 'css)
      (setq org-html-htmlize-font-prefix "org-"))

    ;; pretty bullets for org-mode
    (use-package org-bullets
      :ensure t
      :config
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

    ;; export org-mode to various formats using pandoc
    (with-eval-after-load 'ox
      (use-package ox-twbs :ensure t)
      (use-package ox-reveal
        :ensure t
        :init
        (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/")
        (setq org-reveal-mathjax t))))

  ;; Load magit last
  (use-package magit
    :ensure t
    :defer 4
    :config
    (define-key xah-fly-leader-key-map (kbd "b") 'magit-status))
  
  (provide 'init))
;;; init.el ends here

