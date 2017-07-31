;; init.el --- user init file      -*- no-byte-compile: t -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (esup xah-fly-keys which-key use-package telephone-line smart-compile scala-mode py-autopep8 powerline popup ox-twbs ox-pandoc org-bullets org-brain markdown-mode magit linum-relative ledger-mode irony-eldoc idle-highlight-mode htmlize hexrgb gandalf-theme flycheck-ledger flycheck-irony flx elpy company-irony-c-headers company-irony color-theme autothemer))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq-default gc-cons-threshold 100000000)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;;; commentary:
;;; code:

(setq package-enable-at-startup nil) (package-initialize)

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
;; use pretty symbols
(global-prettify-symbols-mode t)

;; do not highlight current line
(global-hl-line-mode -1)

;; highlight matching parenthesis
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; automatically scroll compilation output
(setq compilation-scroll-output t)

;; Close parenthesis automatically
(electric-pair-mode 1)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package xah-fly-keys
  :ensure t
  :diminish xah-fly-keys
  :config
  (xah-fly-keys-set-layout "qwerty") ; required if you use qwerty
  (xah-fly-keys 1))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package gandalf-theme
  :ensure t
  :config
  (load-theme 'gandalf t))

(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-halfcos-left
        telephone-line-secondary-left-separator 'telephone-line-halfcos-hollow-left
        telephone-line-primary-right-separator 'telephone-line-halfcos-right
        telephone-line-secondary-right-separator 'telephone-line-halfcos-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  (telephone-line-mode 1))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config

  (defun ivy-format-function-horizontal (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (ivy--add-face str 'ivy-current-match))
   #'identity
   cands
   " | "))
  
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-format-function 'ivy-format-function-horizontal)
  ;; use a fuzzy matcher
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))

  (ivy-mode 1))

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
  :defer 2
  :diminish yas-global-mode
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
    (add-to-list 'company-backends 'company-math-symbols-unicode)))

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
    (add-hook 'after-init-hook 'global-company-mode)

    (use-package company-irony
      :ensure t)


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
      :ensure t)))

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

;; ;; allow babel to run elisp, python and sh codes
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
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
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

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
  ;; load agenda files
  (add-to-list 'org-agenda-files "/home/ebs/Documents/org/gtd.org)")
  (add-to-list 'org-agenda-files "/home/ebs/Documents/org/ideas.org)")

  ;; htmlize for syntax highlighting in exported html
  (use-package htmlize
    :ensure t
    :config
    ;; syntax highlight from a css file instead of copying
    (setq org-html-htmlize-output-type 'css)
    (setq org-html-htmlize-font-prefix "org-"))

  (setq org-todo-keywords
        '((sequence "NEXT(n)" "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)" "FAILED(f)")))

  ;; pretty bullets for org-mode
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  ;; export org-mode to various formats using pandoc
  (with-eval-after-load 'ox
    (require 'ox-md nil t)
    (use-package ox-pandoc :ensure t)
    (use-package ox-twbs) :ensure t))

;; Load magit last
(use-package magit
  :ensure t
  :defer 5)

(provide 'init)
;;; init.el ends here
