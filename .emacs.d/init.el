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

(setq package-enable-at-startup nil) (package-initialize)

(eval-when-compile
  (require 'use-package))

(setq load-prefer-newer t)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package gnus
  :ensure t
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

;; remove the startup screen
(setq inhibit-startup-screen t)

;; get rid of the annoying bell
(setq ring-bell-function 'ignore)

;; indent with spaces
(setq-default indent-tabs-mode nil)
;; set tab width to 4
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; ssh is faster than the default scp
(setq tramp-default-method "ssh")

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))

(use-package xah-fly-keys
  :diminish xah-fly-keys
  :ensure t
  :config
  (xah-fly-keys-set-layout "qwerty") ; required if you use qwerty
  (xah-fly-keys 1))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

;; ivy mode
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")

  ;; use a fuzzy matcher
  (setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
  (ivy-mode 1))

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
  :defer t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; enable flycheck globally
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :defer t
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

;; company mode
(use-package company
  :ensure t
  :diminish ""
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

(use-package smart-compile
  :ensure t)

(use-package irony
  :ensure t)

(use-package company-irony
  :ensure t)

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
  :defer t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package irony-eldoc
  :ensure t)

;; Enable elpy for python
(use-package elpy
  :ensure t
  :defer t
  :config
  (elpy-enable)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq python-indent-offset 4)
  (highlight-indentation-mode nil))

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
  :defer t
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
  (add-to-list 'org-agenda-files "/home/ebs/Documents/org/ideas.org)"))

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
  (use-package ox-twbs) :ensure t)

;; Load magit last
(use-package magit
  :ensure t
  :defer t)

(provide 'init))
;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#373b41"))
 '(ansi-term-color-vector
   [unspecified "#151515" "#fb9fb1" "#acc267" "#ddb26f" "#6fc2ef" "#e1a3ee" "#6fc2ef" "#d0d0d0"] t)
 '(custom-safe-themes
   (quote
    ("0ee3fc6d2e0fc8715ff59aed2432510d98f7e76fe81d183a0eb96789f4d897ca" "e1994cf306356e4358af96735930e73eadbaf95349db14db6d9539923b225565" "93268bf5365f22c685550a3cbb8c687a1211e827edc76ce7be3c4bd764054bad" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(fci-rule-color "#373b41")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(package-selected-packages
   (quote
    (creamsody-theme esup idle-highlight-mode xah-fly-keys which-key use-package smart-compile scala-mode sbt-mode py-autopep8 popup ox-twbs ox-pandoc org-bullets org-brain markdown-mode linum-relative irony-eldoc htmlize flycheck-irony flx elpy company-irony-c-headers company-irony color-theme-sanityinc-tomorrow color-theme autothemer)))
 '(send-mail-function (quote smtpmail-send-it))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "#c5c8c6" :font "Source Sans Pro" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#c5c8c6" :font "Source Sans Pro" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#c5c8c6" :font "Source Sans Pro" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#c5c8c6" :font "Source Sans Pro" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#c5c8c6" :font "Source Sans Pro" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#c5c8c6" :font "Source Sans Pro"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#c5c8c6" :font "Source Sans Pro"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#c5c8c6" :font "Source Sans Pro"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#c5c8c6" :font "Source Sans Pro")))))
