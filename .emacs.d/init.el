;;; init.el --- user init file      -*- no-byte-compile: t -*-

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;;; commentary:
;;; code:

(package-initialize)

(eval-when-compile
  (require 'use-package))

(setq load-prefer-newer t)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (material)))
 '(custom-safe-themes
   (quote
    ("28c07d8e75ed7e2ea9154239caa4f9d3fc831cf2340ea3e307ee49b4321dc7f5" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "4156d0da4d9b715c6f7244be34f2622716fb563d185b6facedca2c0985751334" "38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" default)))
 '(fci-rule-color "#37474f")
 '(flycheck-clang-args (quote ("-std=c++1z")))
 '(hl-sexp-background-color "#1c1f26")
 '(irony-additional-clang-options (quote ("-std=c++1z -Wall -Wextra")))
 '(package-selected-packages
   (quote
    (tup-mode irony-eldoc swiper company-auctex auctex ensime material-theme ox-pandoc auto-compile evil-magit magit evil-org evil-commentary htmlize evil evil-leader evil-surround relative-line-numbers gruvbox-theme flycheck yasnippet company irony company-irony flycheck-irony elpy org org-bullets)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; some user details for packages
(setq user-full-name "Bhavani Shankar")
(setq user-email-address "ebs@openmailbox.org")

;; remove the startup screen
(setq inhibit-startup-screen t)

;; get rid of the annoying bell
(setq ring-bell-function 'ignore)

;; indent with spaces
(setq-default indent-tabs-mode nil)
;; set tab width to 4
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; force horizontal split always
(setq split-width-threshold 9999)

;; ivy mode
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  )

;; use pretty symbols
(global-prettify-symbols-mode t)

;; highlight current line
(global-hl-line-mode)

;; highlight matching parenthesis
(show-paren-mode t)
(setq show-paren-style 'expression)

;; automatically scroll compilation output
(setq compilation-scroll-output t)

;; (use-package abbrev
  ;; :config
  ;; (if (file-exists-p abbrev-file-name)
      ;; (quietly-read-abbrev-file))
  ;; silently save new abbrevations
  ;; (setq save-abbrevs 'silently))
  ;; always enable abbrev mode
  ;; (setq-default abbrev-mode t))

;; show relative line numbers
(use-package relative-line-numbers
  :config
  (add-hook 'prog-mode-hook #'relative-line-numbers-mode))

;; evil leader
(use-package evil-leader
  :config
  (evil-leader/set-leader ",")
  ;; evil leader settings
  (evil-leader/set-key
    "f" 'find-file
    "b" 'ivy-switch-buffer
    "k" 'kill-buffer)
  (evil-commentary-mode)
  (global-evil-surround-mode 1)
  (global-evil-leader-mode)
  ;; finally load evil mode
  (evil-mode 1))

;; powerline
(powerline-default-theme)

;; snippet manager
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; enable flycheck globally
(use-package flycheck
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
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Enable elpy for python
(use-package elpy :config (elpy-enable)
(setq python-indent-offset 4))

;; Scala tools
(use-package ensime
  :config
  (setq ensime-startup-snapshot-notification nil))

;; Latex support
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; ;; allow babel to run elisp, python and sh codes
(use-package org
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
  (global-set-key (kbd "C-c q") 'auto-fill-mode))

;; htmlize for syntax highlighting in exported html
(use-package htmlize :config
  ;; syntax highlight from a css file instead of copying
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-htmlize-font-prefix "org-"))

 (setq org-todo-keywords
       '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))

;; pretty bullets for org-mode
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; export org-mode to various formats using pandoc
(with-eval-after-load 'ox
  (use-package ox-pandoc))

;; evil bindings for org-mode
(use-package evil-org)

;; Load magit last
(use-package magit
  :config
  (evil-leader/set-key "g" 'magit-status))

;; evil bindings for magit
(use-package evil-magit)

(provide 'init)
;;; init.el ends here
