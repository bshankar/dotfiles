(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (gruvbox)))
 '(custom-safe-themes
   (quote
    ("10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" default)))
 '(package-selected-packages
   (quote
    (rainbow-delimiters htmlize icicles ivy evil org-evil evil-leader evil-surround relative-line-numbers powerline-evil gruvbox-theme flycheck yasnippet company irony company-irony flycheck-irony elpy org org-bullets ox-rst ox-impress-js))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Knack Nerd Font" :foundry "simp" :slant normal :weight normal :height 128 :width normal)))))


;; get rid of some ueless things
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
;;(menu-bar-mode -1)
(scroll-bar-mode -1)

;; open html files with firefox
 '(org-file-apps
    (quote
      ((auto-mode . emacs)
      ("\\.mm\\'" . default)
      ("\\.x?html?\\'" . "/usr/bin/firefox %s")
      ("\\.pdf\\'" . default))))

;; indent with spaces
(setq-default indent-tabs-mode nil)
;; set tab width to 4
(setq-default tab-width 4)

;; abbreviation table
(define-abbrev-table 'global-abbrev-table
  '((";name" "Bhavani Shankar")
    (";email" "ebs@openmailbox.org")
    ))

;; always enable abbrev mode
(setq-default abbrev-mode t)

;; icicles minibuffer completion
(require 'icicles)
(icy-mode 1)

;; use pretty symbols
(global-prettify-symbols-mode t)

;; highlight current line
(global-hl-line-mode)

;; highlight matching parenthesis
(show-paren-mode t)
(defvar show-paren-style)
(setq show-paren-style 'expression)

;; automatically scroll compilation output
(defvar compilation-scroll-output)
(setq compilation-scroll-output t)

;; show relative line numbers
(require 'relative-line-numbers)
(global-relative-line-numbers-mode)

;; leader key emulation
(require 'evil-leader)
;; set leader key
(evil-leader/set-leader ",")
(global-evil-leader-mode)
;; evil surround
(require 'evil-surround)
(global-evil-surround-mode 1)
;; org-evil
(require 'org-evil)

;; finally enable evil mode (vim emulator)
(require 'evil)
  (evil-mode 1)

;; powerline
(require 'powerline)
(powerline-default-theme)
(require 'powerline-evil)

;; snippet manager
(require 'yasnippet)
(yas-global-mode 1)

;; htmlize for syntax highlighting in exported html
(require 'htmlize)
(require 'rainbow-delimiters)

;; fontify natively for org
(setq org-src-fontify-natively t)

;; syntax highlight from a css file instead of copying
;; emacs theme
(defvar org-html-htmlize-output-type)
(setq org-html-htmlize-output-type 'css)
(defvar org-html-htmlize-font-prefix)
(setq org-html-htmlize-font-prefix "org-")
;; preserve indentation
(setq org-src-preserve-indentation t)

;; allow babel to run elisp, python and gnuplot codes
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (gnuplot . t)))

;; evaluate code without confirm
(setq org-confirm-babel-evaluate nil)

;; enable spell checking for org-mode
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; warp paragraphs for text and org mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; disable text warping with hotkey
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; check written text using proselint
(require 'flycheck)

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
(add-hook 'org-mode-hook #'flycheck-mode)

;; don't include author info at the bottom of every html
(setq org-html-postamble nil)

(require 'ox-rst)
(require 'ox-impress-js)

;; pretty bullets for org-mode
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(eval-when-compile
  (require 'use-package))

;; enable flycheck for all files
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; load the dark theme by default
;; (load-theme 'gruvbox t)

;; Irony mode for c++
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; use comany backend for auto completion everywhere
(add-hook 'after-init-hook 'global-company-mode)
(defvar company-backends)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Enable elpy for python
(setq python-indent 4)
(require 'elpy)
(elpy-enable)

