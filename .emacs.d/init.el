;;; package --- package manager for emacs
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line
;;; commentary:
;;; code:

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

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
    ("938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" default)))
 '(package-selected-packages
   (quote
    (evil-org evil-commentary esup swiper rainbow-delimiters htmlize ivy evil evil-leader evil-surround relative-line-numbers gruvbox-theme flycheck yasnippet company irony company-irony flycheck-irony elpy org org-bullets ox-rst ox-impress-js))))
 
;; set default font
(set-frame-font "Knack Nerd Font-13" nil t)

;; get rid of some ueless things
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
;;(menu-bar-mode -1)
(scroll-bar-mode -1)

;; indent with spaces
(setq-default indent-tabs-mode nil)
;; set tab width to 4
(setq-default tab-width 4)

;; silently save new abbrevations
(setq save-abbrevs 'silently)
;; always enable abbrev mode
(setq-default abbrev-mode t)

;; ivy mode
(use-package ivy
:diminish ivy-mode
:defer t
:ensure t
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
(defvar show-paren-style)
(setq show-paren-style 'expression)

;; automatically scroll compilation output
(defvar compilation-scroll-output)
(setq compilation-scroll-output t)

(use-package abbrev
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;; show relative line numbers
(use-package relative-line-numbers
:config
(add-hook 'prog-mode-hook #'relative-line-numbers-mode))

;; enable evil mode (vim emulator)
(use-package evil
:defer t
:config
(evil-mode 1)
(evil-commentary-mode)
;; evil surround
(global-evil-surround-mode 1))

;; evil leader
(use-package evil-leader 
:config             
(global-evil-leader-mode)
(evil-leader/set-leader ",")
;; evil leader settings
(evil-leader/set-key
  "e" 'find-file
  "b" 'ivy-switch-buffer
  "k" 'kill-buffer)
)

;; evil bindings for org-mode
(use-package evil-org-mode
  :mode "\\.org\\'")

;; powerline
(powerline-default-theme)

;; snippet manager
(use-package yasnippet
:diminish yas-minor-mode
:defer t
:ensure t
:config
(yas-global-mode 1))

;; htmlize for syntax highlighting in exported html
(use-package htmlize :mode "\\.org\\'")
(use-package rainbow-delimiters
  :defer t
  :config
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; fontify natively for org
(defvar org-src-fontify-natively)
(setq org-src-fontify-natively t)

;; syntax highlight from a css file instead of copying
;; emacs theme
(defvar org-html-htmlize-output-type)
(setq org-html-htmlize-output-type 'css)
(defvar org-html-htmlize-font-prefix)
(setq org-html-htmlize-font-prefix "org-")
;; preserve indentation
(defvar org-src-preserve-indentation)
(setq org-src-preserve-indentation t)

;; ;; allow babel to run elisp, python and sh codes
(use-package org
  :mode "\\.org\\'"

:config
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (sh . t)))
  )
;; evaluate code without confirm
(defvar org-confirm-babel-evaluate)
(setq org-confirm-babel-evaluate nil)

;; enable spell checking for org-mode
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; warp paragraphs for text and org mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; disable text warping with hotkey
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; enable flycheck globally
(use-package flycheck
  :diminish flycheck-mode
  :init
 (global-flycheck-mode))

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
(defvar org-html-postamble)
(setq org-html-postamble nil)

(use-package ox-rst :mode "\\.org\\'")
(use-package ox-impress-js :mode "\\.org\\'")

;; pretty bullets for org-mode
(use-package org-bullets
:config
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Irony mode for c++
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  "Recommended settings for irony-mode hook."
  (defvar irony-mode-map)
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
(defvar python-indent)
(setq python-indent 4)
(use-package elpy :mode "\\.py\\'" :config (elpy-enable))

(provide 'init)
;;; init.el ends here
