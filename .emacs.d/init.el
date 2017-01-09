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
 '(package-selected-packages
   (quote
    (org-bullets org relative-line-numbers ## company-irony elpy flycheck-irony company irony powerline-evil ivy yasnippet flycheck gruvbox-theme org-evil evil-leader evil-surround evil)))
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Knack Nerd Font" :foundry "simp" :slant normal :weight normal :height 130 :width normal)))))

;; get rid of startup screen
(setq inhibit-startup-screen t)

 ;; Disable the bell
(setq ring-bell-function 'ignore)

;; disable menu bar
(menu-bar-mode -1)
(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)

;; highlight matching parenthesis
(show-paren-mode t)
(defvar show-paren-style)
(setq show-paren-style 'expression)

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

(eval-when-compile
  (require 'use-package))

;; real time error checker
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; load the dark theme by default
(load-theme 'gruvbox t)

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

;; use comany backend for auto completion
(defvar company-backends)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Enable elpy for python
(require 'elpy)
(elpy-enable)

;; pretty bullets for org-mode
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
