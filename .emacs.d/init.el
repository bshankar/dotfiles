; init.el --- user init file      -*- no-byte-compile: t -*-

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;;; commentary:
;;; code:

(setq package-enable-at-startup nil) (package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#151515" "#fb9fb1" "#acc267" "#ddb26f" "#6fc2ef" "#e1a3ee" "#6fc2ef" "#d0d0d0"])
 '(ansi-term-color-vector
   [unspecified "#151515" "#fb9fb1" "#acc267" "#ddb26f" "#6fc2ef" "#e1a3ee" "#6fc2ef" "#d0d0d0"])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("d5f17ae86464ef63c46ed4cb322703d91e8ed5e718bf5a7beb69dd63352b26b2" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "4980e5ddaae985e4bae004280bd343721271ebb28f22b3e3b2427443e748cd3f" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "eea01f540a0f3bc7c755410ea146943688c4e29bea74a29568635670ab22f9bc" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "d83e34e28680f2ed99fe50fea79f441ca3fddd90167a72b796455e791c90dc49" "5673c365c8679addfb44f3d91d6b880c3266766b605c99f2d9b00745202e75f6" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "a49760e39bd7d7876c94ee4bf483760e064002830a63e24c2842a536c6a52756" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(fci-rule-color "#3C3D37")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(hl-sexp-background-color "#efebe9")
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (xah-fly-keys color-theme-sanityinc-tomorrow use-package smart-compile py-autopep8 powerline ox-pandoc org-bullets org-brain markdown-mode linum-relative irony-eldoc htmlize flycheck-irony evil-surround evil-org evil-magit evil-leader evil-easymotion evil-commentary ensime elpy company-irony-c-headers company-irony color-theme clang-format all-the-icons)))
 '(pos-tip-background-color "#A6E22E" t)
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(eval-when-compile
  (require 'use-package))

(setq load-prefer-newer t)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

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

(defun indent-whole-buffer ()
  "Indent the whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; relative line numbers
;; (use-package linum-relative
;;   :ensure t
;;   :config
;;   (linum-on)
;;   (linum-relative-on))

;; ivy mode
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  )

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

;; evil mode
(use-package evil-mode
  :config
  (evil-mode 1))

;; evil leader
(use-package evil-leader
  :ensure t
  :diminish undo-tree-mode
  :diminish auto-revert-mode
  :config
  (evil-leader/set-leader ",")
  ;; evil leader settings
  (evil-leader/set-key
    "f" 'find-file
    "b" 'ivy-switch-buffer
    "k" 'kill-buffer)
  (global-evil-leader-mode)
  ;; finally load evil mode
  (evil-mode 1))

(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-easymotion
  :ensure t
  :config
  (evilem-default-keybindings "SPC"))

;; powerline
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;; snippet manager
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
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
  :ensure t
  :config
  (evil-leader/set-key
   "c" 'smart-compile)
)

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
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package irony-eldoc
  :ensure t)

(use-package clang-format
  :ensure t
  :config
  (evil-leader/set-key
    "i" 'clang-format-buffer)
  (setq clang-format-style-option "llvm"))


;; Enable elpy for python
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq python-indent-offset 4)
  (highlight-indentation-mode nil))


;; Scala tools
(use-package ensime
  :ensure t
  :config
  (setq ensime-startup-snapshot-notification nil))

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
  (use-package ox-pandoc :ensure t))

;; evil bindings for org-mode
(use-package evil-org
  :ensure t
  )

;; Load magit last
(use-package magit
  :ensure t
  :config
  (evil-leader/set-key "g" 'magit-status))

(use-package evil-magit
  :ensure t)

(provide 'init)
;;; init.el ends here
