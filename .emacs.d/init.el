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
 '(ansi-term-color-vector
   [unspecified "#14191f" "#d15120" "#81af34" "#deae3e" "#7e9fc9" "#a878b5" "#7e9fc9" "#dcdddd"] t)
 '(custom-enabled-themes (quote (jbeans)))
 '(custom-safe-themes
   (quote
    ("ea489f6710a3da0738e7dbdfc124df06a4e3ae82f191ce66c2af3e0a15e99b90" "c02476423672bc917cfe502b28bce059e52da8137fec9cdf8a2ff771923d7a03" "9122dfb203945f6e84b0de66d11a97de6c9edf28b3b5db772472e4beccc6b3c5" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "b8b66b57d6bb59b85416578cb1a338e3ad00a56da28e83362949cc2ca584c850" "0ad9ed23b1f323e4ba36a7f0cbef6aff66128b94faa473aacd79317fbd24abda" "7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" "db2ecce0600e3a5453532a89fc19b139664b4a3e7cbefce3aaf42b6d9b1d6214" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "fad38808e844f1423c68a1888db75adf6586390f5295a03823fa1f4959046f81" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "a36c5e55d153b4971ec547998cbd42560df175cb4ae464ce2dc78c2652bc1576" "35b76d6f1d311d4c596dd200245ad48ff729b45b27cb88d152e5a873593fcbbc" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "6e0b5911ec1e75e965130b5a683b4e2dc288651a2bdd17148caae15b4acb162b" "cd560f7570de0dcdcf06953b3f1a25145492a54f100f9c8da3b4091b469f7f02" "aae95fc700f9f7ff70efbc294fc7367376aa9456356ae36ec234751040ed9168" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "28c07d8e75ed7e2ea9154239caa4f9d3fc831cf2340ea3e307ee49b4321dc7f5" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "4156d0da4d9b715c6f7244be34f2622716fb563d185b6facedca2c0985751334" "38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#9876aa" :underline
          (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
                         (:color "#808080"))
     (implicitParams :underline
                     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6"))))
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#37474f" t)
 '(flycheck-clang-args (quote ("-std=c++1z")))
 '(gnus-logo-colors (quote ("#4c8383" "#bababa")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(hl-sexp-background-color "#1c1f26")
 '(irony-additional-clang-options (quote ("-std=c++1z -Wall -Wextra")))
 '(linum-format " %3i ")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (jbeans-theme rainbow-blocks markdown-mode tup-mode irony-eldoc swiper company-auctex auctex ensime ox-pandoc auto-compile evil-magit magit evil-org evil-commentary htmlize evil evil-leader evil-surround flycheck yasnippet company irony company-irony flycheck-irony elpy org org-bullets)))
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
 '(vc-annotate-very-old-color nil)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t))


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

(defun indent-whole-buffer ()
  "Indent the whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; force horizontal split always
(setq split-width-threshold 9999)

;; ivy mode
(use-package ivy
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

;; evil leader
(use-package evil-leader
  :diminish undo-tree-mode
  :diminish auto-revert-mode
  :diminish evil-commentary-mode
  :config
  (evil-leader/set-leader ",")
  ;; evil leader settings
  (evil-leader/set-key
    "f" 'find-file
    "b" 'ivy-switch-buffer
    "k" 'kill-buffer
    "i" 'indent-whole-buffer)
  (evil-commentary-mode)
  (global-evil-surround-mode 1)
  (global-evil-leader-mode)
  ;; finally load evil mode
  (evil-mode 1))

;; powerline
(powerline-default-theme)

;; snippet manager
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; enable flycheck globally
(use-package flycheck
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
  (require 'ox-md nil t)
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
