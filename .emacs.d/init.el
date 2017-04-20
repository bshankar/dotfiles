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
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "64f2981274fd8740b794bce9feee7949ed87b88fc0b4654bd98594e1aa81abcd" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "60d4556ebff0dc94849f177b85dcb6956fe9bd394c18a37e339c0fcd7c83e4a9" "707227acad0cf8d4db55dcf1e574b3644b68eab8aca4a8ce6635c8830bc72144" "20e23cba00cf376ea6f20049022241c02a315547fc86df007544852c94ab44cb" "0f97285f9e0c7d9cad04f2130859d20d6c9b3142877b2bca52d958f4f1cf346f" "ea489f6710a3da0738e7dbdfc124df06a4e3ae82f191ce66c2af3e0a15e99b90" "c02476423672bc917cfe502b28bce059e52da8137fec9cdf8a2ff771923d7a03" "9122dfb203945f6e84b0de66d11a97de6c9edf28b3b5db772472e4beccc6b3c5" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "b8b66b57d6bb59b85416578cb1a338e3ad00a56da28e83362949cc2ca584c850" "0ad9ed23b1f323e4ba36a7f0cbef6aff66128b94faa473aacd79317fbd24abda" "7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" "db2ecce0600e3a5453532a89fc19b139664b4a3e7cbefce3aaf42b6d9b1d6214" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "fad38808e844f1423c68a1888db75adf6586390f5295a03823fa1f4959046f81" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "a36c5e55d153b4971ec547998cbd42560df175cb4ae464ce2dc78c2652bc1576" "35b76d6f1d311d4c596dd200245ad48ff729b45b27cb88d152e5a873593fcbbc" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "6e0b5911ec1e75e965130b5a683b4e2dc288651a2bdd17148caae15b4acb162b" "cd560f7570de0dcdcf06953b3f1a25145492a54f100f9c8da3b4091b469f7f02" "aae95fc700f9f7ff70efbc294fc7367376aa9456356ae36ec234751040ed9168" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "28c07d8e75ed7e2ea9154239caa4f9d3fc831cf2340ea3e307ee49b4321dc7f5" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "4156d0da4d9b715c6f7244be34f2622716fb563d185b6facedca2c0985751334" "38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" default)))
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
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(hl-sexp-background-color "#1c1f26")
 '(irony-additional-clang-options (quote ("-std=c++1z -Wall -Wextra")))
 '(linum-format " %3i ")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files
   (quote
    ("/home/ebs/Documents/org/gtd.org" "/home/ebs/Documents/org/ideas.org")))
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (swiper company-irony-c-headers 4clojure relative-line-numbers dracula-theme gruvbox-theme rainbow-blocks markdown-mode tup-mode irony-eldoc company-auctex auctex ensime ox-pandoc auto-compile evil-magit magit evil-org evil-commentary htmlize evil evil-leader evil-surround flycheck company irony company-irony flycheck-irony elpy org org-bullets)))
 '(pdf-view-midnight-colors (quote ("#969896" . "#f8eec7")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(menu-bar-mode nil)
 '(tooltip-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
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
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(window-divider-default-right-width 1)
 '(window-divider-mode t)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "simp" :slant normal :weight normal :height 128 :width normal)))))

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

;; theme
(use-package dracula-theme
  :ensure t)

;; relative line numbers
(use-package linum-relative 
  :ensure t 
  :config
  (global-relative-line-numbers-mode))

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
;; (electric-pair-mode 1)

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
    "k" 'kill-buffer
    "i" 'indent-whole-buffer)
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
(use-package elpy 
  :ensure t
  :config (elpy-enable)
  (setq python-indent-offset 4))

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

(provide 'init)
;;; init.el ends here
