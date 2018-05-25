;; init.el --- user init file
;;; commentary:
;;; code:

(let ((file-name-handler-alist nil))
  (setq-default gc-cons-threshold 100000000)

  (require 'package)
  (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")
          ("org" . "https://orgmode.org/elpa/")))
  (package-initialize)

  (setq package-enable-at-startup nil)
  (package-initialize)

  ;; Always ensure packages exist
  (setq use-package-always-ensure t)

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (use-package auto-compile
    :config
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode))

  ;; remove the startup screen
  (setq inhibit-startup-screen t)

  ;; get rid of the annoying bell
  (setq ring-bell-function 'ignore)

  ;; indent with spaces
  (setq-default indent-tabs-mode nil)
  ;; set tab width to 2
  (setq-default tab-width 2)
  (setq indent-line-function 'insert-tab)

  (setq load-prefer-newer t)

  ;; do not highlight current line
  (global-hl-line-mode -1)

  ;; highlight matching parenthesis
  (show-paren-mode t)
  (defvar show-paren-style)
  (setq show-paren-style 'parenthesis)

  ;; automatically scroll compilation output
  (setq compilation-scroll-output t)

  ;; Close parenthesis automatically
  (electric-pair-mode 1)

  ;; replace highlighted text with typed stuff
  (delete-selection-mode 1)

  ;; Load additional files
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)
  (load "~/.emacs.d/fira.el")

  ;; apropos sort by score
  (defvar apropos-sort-by-scores)
  (setq apropos-sort-by-scores t)

  ;; scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time

  ;; do not create backup or lock files
  (setq make-backup-files nil)
  (setq create-lockfiles nil)

  ;; Faster scrolling
  (setq auto-window-vscroll nil)

  ;; trim trailing whitespaces before saving
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (use-package material-theme
    :config
    (load-theme 'material))

  (use-package telephone-line
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

  (use-package delight)
  (use-package try
    :defer 4)

  (use-package helpful
    :defer 5
    :config
    (global-set-key (kbd "C-h f") #'helpful-callable)
    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (global-set-key (kbd "C-c C-.") #'helpful-at-point))

  (use-package xah-fly-keys
    :delight xah-fly-keys
    :config
    (xah-fly-keys-set-layout "qwerty") ; required if you use qwerty
    (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
    (xah-fly-keys 1))

  (use-package multiple-cursors
    :defer 2
    :config
    (define-key xah-fly-comma-keymap (kbd ";") 'mc/mark-next-like-this)
    (define-key xah-fly-comma-keymap (kbd "h") 'mc/mark-previous-like-this)
    (define-key xah-fly-comma-keymap (kbd "i") 'mc/mark-all-dwim))

  (use-package which-key
    :delight
    :config
    (which-key-setup-side-window-bottom)
    (which-key-mode 1))

  ;; set scratch buffer's message to a random fortune
  (use-package fortune-cookie
    :config
    (setq fortune-cookie-cowsay-args  "-f tux -s")
    (fortune-cookie-mode))

  (use-package pdf-tools
    :mode ("\\.pdf\\'" . pdf-view-mode))

  (use-package counsel
    :delight ivy-mode
    :config
    (ivy-mode 1)

    ;; Slim down ivy display
    (setq ivy-count-format ""
          ivy-display-style nil
          ivy-minibuffer-faces nil)

    ;; Let ivy use flx for fuzzy-matching
    (use-package flx)
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

    ;; Use Enter on a directory to navigate into the directory, not open it with dired.
    (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)

    (defvar ivy-use-virtual-buffers)
    (setq ivy-use-virtual-buffers t)

    ;; Let projectile use ivy
    (defvar projectile-completion-system)
    (setq projectile-completion-system 'ivy)

    ;; bind counsel-ripgrep
    (define-key xah-fly-dot-keymap (kbd "a") 'counsel-rg))

  (use-package smex
    :config
    (smex-initialize))

  (use-package dumb-jump
    :defer 3
    :config
    (define-key xah-fly-dot-keymap (kbd "b") 'dumb-jump-go)
    (define-key xah-fly-dot-keymap (kbd "c") 'dumb-jump-back))

  (use-package abbrev
    :delight
    :config
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file))
    ;; silently save new abbrevations
    (setq save-abbrevs 'silently)
    ;; always enable abbrev mode
    (setq-default abbrev-mode t))

  ;; snippet manager
  (use-package yasnippet
    :delight yas-minor-mode
    :defer 2
    :config
    (yas-global-mode 1)
    (use-package yasnippet-snippets))

  ;; highlight indentation levels
  (use-package highlight-indent-guides
    :hook ((prog-mode yaml-mode) . highlight-indent-guides-mode)
    :init
    (setq highlight-indent-guides-method 'character))

  ;; keep code always indented
  (use-package aggressive-indent
    :delight aggressive-indent-mode
    :config
    (add-hook 'prog-mode-hook #'aggressive-indent-mode))

  ;; enable flycheck globally
  (use-package flycheck
    :defer 2.5
    :delight
    :config
    ;; disable jshint since we prefer eslint checking
    (defvar flycheck-eslintrc)
    (setq flycheck-eslintrc "~/.eslintrc.json")
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))

    ;; use eslint with rjsx-mode for jsx files
    (add-hook 'js2-mode-hook 'flycheck-mode)

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
    (global-flycheck-mode))

  ;; company mode
  (use-package company
    :delight
    :defer 2.5
    :config
    (progn
      (setq company-global-modes '(not gud-mode org-mode))
      (bind-key "<tab>" 'company-complete-selection company-active-map)
      (setq-default pos-tip-background-color "khaki1")
      (bind-keys :map company-active-map
                 ("C-n" . company-select-nex)
                 ("C-p" . company-select-previous)
                 ("C-d" . company-show-doc-buffer)
                 ("<tab>" . company-complete))
      (setq company-idle-delay 0
            company-require-match nil
            company-show-numbers t
            company-tooltip-align-annotations t
            company-tooltip-flip-when-above t
            company-tooltip-margin 1
            company-echo-delay 0
            company-minimum-prefix-length 2
            company-selection-wrap-around t
            company-transformers '(company-sort-by-occurrence
                                   company-sort-by-backend-importance)
            company-backends '(company-capf
                               (company-dabbrev company-dabbrev-code company-keywords)
                               company-yasnippet company-files))
      (push (apply-partially #'cl-remove-if
                             (lambda (c) (string-match-p "\\`[0-9]+\\'" c)))
            company-transformers))
    (global-company-mode 1))

  (use-package typescript-mode
    :mode ("\\.ts\\'" . typescript-mode)
    :config
    (use-package tide
      :config
      (defun setup-tide-mode ()
        (interactive)
        (tide-setup)
        (setq flycheck-check-syntax-automatically '(save mode-enabled))
        (setq typescript-indent-level 2)
        (tide-hl-identifier-mode +1))

      (setup-tide-mode)
      (add-hook 'before-save-hook 'tide-format-before-save)
      (add-hook 'typescript-mode-hook #'setup-tide-mode)))

  (use-package rust-mode
    :mode ("\\.rs\\'" . rust-mode)
    :config
    (setq rust-format-on-save t)
    (use-package flycheck-rust
      :ensure
      :config
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

    (use-package racer
      :config
      (add-hook 'rust-mode-hook #'racer-mode)
      (add-hook 'racer-mode-hook #'company-mode)
      (add-hook 'racer-mode-hook #'eldoc-mode))

    (use-package cargo
      :config
      (add-hook 'rust-mode-hook 'cargo-minor-mode)))

  (use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

  (use-package json-mode
    :mode ("\\.json\\'" . json-mode)
    :config
    (setq js-indent-level 2)
    (setq json-reformat:indent-width 2))

  (use-package yaml-mode
    :mode (("\\.yml'" . yaml-mode)
           ("\\.yaml'" . yaml-mode)))

  (use-package web-mode
    :mode (("\\.phtml\\'" . web-mode)
           ("\\.tpl\\.php\\'" . web-mode)
           ("\\.[agj]sp\\'" . web-mode)
           ("\\.as[cp]x\\'" . web-mode)
           ("\\.erb\\'" . web-mode)
           ("\\.mustache\\'" . web-mode)
           ("\\.djhtml\\'" . web-mode)
           ("\\.html?\\'" . web-mode)
           ("\\.s?css'" . web-mode)
           ("\\.xml\\'" . web-mode)
           ("\\.ejs\\'" . web-mode))

    :config
    (defun my-web-mode-hook ()
      "Hooks for Web mode. Set all indentations to 2 spaces"
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-attr-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (defvar css-indent-offset)
      (setq css-indent-offset 2))

    (add-hook 'web-mode-hook  'my-web-mode-hook)
    (setq web-mode-enable-auto-expanding t)

    (use-package emmet-mode
      :config
      (add-hook 'sgml-mode-hook 'emmet-mode)
      (add-hook 'web-mode-hook 'emmet-mode)))

  (use-package org
    :mode ("\\.org\\'" . org-mode)
    :init
    ;; (setq org-pretty-entities t)
    (setq org-export-with-timestamps nil)
    :config
    ;; latex options
    (setq org-format-latex-header "\\documentclass{article}\n\\usepackage[usenames]{color}\n[PACKAGES]\n[DEFAULT-PACKAGES]\n\\pagestyle{empty}             % do not remove\n% The settings below are copied from fullpage.sty\n\\setlength{\\textwidth}{\\paperwidth}\n\\addtolength{\\textwidth}{-3cm}\n\\setlength{\\oddsidemargin}{1.5cm}\n\\addtolength{\\oddsidemargin}{-2.54cm}\n\\setlength{\\evensidemargin}{\\oddsidemargin}\n\\setlength{\\textheight}{\\paperheight}\n\\addtolength{\\textheight}{-\\headheight}\n\\addtolength{\\textheight}{-\\headsep}\n\\addtolength{\\textheight}{-\\footskip}\n\\addtolength{\\textheight}{-3cm}\n\\setlength{\\topmargin}{1.5cm}\n\\addtolength{\\topmargin}{-2.54cm}\\everymath{\\displaystyle}")
    (plist-put org-format-latex-options :scale 2.0)

    ;; display all inline images
    (setq org-startup-with-inline-images t)
    ;; update inline images on execute code block
    (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

    ;; open html exported file in firefox
    '(org-file-apps
      (quote
       ((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.html\\'" . "/usr/bin/firefox-developer %s")
        ("\\.pdf\\'" . default))))

    ;; set generic browser
    (defvar browse-url-generic-program)
    (setq browse-url-generic-program
          (executable-find "firefox-developer-edition")
          browse-url-browser-function 'browse-url-generic)

    ;; let babel execute python and sh in org documents
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       (dot . t)
       (shell . t)))

    ;; custom ellipsis for org-mode (...)
    (set-display-table-slot standard-display-table
                            'selective-display (string-to-vector " ⤵"))

    ;; fontify natively for org
    (setq org-src-fontify-natively t)

    ;; hide emphasis markers
    (setq org-hide-emphasis-markers t)

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

    ;; refile targets
    (setq org-refile-targets '(("~/Documents/org/gtd/gtd.org" :maxlevel . 3)
                               ("~/Documents/org/gtd/someday.org" :level . 1)
                               ("~/Documents/org/gtd/tickler.org" :maxlevel . 2)))

    (define-key xah-fly-t-keymap (kbd "a") 'org-agenda-list)
    (define-key xah-fly-t-keymap (kbd "b") 'org-capture)

    ;; htmlize for syntax highlighting in exported html
    (use-package htmlize
      :config
      ;; syntax highlight from a css file instead of copying
      (setq org-html-htmlize-output-type 'css)
      (setq org-html-htmlize-font-prefix "org-"))

    ;; pretty bullets for org-mode
    (use-package org-bullets
      :config
      (add-hook 'org-mode-hook 'org-bullets-mode))

    ;; export org-mode to various formats using pandoc
    (with-eval-after-load 'ox
      (use-package ox-twbs :ensure)
      (use-package ox-reveal
        :init
        (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/")
        (setq org-reveal-mathjax t))))

  ;; Load magit last
  (use-package magit
    :defer 4
    :delight auto-revert-mode
    :config
    (define-key xah-fly-leader-key-map (kbd "b") 'magit-status)
    (use-package gist))

  (provide 'init))

;;; init.el ends here
