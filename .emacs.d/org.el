(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-export-with-timestamps nil)
  :config
  (setq org-format-latex-header "\\documentclass{article}\n\\usepackage[usenames]{color}\n[PACKAGES]\n[DEFAULT-PACKAGES]\n\\pagestyle{empty}             % do not remove\n% The settings below are copied from fullpage.sty\n\\setlength{\\textwidth}{\\paperwidth}\n\\addtolength{\\textwidth}{-3cm}\n\\setlength{\\oddsidemargin}{1.5cm}\n\\addtolength{\\oddsidemargin}{-2.54cm}\n\\setlength{\\evensidemargin}{\\oddsidemargin}\n\\setlength{\\textheight}{\\paperheight}\n\\addtolength{\\textheight}{-\\headheight}\n\\addtolength{\\textheight}{-\\headsep}\n\\addtolength{\\textheight}{-\\footskip}\n\\addtolength{\\textheight}{-3cm}\n\\setlength{\\topmargin}{1.5cm}\n\\addtolength{\\topmargin}{-2.54cm}\\everymath{\\displaystyle}")
  (plist-put org-format-latex-options :scale 2.0)

  (setq org-startup-with-inline-images t)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  (defvar browse-url-generic-program)
  (setq browse-url-generic-program
        (executable-find "firefox-developer-edition")
        browse-url-browser-function 'browse-url-generic)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (dot . t)
     (shell . t)))

  (set-display-table-slot standard-display-table
                          'selective-display (string-to-vector " ⤵ "))

  (setq org-src-fontify-natively t)
  (setq org-hide-emphasis-markers t)

  (let* ((base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold))))

  (setq org-src-preserve-indentation t)
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (global-set-key (kbd "C-c q") 'auto-fill-mode)

  (setq org-todo-keywords
        '((sequence "NEXT(n)" "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)" "FAILED(f)")))

  (use-package htmlize
    :config
    (setq org-html-htmlize-output-type 'css)
    (setq org-html-htmlize-font-prefix "org-"))

  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook 'org-bullets-mode))

  (with-eval-after-load 'ox
    (use-package ox-twbs)
    (use-package ox-reveal
      :init
      (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/")
      (setq org-reveal-mathjax t))))
