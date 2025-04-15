;; Move custom set variables
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'no-error 'no-message)

;; Set backup and auto-save files directory
(setq backup-directory-alist
      '(("." . "~/.config/backup-files/")))
(make-directory "~/.config/emacs/backup-files" t)

(setq auto-save-file-name-transforms
      `((".*" "~/.config/emacs/auto-save-files/" t)))
(make-directory "~/.config/emacs/auto-save-files" t)

;; No start up message
(setq inhibit-startup-message t)

;; Remove some visuals
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

;; Transparent background
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; Line numbers
(global-display-line-numbers-mode t)

(setq display-line-numbers-type 'relative)

(dolist (mode '(org-mode-hook
                org-agenda-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default display-line-numbers-width 3)

(setq scroll-conservatively 10000)
(setq scroll-margin 8)

;; Set tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-always-indent nil)

;; Whitespace style
(add-hook 'prog-mode-hook (lambda ()
                (whitespace-mode)))
(setq-default whitespace-style
              '(face spaces empty tabs newline trailing space-mark tab-mark newline-mark))

;; Change the font
(set-face-attribute 'default nil :font "RobotoMono Nerd Font" :height 150)

;; org-agenda
(setq org-agenda-files '("~/Nextcloud/RoamNotes"))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-start-day "-1d")
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-timestamp-if-deadline-is-shown t)

(setq org-agenda-prefix-format
      '((agenda . "   %?-2i %t ")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

(add-to-list 'display-buffer-alist
             '("^\\*Org Agenda\\*"
               (display-buffer-same-window)))

(global-set-key (kbd "C-c a") 'org-agenda)

(add-to-list 'display-buffer-alist
             '("^\\*Help\\*"
               (display-buffer-same-window)))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup         nil
      straight-use-package-by-default   t)

(use-package vertico
  :init (vertico-mode)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package savehist
  :init (savehist-mode 1))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-c c l" . consult-line)
         ("C-c y" . consult-yank-from-kill-ring)))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  :init
  (global-corfu-mode 1)
  (corfu-history-mode t))

(use-package project-x
  :straight (project-x :type git :host github :repo "karthink/project-x")
  :after project
  :config
  (setq project-x-local-identifier '(".project" ".git"))
  (project-x-mode 1))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 3))

(use-package vterm
  :bind ("C-c t" . vterm)
  :init
  (setq vterm-shell "/bin/fish")
  (setq vterm-kill-buffer-on-exit t))

(use-package magit
  :bind ("C-c g" . magit))

(use-package mozc
  :config (setq default-input-method "japanese-mozc"))

(use-package eglot
  :hook (prog-mode . eglot-ensure))

(use-package yasnippet
  :init (yas-global-mode 1)
  :config (setq yas-snippet-dirs '("~/.config/emacs/snippets/"))
  :bind ("C-<tab>" . yas-expand))

(use-package org
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (load-theme 'doom-tomorrow-night t)

  (doom-themes-org-config))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "◆" "◉" "○" "◆")))

(custom-set-faces
 '(org-todo ((t (:background "light green" :foreground "black" :weight bold))))
 '(org-done ((t (:background "gray30" :foreground "white" :weight bold)))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

(use-package olivetti
  :after org
  :hook ((org-mode org-agenda-mode) . olivetti-mode))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory  "~/Nextcloud/RoamNotes")
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package zig-mode)

(setq treesit-language-source-alist
      '((bash        . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (zig         . ("https://github.com/maxxnino/tree-sitter-zig"))
        (python      . ("https://github.com/tree-sitter/tree-sitter-python"))
        (javascript  . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
        (typescript  . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (tsx         . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (json        . ("https://github.com/tree-sitter/tree-sitter-json"))
        (yaml        . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (html        . ("https://github.com/tree-sitter/tree-sitter-html"))
        (css         . ("https://github.com/tree-sitter/tree-sitter-css"))))

(setq major-mode-remap-alist
      '((c-mode             . c-ts-mode)
        (c++-mode           . c++-ts-mode)
        (python-mode        . python-ts-mode)
        (js-mode            . js-ts-mode)
        (json-mode          . json-ts-mode)
        (yaml-mode          . yaml-ts-mode)
        (html-mode          . html-ts-mode)
        (css-mode           . css-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))
