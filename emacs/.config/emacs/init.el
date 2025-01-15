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
(setq-default whitespace-style
              '(face spaces empty tabs newline trailing space-mark tab-mark newline-mark))

;; Change the font
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 130)

(use-package vertico
  :init (vertico-mode 1)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package orderless
  :config
  ;; Set Orderless as the primary completion style
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion))))

  ;; Enable case-insensitive matching
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)

  ;; Define matching styles for Orderless
  (setq orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp)))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-c c l" . consult-line)
         ("C-c y" . consult-yank-from-kill-ring)))

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode 1)
  (corfu-history-mode t))

(use-package savehist
  :init (savehist-mode 1))

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

(use-package smartparens
  :hook (prog-mode text-mode org-mode)
  :config
  (require 'smartparens-config))

(use-package mozc
  :config (setq default-input-method "japanese-mozc"))

(use-package eglot
  :hook (prog-mode . eglot-ensure))

(use-package yasnippet
  :init (yas-global-mode 1)
  :config (setq yas-snippet-dirs '("~/.config/emacs/snippets/"))
  :bind ("C-<tab>" . yas-expand))

(add-hook 'prog-mode-hook (lambda ()
                            (whitespace-mode)))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(add-hook 'go-ts-mode-hook (lambda ()
                             (setq tab-width 4)
                             (setq go-ts-mode-indent-offset 4)
                             (setq indent-tabs-mode nil)))

(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                    "master" "typescript/src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript"
                    "master" "tsx/src")))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(add-hook 'tsx-ts-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq tsx-ts-mode-indent-offset 4)
            (setq indent-tabs-mode nil)))

(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq typescript-ts-mode-indent-offset 4)
            (setq indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
(use-package zig-mode)

(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))

(add-hook 'c-ts-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq c-ts-mode-indent-offset 4)
            (setq indent-tabs-mode nil)))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (load-theme 'doom-tomorrow-night t)

  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package org
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode))


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
