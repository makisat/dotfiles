;; --- Settings --- ;;
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

;; Font & visuals
(set-face-attribute 'default nil :font "HackGen Console NF" :height 140)
;(add-hook 'prog-mode-hook #'whitespace-mode)

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

;; transparent background
(add-to-list 'default-frame-alist '(alpha . 90))

;; --- Miscellaneous Settings --- ;;
(setq org-image-actual-width nil)
(savehist-mode 1)
(global-set-key (kbd "C-S-i") 'tab-next)

(add-to-list 'display-buffer-alist
             '("^\\*.*\\*$"
               (display-buffer-reuse-mode-window)
	       (body-function . (lambda (window) (select-window window)))))

;; --- Straight Bootstrap --- ;;
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
(setq package-enable-at-startup nil)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; --- Mini Buffer --- ;;
(use-package vertico
  :init (vertico-mode)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-c c l" . consult-line)
         ("C-c y" . consult-yank-from-kill-ring)
         ("C-c r" . consult-ripgrep)))

;; --- Completion --- ;;
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
  ;; (global-corfu-mode 1)
  (corfu-history-mode t)
  :hook (prog-mode . corfu-mode))

(use-package cape
  :bind ("M-p" . cape-prefix-map))

;; -- Miscellaneous --- ;;
(use-package vterm
  :bind ("C-c t" . vterm)
  :init
  (setq vterm-shell "/bin/zsh")
  (setq vterm-kill-buffer-on-exit t))

(use-package move-text
  :config
  (move-text-default-bindings)
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))

  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice))

(use-package multiple-cursors
  :bind (("C-c m" . mc/mark-pop)
         ("C->"   . mc/mark-next-like-this)
         ("C-<"   . mc/mark-previous-like-this)
	 ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("<return>" . nil)))

(use-package orderless
  :init
  (setq completion-ignore-case t)
  :config
  (setq completion-styles '(orderless basic)))

(use-package project-x
  :straight (:host github :repo "karthink/project-x")
  :config
  (setq project-x-local-identifier '(".project" ".git"))
  (project-x-mode 1))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 3))

(use-package magit
  :bind
  ("C-c g" . magit))

(use-package mozc
  :config (setq default-input-method "japanese-mozc"))

;; --- Theme & Font --- ;;
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (load-theme 'doom-tomorrow-night t)

  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; --- Org Mode --- ;;
(use-package org
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)
  (org-agenda-mode . olivetti-mode)
  (org-mode . olivetti-mode)
  :bind (:map org-mode-map
	 ("C-c l" . org-toggle-link-display)))

(use-package olivetti)

(setq ispell-program-name "/usr/bin/hunspell")
(add-hook 'org-mode-hook 'flyspell-mode)

;; --- Org Roam --- ;;
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-node-display-template "${title} ${tags}")
  :custom
  (org-roam-directory  "~/Nextcloud/RoamNotes")
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . org-roam-buffer-toggle)))

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; --- Org Agenda --- ;;
;; org-agenda
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-files '("~/Nextcloud/RoamNotes"))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-start-day "-1d")
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-timestamp-if-deadline-is-shown t)

;; --- Programming --- ;;
(global-set-key (kbd "C-c p") 'compile)
(global-set-key (kbd "C-c P") 'project-compile)
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "C-w") 'wdired-change-to-wdired-mode))

(add-hook 'prog-mode-hook #'eglot-ensure)
(eval-after-load 'eglot
  '(add-to-list 'eglot-stay-out-of 'yasnippet))

(setq eglot-ignored-server-capabilities '(:inlayHintProvider))

(define-key prog-mode-map (kbd "C-c l d") 'eglot-find-declaration)
(define-key prog-mode-map (kbd "C-c l i") 'eglot-find-implementation)

(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(use-package yasnippet
  :init (yas-global-mode 1)
  :config (setq yas-snippet-dirs '("~/.config/emacs/snippets/"))
  :bind ("C-<tab>" . yas-expand))

(use-package zig-mode
  :init (setq zig-format-on-save nil))

(use-package gdscript-mode
    :straight (gdscript-mode
               :type git
               :host github
               :repo "godotengine/emacs-gdscript-mode"))

(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))

(use-package markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
