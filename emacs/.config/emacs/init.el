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

;; --- Elpaca Bootstrap --- ;;
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; --- Mini Buffer --- ;;
(use-package vertico :ensure t
  :init (vertico-mode)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia :ensure t
  :init
  (marginalia-mode))

(use-package consult :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-c c l" . consult-line)
         ("C-c y" . consult-yank-from-kill-ring)
         ("C-c r" . consult-ripgrep)))

;; --- Completion --- ;;
(use-package corfu :ensure t
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

;; -- Miscellaneous --- ;;
(use-package vterm :ensure t
  :bind ("C-c t" . vterm)
  :init
  (setq vterm-shell "/bin/bash")
  (setq vterm-kill-buffer-on-exit t))

(use-package move-text :ensure t
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

(use-package multiple-cursors :ensure t
  :bind (("C-c m" . mc/mark-pop)
         ("C->"   . mc/mark-next-like-this)
         ("C-<"   . mc/mark-previous-like-this)
	 ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("<return>" . nil)))

(use-package orderless :ensure t
  :init
  (setq completion-ignore-case t)
  :config
  (setq completion-styles '(orderless basic)))

(use-package project-x :ensure t
  (:host github :repo "karthink/project-x")
  :config
  (setq project-x-local-identifier '(".project" ".git"))
  (project-x-mode 1))

(use-package which-key :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 3))

(use-package magit :ensure t
  :bind
  ("C-c g" . magit))

(use-package transient :ensure t)

(use-package mozc :ensure t
  :config (setq default-input-method "japanese-mozc"))

;; --- Theme & Font --- ;;

(use-package doom-themes :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (load-theme 'doom-tomorrow-night t)

  (doom-themes-org-config))

(use-package doom-modeline :ensure t
  :init (doom-modeline-mode 1))

;; font
(use-package ligature :ensure t
  :init
  (defconst my/jetbrains-ligatures
    '("->" "<-" "-->" "<--" "=>" "<="
      "==" "!=" "/=" "~=" "www" "&&" "||"
      "/**" "/*" "*/" "///" "{-" "-}"
      "++" "+++" "**" "***" "?:" "f_i" "f_l"
      "::" ":::" ".." "..." ".?" "#{" "}#"
      "#(" ")#" "#[" "]#" "#<" ">#" "#?"
      "|>" "<|" "|-" "-|" "|=" "=|" "[|" "|]"
      "||-" "-||" "||=" "=||" "<>" "</" "/>"
      ":=" "<<<" ">>>" "<<=" ">>=" ".."
      "=/=" "<-<" "<<-" "<-<" "->>" ">->"))
  :config
  (ligature-set-ligatures 'prog-mode my/jetbrains-ligatures)
  (ligature-set-ligatures 'org-mode my/jetbrains-ligatures)
  :hook ((prog-mode . ligature-mode)
	 (org-mode . ligature-mode)))

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 140)

(add-hook 'prog-mode-hook #'whitespace-mode)

;; --- Org Mode --- ;;
(use-package org :ensure t
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode))

(setq ispell-program-name "/usr/bin/hunspell")
(add-hook 'org-mode-hook 'flyspell-mode)

;; --- Org Roam --- ;;
(use-package org-roam :ensure t
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

(use-package org-roam-ui :ensure t
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
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

(add-hook 'prog-mode-hook #'eglot-ensure)

(use-package yasnippet :ensure t
  :init (yas-global-mode 1)
  :config (setq yas-snippet-dirs '("~/.config/emacs/snippets/"))
  :bind ("C-<tab>" . yas-expand))

