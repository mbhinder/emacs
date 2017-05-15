;; enable melpa and marmalade
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; list the packages you want
(defvar package-list)
(setq package-list '(use-package))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)

(use-package color-theme
  :ensure t)

(use-package magit
  :ensure t)

;;; Package for quickly finding notes
(use-package deft
  :ensure t)
(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/org-notes")
(global-set-key [f8] 'deft-find-file)

(use-package git-gutter
  :ensure t)

(use-package flycheck
  :ensure t)

;;; Enable auto completion
(use-package auto-complete
  :ensure t)

;; Navigate project files
(use-package helm
  :ensure t)

(use-package helm-projectile
  :ensure t)

(helm-projectile-on)

;;; Install YASnippet
(use-package yasnippet
  :ensure t)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
(global-set-key (kbd "C-c C-s") 'yas-insert-snippet)
(global-set-key (kbd "C-c C-n") 'yas-new-snippet)
(global-set-key (kbd "C-c C-g") 'helm-ag-project-root)

(ac-config-default)

(defun theme-init ()
  (load-theme 'tango-dark t)
  )

;; Change theme to zenburn
(add-hook 'after-init-hook 'theme-init)

(use-package exec-path-from-shell
  :ensure t)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-env "GOPATH")

(when (eq system-type 'darwin)

  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "SourceCodePro-Regular")
  (set-face-attribute 'default nil :height 120)
  )

(helm-mode t)

;; Don't blink the cursor
(blink-cursor-mode 0)

;; Highlight matching parentheses
(show-paren-mode t)

;; Set tabs to 4 spaces
(setq-default indent-tabs-mode nil)
(setq standard-indent 4)
(setq tab-width 4)
(setq sgml-basic-offset 4)

;; Prettify programming languages syntax
(global-prettify-symbols-mode)

;; Display column number
(column-number-mode)

;; Fuzzy matching plugin for IDO
(use-package flx-ido
  :ensure t)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Enable line numbers like vim
(global-linum-mode t)

;; Enable projectile globally
(projectile-global-mode)

;; Go specific stuff
(use-package go-mode
  :ensure t)
(use-package go-autocomplete
  :ensure t)
(use-package go-eldoc
  :ensure t)
(defun go-mode-setup ()
  (go-eldoc-setup)
  (local-set-key (kbd "M-.") 'godef-jump)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 4)
  (setq indent-tabs-mode 1)
  (setq gofmt-command "goimports")
  )

(add-hook 'go-mode-hook 'go-mode-setup)

;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done t)

(setq org-src-fontify-natively t)
(setq org-default-notes-file "~/org-notes/reference.org")
(define-key global-map "\C-cc" 'org-capture)
(setq org-todo-keyword-faces
      `(("TODO" . (:foreground "grey" :weight bold))
        ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
        ("WAITING" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("CANCELLED" . (:foreground "red" :weight bold))))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "|" "WAITING" "DONE" "CANCELLED")))

(defun my-after-load-org ()
  (add-to-list 'org-modules 'org-habit))
(eval-after-load "org" '(my-after-load-org))

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)
                             (ledger . t)
                             (C . t)
                             (sh . t)
                             (emacs-lisp . t)
                             ))

;; capture-templates
(setq org-capture-templates '(
                              ("n" "Quick reference note"
                               entry (file+datetree (format "~/org-notes/%s_notes.org" (format-time-string "%Y")))
                               "* Note: %?\n\n  %i\n\n  From: %a"
                               :empty-lines 1)
                              ("o" "Quick reference note - work"
                               entry (file+datetree (format "~/org-notes/%s_work_notes.org" (format-time-string "%Y")))
                               "* Note: %?\n\n  %i\n\n  From: %a"
                               :empty-lines 1)
                              ("c" "Personal chore"
                               entry (file "~/org-notes/chores.org")
                               "* TODO %?  %i\n  From: %a"
                               :empty-lines 1)
                              ("w" "Work task"
                               entry (file "~/org-notes/work.org")
                               "* TODO %?  %i\n  From: %a"
                               :empty-lines 1))
      )

(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key (kbd "C-x g") 'magit-status)

(global-git-gutter-mode +1)
(when (fboundp 'winner-mode)
  (winner-mode 1))

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)

;; when cursor is on edge, move to the other side, as in a torus space
(setq windmove-wrap-around t )

(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
  )

(global-set-key [f5] 'refresh-file)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package magit
  :ensure t)

;; ======================================================================
;; Latex stuff
;; Start emacs server

(require 'server)
(unless (server-running-p)
  (server-start))

(global-unset-key "\C-x\C-b")

(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(global-set-key "\C-x\\" 'indent-buffer)

;; Cache  ssh passwords
(require 'password-cache)
(setq password-cache-expiry nil)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
