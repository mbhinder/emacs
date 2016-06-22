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

(use-package git-gutter
  :ensure t)

(use-package flycheck
  :ensure t)

;;; Enable auto completion
(use-package auto-complete
  :ensure t)

;;; Enable solarized dark theme
(use-package solarized-theme
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

(ac-config-default)

(defun theme-init ()
  (load-theme 'solarized-dark t)
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
  (set-face-attribute 'default nil :family "Monaco")
  (set-face-attribute 'default nil :height 130)
)

(use-package highlight-current-line
  :ensure t)

(global-hl-line-mode t)
(helm-mode t)
(setq highlight-current-line-globally t)
(setq highlight-current-line-high-faces nil)
(setq highlight-current-line-whole-line nil)
(setq hl-line-face (quote highlight))

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
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/Dropbox/org/work.org"
                             "~/Dropbox/org/home.org"))
(setq org-src-fontify-natively t)
(setq org-default-notes-file "~/Dropbox/org/inbox.org")
(define-key global-map "\C-cc" 'org-capture)

(setq org-todo-keyword-faces
      `(("TODO" . (:foreground "blue" :weight bold))
      ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
      ("WAITING" . (:foreground "orange" :weight bold))
      ("DONE" . (:foreground "green" :weight bold))
      ("CANCELLED" . (:foreground "red" :weight bold))))

(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key (kbd "C-x g") 'magit-status)

(global-git-gutter-mode +1)
(when (fboundp 'winner-mode)
(winner-mode 1))

(if (eq system-type 'darwin)
 (global-set-key (kbd "C-c b") 'browse-url-default-macosx-browser)
)

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2e5705ad7ee6cfd6ab5ce81e711c526ac22abed90b852ffaf0b316aa7864b11f" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)

;; when cursor is on edge, move to the other side, as in a torus space
(setq windmove-wrap-around t )

(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
  )

(global-set-key [f5] 'refresh-file)
