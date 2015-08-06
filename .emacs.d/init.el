;; enable melpa and marmalade
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; list the packages you want
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

(use-package zenburn-theme
  :ensure t)

(use-package go-mode
  :ensure t)

(defun zenburn-init ()
  (load-theme 'zenburn t)
)

;; Change theme to zenburn
(add-hook 'after-init-hook 'zenburn-init)

(use-package exec-path-from-shell
  :ensure t)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-env "GOPATH")

(when (eq system-type 'darwin)

  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "Consolas")
  (set-face-attribute 'default nil :height 150)
)

(use-package fill-column-indicator
  :ensure t)

(define-globalized-minor-mode
 global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)

(use-package highlight-current-line
  :ensure t)

(global-hl-line-mode t)
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
