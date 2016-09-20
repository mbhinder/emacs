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

(ac-config-default)

(defun theme-init ()
  (load-theme 'zenburn t)
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

(setq org-agenda-files (list "~/org-notes/"))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-src-fontify-natively t)
(setq org-default-notes-file "~/org-notes/reference.org")
(define-key global-map "\C-cc" 'org-capture)
(setq org-todo-keyword-faces
      `(("TODO" . (:foreground "grey" :weight bold))
        ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
        ("WAITING" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("CANCELLED" . (:foreground "red" :weight bold))))

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

(if (eq system-type 'darwin)
    (global-set-key (kbd "C-c b") 'browse-url-default-macosx-browser)
  )

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   (quote
    ("2e5705ad7ee6cfd6ab5ce81e711c526ac22abed90b852ffaf0b316aa7864b11f" default)))
 '(elfeed-feeds
   (quote
    ("http://nullprogram.com/feed/" "http://leetcode.com/feed" "http://meowni.ca/atom.xml" "http://dave.cheney.net/feed" "http://garrett.damore.org/feeds/posts/default" "http://feeds.feedburner.com/catonmat" "http://feeds.feedburner.com/ScottHanselman" "http://feeds.feedburner.com/UnethicalBlogger" "http://sudharsh.me/atom.xml" "http://biodieselhauling.blogspot.com/feeds/posts/default" "http://www.mrsmoneymustache.com/feed/" "http://feeds.feedburner.com/RickFerri" "http://www.techmeshugana.com/feed/" "http://brownvagabonder.com/feed/" "http://feeds.feedburner.com/MadFientist" "http://feeds.feedburner.com/jlcollinsnh" "http://radicalpersonalfinance.libsyn.com/rss" "http://gooddaytolive.wordpress.com/feed/" "http://livingafi.com/feed/" "http://feeds.feedburner.com/EarlyRetirementExtreme" "http://feeds.feedburner.com/GoCurryCracker" "http://feeds.feedburner.com/MrMoneyMustache" "http://feeds.feedburner.com/FlannelGuyROI" "http://feeds.feedburner.com/NerdFitnessBlog" "http://xkcd.com/rss.xml" "http://coreos.com/atom.xml" "http://rachelbythebay.com/w/atom.xml" "http://0x74696d.com/rss.xml" "http://feeds.feedburner.com/HighScalability" "http://feeds.feedburner.com/corner-squareup-com" "http://nathanleclaire.com/index.xml" "http://zenhabits.net/feed/" "http://www.raptitude.com/feed/" "http://feeds.feedburner.com/StudyHacks" "http://feeds.feedburner.com/BrazenCareerist" "http://feeds.feedburner.com/sachac")))
 '(send-mail-function (quote smtpmail-send-it)))
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

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; AucTeX
(use-package magit
  :ensure t)

;; AucTeX
(use-package ledger-mode
  :ensure t)

;; ======================================================================
;; Latex stuff
;; ======================================================================
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-cite-format 'natbib)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(setq TeX-PDF-mode t)
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("All" "latexmk -pdf %t" TeX-run-TeX nil
                  (latex-mode doctex-mode)
                  :help "Run latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))


;; Start emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(load-file "~/ledger-pricedb/ledger-pricedb.el")
(set 'ledger-pricedb--yahoo_uri "http://download.finance.yahoo.com/d/quotes.csv?s=")
(set 'ledger-pricedb--stocks '("VGTSX" "VFIFX" "VBTLX" "VTIAX" "VTSAX" "VTSMX" "NFLX" "VSMAX"))
(set 'ledger-pricedb--pricedb "~/ledger/.pricedb")

(global-set-key (kbd "C-c s") (lambda () (interactive) (ledger-pricedb-save-pricedb)))

(global-unset-key "\C-x\C-b")

(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(global-set-key "\C-x\\" 'indent-buffer)

;; -------------------------------------
;; MAIL
;; -------------------------------------

;; mu4e
(add-to-list 'load-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)
(setq mu4e-mu-binary "/usr/local/bin/mu")

(setq mu4e-maildir "~/.mail")
(setq mu4e-view-show-images t)
(setq mu4e-html2text-command "w3m -dump -T text/html")
(setq mu4e-view-prefer-html t)
(setq mu4e-use-fancy-chars t)
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-get-mail-command "offlineimap -q")
(setq mu4e-update-interval 300)
(setq mu4e-attachment-dir  "~/downloads")
(setq mu4e-sent-messages-behavior 'delete)
(setq message-kill-buffer-on-exit t)
(setq mu4e-hide-index-messages t)
(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
(setq
 user-mail-address "preet@bhinder.me"
 user-full-name  "Preet Bhinder"
 mu4e-compose-signature
 (concat
  "Thanks,\n"
  "Preet Bhinder\n"))


(setq smtpmail-smtp-user "preetbhinder@fastmail.com")


(setq message-send-mail-function 'smtpmail-send-it
    smtpmail-stream-type 'starttls
    smtpmail-default-smtp-server "mail.messagingengine.com"
    smtpmail-smtp-server "mail.messagingengine.com"
    smtpmail-smtp-service 587)

(use-package elfeed
  :ensure t)

(global-set-key (kbd "C-c e") 'elfeed)
(global-set-key (kbd "C-c m") 'mu4e)

(require 'password-cache)
(setq password-cache-expiry nil)
