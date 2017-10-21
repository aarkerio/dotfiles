;; Manuel Montoya .emacs file 2006-2017
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- lexical-binding: t -*-

(defconst d/emacs-start-time (current-time))
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold 800000)))

;;(set-default-font "Fira Mono-11")
;; (set-default-font "Inconsolata-12")
(set-default-font "Hack-11")

(show-paren-mode 1)   ;; Show parentesis
(global-linum-mode 1) ;; always show line numbers
(column-number-mode 1)
(global-hl-line-mode 1)
(global-visual-line-mode 1)  ;; Proper line wrapping

;; Spaces nos real tabs
(setq-default indent-tabs-mode nil
              line-spacing 1
              tab-width 2
              c-basic-offset 2
              cursor-type 'box
              cursor-in-non-selected-windows nil
              bidi-display-reordering nil
              show-trailing-whitespace t
              truncate-lines t)

;;  Change Tab Width
(setq default-tab-width 2
      tab-width 2
      indent-tabs-mode t
      c-basic-offset 2
      default-major-mode 'text-mode
      transient-mark-mode t                  ;;Colors for selections (mark region)
      user-mail-address "mmontoya@gmail.com"
      user-full-name "Manuel Montoya"
      save-interprogram-paste-before-kill t
      inhibit-splash-screen t         ;; Disable splash screen
      visible-bell t                  ;; Flashes on error
      TeX-PDF-mode t                  ;; PDF mode (rather than DVI-mode)
      standard-indent 2
      package-enable-at-startup nil
      split-width-threshold 9999     ;; split horizontal always
      scroll-conservatively 20       ;; move minimum when cursor exits view, instead of recentering
      load-prefer-newer t)           ;; Don't load outdated byte code

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Change the echo message
(defun display-startup-echo-area-message ()
  (message "Herrlicher Mann ist bereit, einen erstaunlichen Job zu liefern!"))

(setq default-directory (if (string= system-name "pav23")
    "/home/manuel/entwicklung/chipotle/"
    "/home/mmontoya/entwicklung/chipotle/"))

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'diminish)    ;; Hiding or abbreviation of the mode line displays (lighters) of minor-modes
(require 'bind-key)   ;; A simple way to manage personal keybindings

;; (use-package twilight-bright-theme
;;   :ensure t
;;   :config (load-theme 'twilight-bright t))

(use-package color-theme-sanityinc-solarized
  :ensure t
  :config (load-theme 'solarized t))

(use-package auto-complete
  :ensure t)

(use-package origami
  :ensure t
  :bind (("C-c O O" . origami-mode)
         ("C-c t"  . origami-toggle-node))
  :mode
  (("\\.cjls" . origami-mode)
   ("\\.cjl"  . origami-mode)
   ("\\.js"   . origami-mode)
   ("\\.rb"   . origami-mode))
  :init
  (dolist (hook '(js-mode-hook clojure-mode-hook
                  ruby-mode-hook))
        (add-hook hook #'origami-mode))
  :config (setq whitespace-line-column nil)
  :diminish origami-mode)

;; (use-package paredit
;;   :ensure t
;;   :init
;;   (progn
;;     (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
;;     (add-hook 'clojure-mode-hook 'paredit-mode)
;;     (add-hook 'clojurescript-mode-hook 'paredit-mode)
;;     (add-hook 'clojurec-mode-hook 'paredit-mode)
;;     (add-hook 'cider-repl-mode-hook 'paredit-mode)))

(use-package avy
  :ensure t
  :bind
  (("C-." . avy-goto-word-1)
   ("C-," . avy-goto-char-2))
  :config (setq avy-all-windows nil))

(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window)
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package clojure-snippets
  :ensure t)

(use-package clojure-mode
  :ensure t
  :mode (("\\.edn$"  . clojure-mode)
         ("\\.clj$"  . clojure-mode)
         ("\\.cljs$" . clojure-mode))
  :bind (("C-c d f" . cider-code)
         ("C-c d g" . cider-grimoire)
         ("C-c d w" . cider-grimoire-web)
         ("C-c d c" . clojure-cheatsheet)
         ("C-c d d" . dash-at-point))
  :init
  (defconst clojure--prettify-symbols-alist
    '(("fn"   . ?λ)
      ("__"   . ?⁈)))
  :config
    (progn
      (setq clojure-align-forms-automatically t)
      (define-clojure-indent
        (defroutes 'defun)
        (GET 2)
        (POST 2)
        (PUT 2)
        (DELETE 2)
        (HEAD 2)
        (ANY 2)
        (context 2)
        (let-routes 1))

      (define-clojure-indent
        (s/fdef 1))

      (defun toggle-nrepl-buffer ()
        "Toggle the nREPL REPL on and off"
        (interactive)
        (if (string-match "cider-repl" (buffer-name (current-buffer)))
            (delete-window)
          (cider-switch-to-repl-buffer)))

      (defun cider-save-and-refresh ()
        (interactive)
        (save-buffer)
        (call-interactively 'cider-refresh))

      (defun cider-eval-last-sexp-and-append ()
        (interactive)
        (cider-eval-last-sexp '(1)))

      (add-hook 'clojure-mode-hook 'global-prettify-symbols-mode)))

(use-package cider
  :ensure t
  :config
  (progn
    (setq nrepl-hide-special-buffers t)
    (setq cider-popup-stacktraces-in-repl t)
    (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-auto-select-error-buffer nil)
    (setq cider-prompt-save-file-on-load nil)
    (setq cider-repl-display-help-banner nil)
    (setq cider-repl-use-pretty-printing t)
    (setq cider-refresh-before-fn "reloaded.repl/suspend")
    (setq cider-refresh-after-fn "reloaded.repl/resume")
    (setq cider-cljs-lein-repl "(do (reloaded.repl/go) (user/cljs-repl))")))

(use-package yasnippet
  :ensure t
  :init
  (progn
    (yas-global-mode 1)
    (use-package clojure-snippets)))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; Powerline
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package yasnippet
  :ensure t)

(use-package magit
  :ensure t
  :bind (([(shift f6)] . magit-status)))

;; use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Whitespace
(use-package whitespace
  :bind (("C-c T w" . whitespace-mode))
  :init
  (dolist (hook '(conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config (setq whitespace-line-column nil)
  :diminish whitespace-mode)

;; Imenu
(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)))

(use-package org-bullets
  :ensure t)

(use-package org
  :mode (("\\.org$" . org-mode))
  :config
  (dolist (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "STAGING" "DONE")))
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
     ))

(use-package multiple-cursors
  :ensure t
  :bind
  ( ;; Multiple continum lines
    ([(super shift f10)] . mc/edit-lines)
    ;; Multiple cursors not based on continuous lines
    (("C->") . mc/mark-next-like-this)
    (("C-<") . mc/mark-previous-like-this)
    (("C-c C-<") . mc/mark-all-like-this)))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2))

;; Dired reuse bufffer
(use-package dired+
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package recentf
  :ensure t
  :commands recentf-mode
  :config
  (progn (recentf-mode t) (setq-default recentf-max-saved-items 1000))
  :bind ("C-x C-a" . recentf-open-files)
  :init
  (recentf-mode 1))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
     (helm-mode 1)
    (require 'helm-config)
    (setq helm-candidate-number-limit 100))
  :config
  (setq helm-boring-buffer-regexp-list (list (rx "*scratch") (rx "*Messages") (rx "*magit") (rx "*helm")))
  :bind
  ([(?\s-q)] . helm-buffers-list))

(use-package undo-tree
  :ensure t)

(use-package neotree
  :ensure t
  :bind
  ([(f8)] . neotree-toggle))

(use-package rubocop
  :ensure t
  :config
  (dolist
      (add-hook 'ruby-mode-hook 'rubocop-mode)
      (setq rubocop-check-command "/usr/bin/rubocop --format emacs"))
  :bind
  ((("C-M-ñ") . rubocop-check-current-file)))

(use-package swbuff
  :ensure t
  :bind
  (([(shift f8)]  . swbuff-switch-to-next-buffer)
   ([(shift f10)] . swbuff-switch-to-previous-buffer)))

;; Column flash
(use-package col-highlight
  :ensure t
  :bind
  ([(C-escape)] . col-highlight-flash))

;; Display dir if two files have the same name
(use-package uniquify
  :init
    (progn
     (setq uniquify-buffer-name-style 'reverse
           uniquify-separator "|"
           uniquify-after-kill-buffer-p t
           uniquify-ignore-buffers-re "^\\*")))

(use-package ruby-electric
  :ensure t
  :init
    (progn
      (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))))

(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

;;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; Go to
(global-set-key (kbd "M-g") 'goto-line)    ;; M-g  'goto-line
(global-set-key [(f7)]  'comment-region)
(global-set-key [(shift f7)] 'uncomment-region)

;; JSX mode
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

;; Flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(setq flyspell-default-dictionary "castellano")

(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

(global-set-key [(shift f5)] 'my-replace-string)

(defun my-replace-string ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (call-interactively 'replace-string)))

;;CSS
;;(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(setq auto-mode-alist
       (append '(("\\.css$" . css-mode))
               auto-mode-alist))

(eval-after-load "dired"
       ;; don't remove `other-window', the caller expects it to be there
       '(defun dired-up-directory (&optional other-window)
          "Run Dired on parent directory of current directory."
          (interactive "P")
          (let* ((dir (dired-current-directory))
     	    (orig (current-buffer))
     	    (up (file-name-directory (directory-file-name dir))))
            (or (dired-goto-file (directory-file-name dir))
     	   ;; Only try dired-goto-subdir if buffer has more than one dir.
     	   (and (cdr dired-subdir-alist)
     		(dired-goto-subdir up))
     	   (progn
     	     (kill-buffer orig)
     	     (dired up)
     	     (dired-goto-file dir))))))

;; Kill all other buffers
(defun kill-other-buffers ()
  "Kill all buffers but the current one.
   Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(global-set-key (kbd "C-x C-b") 'kill-other-buffers)

;; gets the path and file name
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name))
)

(global-set-key "\C-cz" 'show-file-name)

(defun search-selection (beg end)
      "search for selected text"
      (interactive "r")
      (kill-ring-save beg end)
      (isearch-mode t nil nil nil)
      (isearch-yank-pop))

(define-key global-map (kbd "<C-f1>") 'search-selection)


;; Powerline configuration
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (avy org-bullets web-mode use-package undo-tree tabbar swap-buffers sublimity smooth-scrolling smart-mode-line slime slim-mode shell-switcher scss-mode sass-mode rvm ruby-electric ruby-block rubocop rspec-mode react-snippets projectile-speedbar powershell origami nurumacs neotree multiple-cursors mocha-snippets minimap markdown-mode magit light-soap-theme less-css-mode jsx-mode ivy-pages helm-rb helm-rails helm-git git-timemachine git-auto-commit-mode fountain-mode folding flyspell-lazy flymake-json flymake-jshint faff-theme dired+ color-theme-solarized col-highlight auctex airline-themes ac-inf-ruby)))
 '(powerline-default-separator (quote curve))
 '(show-paren-mode t)
 '(tramp-syntax (quote default) nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-active1 ((t (:inherit mode-line :background "hot pink"))))
 '(powerline-active2 ((t (:inherit mode-line :background "DarkOliveGreen3" :weight bold))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "OliveDrab2"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "plum1"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "OliveDrab2")) (((class color) (background dark)) (:background "OliveDrab2")) (t (:inverse-video t)))))

