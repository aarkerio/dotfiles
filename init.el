;; Manuel Montoya .emacs file 2006-2019
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- lexical-binding: t -*-
;; M-s h .  &  M-s h u  ;; Highlight and Unhighlight text


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

;; Add melpa package source when using package list
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("mermalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/")  t)

(defconst d/emacs-start-time (current-time))
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold 800000)))  ;; Better Garbage Collection

(setq package-archives '(("gnu" .          "https://elpa.gnu.org/packages/")
                         ("marmalade" .    "https://marmalade-repo.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" .        "https://melpa.org/packages/")))

(setq package-check-signature nil)

;; (package-initialize)
(add-to-list 'exec-path "/home/manuel/.yarn/bin/")

(require 'recentf)
(recentf-mode 1)

;; Turn off mouse interface early in startup to avoid momentary display
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

;; use ibuffer instead of buffer
(bind-key "C-x C-b" 'ibuffer)

;; use trash
(setq delete-by-moving-to-trash t)

;; disable garbage collection when minibuffer is active
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; disable confirmation if a file or buffer does not exist when you
;; use C-x C-f or C-x b
(setq confirm-nonexistent-file-or-buffer nil)

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(windmove-default-keybindings 'super)   ; bind windmove to s-{arrows}

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

(defun acg-initial-buffer-choice ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*"))
  (get-buffer "*Messages*"))

(setq initial-buffer-choice 'acg-initial-buffer-choice)  ;; no scratch buffer

;; electric-pair-mode
(electric-pair-mode 1)
(show-paren-mode 1)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; (set-default-font "Fira Mono-11")
;; (set-default-font "Inconsolata-12")
;; (set-default-font "Hack-11")
(set-frame-font "Hack-11" nil t)

(global-hi-lock-mode 1)
(setq hi-lock-file-patterns-policy #'(lambda (dummy) t))

(require 'hi-lock)   ;; highlight a string in the current buffer.

(show-paren-mode 1)   ;; Show parentesis
(global-linum-mode 1) ;; always show line numbers
(column-number-mode 1)
(global-hl-line-mode 1)
(global-visual-line-mode 1)  ;; Proper line wrapping
(add-hook 'after-init-hook 'global-company-mode)
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

;; Change the echo message
(defun display-startup-echo-area-message ()
  (message "Herrlicher Mann ist bereit, einen erstaunlichen Job zu liefern!"))

(setq home-directory (if (string= (system-name) "pav23") ;; laptop or desktop ?
			    "/home/manuel/"
			    "/home/mmontoya/"))

(setq default-directory (concat home-directory "entwicklung/chipotle/fondeadora/"))


;; (load (concat home-directory "elisp/myfunctions"))

;;;;;;;;;;;;;;   USE PACKAGE THEME SECTION  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package apropospriate-theme
;;    :ensure t
;;    :config
;;    (load-theme 'apropospriate-light t))

;; (use-package majapahit-theme
;;	     :ensure majapahit-theme
;;	     :config (load-theme 'majapahit-light t))

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-light t))

;;;;;;;;;;;;;;   USE PACKAGE SECTION STARTS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-jump-buffer
  :bind
  ("s-|" . ace-jump-buffer)
  :config
  (make-ace-jump-buffer-function
      "special"
    (with-current-buffer buffer
      (--all?
       (not (derived-mode-p it))
       '(comint-mode magit-mode inf-ruby-mode rg-mode compilation-mode)))))

(use-package all-the-icons
  :ensure t) ;; various Icon and Fonts for Emacs

(use-package auctex  ;;  Sophisticated document creation
  :defer t
  :ensure t)

(use-package exec-path-from-shell
	     :ensure t)

(use-package page-break-lines
	     :ensure t)

(use-package dashboard  ;; An extensible emacs startup screen showing you what’s most important.
	     :ensure t
	     :config
	     (dashboard-setup-startup-hook)
       ;; Set the title
       (setq dashboard-banner-logo-title "Willkommen zu einem weiteren großen Tag des Erfolgs!!")
       ;; Set the banner
       (setq dashboard-startup-banner (concat home-directory "Bilder/lisplogo_fancy_256.png"))
       (setq dashboard-items '((recents  . 5)
                               (bookmarks . 5)
                               (projects . 0)
                               (agenda . 5)
                               (registers . 5))))

(use-package better-jumper
  :ensure t
  :bind (("C-c o" . better-jumper-jump-backward)
         ("C-c g" . better-jumper-jump-forward))
  :config
  (better-jumper-mode +1))

(use-package buffer-flip
  :ensure t
  :bind  (("M-<tab>" . buffer-flip)
          :map buffer-flip-map
          ( "M-<tab>" .   buffer-flip-forward)
          ( "M-S-<tab>" . buffer-flip-backward)
          ( "M-<ESC>" .     buffer-flip-abort))
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*swiper\\*$")))

(use-package cider
  :ensure t
  :pin melpa-stable
  :bind (("C-x f" . cider-namespace-refresh))
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

(use-package clojure-snippets
  :ensure t)

(use-package clojurescript-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  :mode (("\\.edn$"  . clojure-mode)
         ("\\.clj$"  . clojure-mode))
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

      (add-hook 'clojure-mode-hook 'global-prettify-symbols-mode)
      (add-hook 'clojure-mode-hook 'hs-minor-mode)))

(use-package col-highlight    ;; Column flash
	:ensure t
	:bind
	([(C-escape)] . col-highlight-flash))

(use-package company  ;; Company is a text completion framework for Emacs.
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

(use-package dashboard  ;; An extensible emacs startup screen showing you what’s most important.
	:ensure t
	:config
  (setq dashboard-banner-logo-title "Willkommen zu einem weiteren großen Tag des Erfolgs!!") ;; Set the title
  (setq dashboard-startup-banner (concat home-directory "Bilder/lisplogo_fancy_256.png")) ;; Set the banner
  (setq show-week-agenda-p t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 0)
                          (agenda . 5)
                          (registers . 5)))
	(dashboard-setup-startup-hook))

(use-package dired-narrow               ; Live-narrowing of search results
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-quick-sort
	:ensure t
  :init
  (dired-quick-sort-setup))

(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

(use-package diredfl                    ; Add colours to Dired
  :ensure t
  :config (diredfl-global-mode))

(use-package exec-path-from-shell
	:ensure t)

; Flycheck
(use-package flycheck
  :defer 1
  :init (setq
         flycheck-checkers
         '(typescript-tslint
           css-csslint
           emacs-lisp
           haml
           json-jsonlint
           yaml-jsyaml))
  :config (global-flycheck-mode))

(use-package flycheck-clojure
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (use-package flycheck
    :config
    (flycheck-clojure-setup)))

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))

(use-package frog-jump-buffer
  :ensure t
  :bind
  (([(?\s-q)] . frog-jump-buffer))
  :config
  (progn
    (defvar frog-jump-buffer-include-current-buffer nil)
    (dolist (regexp '("TAGS" "^\\*Compile-log" "-debug\\*$" "^\\:" "errors\\*$" "^\\*Backtrace" "-ls\\*$"
                  "stderr\\*$" "^\\*Flymake" "^\\*vc" "^\\*Warnings" "^\\*eldoc" "\\^*Shell Command"))
    (push regexp frog-jump-buffer-ignore-buffers))))

(use-package git-timemachine
  :ensure t)

(use-package graphql-mode
   :mode (("\\.graphql.ts\\'" . graphql-mode)
          ("\\.graphql\\'" . graphql-mode))
  :ensure t)

(use-package helm
  :ensure t
  :init
    (progn
      (helm-mode 1)
      (require 'helm-config)
      (setq helm-candidate-number-limit 100))
  :config
    (setq helm-boring-buffer-regexp-list (list (rx "*scratch") (rx "*Messages") (rx "*magit") (rx "*Echo")(rx "*Complet")(rx "*code")(rx "*Mini") (rx "*helm")))
  :bind
   (([(?\s-w)] . helm-buffers-list)
    ("M-x" . helm-M-x)
    ("C-x C-f" . helm-find-files)
    ("C-x l" . helm-recentf)
    :map helm-map
    ("C-j" . helm-next-line)
    ("C-k" . helm-previous-line)))

(use-package helm-files
  :bind
  (:map helm-find-files-map
   ("C-h" . helm-find-files-up-one-level)
   ("C-l" . helm-execute-persistent-action)))

(use-package hs-minor-mode   ;; hide-show blocks
  :bind
  ("C-c T h" . hs-minor-mode)
  ("C-c h a" . hs-hide-all)
  ("C-c h s" . hs-show-all)
  ("C-c h h" . hs-toggle-hiding))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :bind (("C-s-l" . back-to-indentation-or-beginning-of-line)
         ("C-M-h" . backward-kill-word))
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda ()
                               (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map)))))

(use-package magit    ;; git magic in Emacs
  :ensure t
  :bind (([(shift f6)] . magit-status)
         ("C-c m b" . magit-blame)
         ("C-c m q" . magit-blame-quit)))

(use-package markdown-mode
	:config (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
	        (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
	        (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package multiple-cursors
  :ensure t
  :bind
  ( ;; Multiple continum lines
    ([(super shift f10)] . mc/edit-lines)
    ;; Multiple cursors not based on continuous lines
    ;;(("C->") . mc/mark-next-like-this)
    ;;(("C-<") . mc/mark-previous-like-this)
    ;;(("C-c C-<") . mc/mark-all-like-this))
    ))

(use-package neotree
  :ensure t
  :bind
  ([(f8)] . neotree-toggle))

(use-package org
  :ensure t
  :init (progn
          (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
          (setq org-todo-keywords
                '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "STAGING" "DONE" "CANCELED")))
          (setq org-clock-persist 'history)
          (setq org-agenda-custom-commands
                '(("d" "Daily agenda and all TODOs"
                   ((tags "PRIORITY=\"A\""
                          ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                           (org-agenda-overriding-header "High-priority unfinished tasks:")))
                    (agenda "" ((org-agenda-ndays 1)))
                    (alltodo ""
                             ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                             (air-org-skip-subtree-if-priority ?A)
                                                             (org-agenda-skip-if nil '(scheduled deadline))))
                              (org-agenda-overriding-header "ALL normal priority tasks:"))))
                   ((org-agenda-compact-blocks t)))))
          (setq org-agenda-files
                '("~/Documents/personal/zeitplane/meine_schweren_Verpflichtungen.org"))
          (org-clock-persistence-insinuate)
          )
  :mode (("\\.org$" . org-mode)))

(use-package org-bullets  ;; for org-mode
  :ensure t)

(use-package page-break-lines ;; dashboard dependency
  :ensure t)

(use-package popwin ;; popwin
	     :ensure t
	     :config
	     (popwin-mode 1))

;; prettier-emacs: minor-mode to prettify javascript files on save
;; https://github.com/prettier/prettier-emacs
(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(use-package projectile  ;; dashboard dependency
  :ensure t)

(use-package powerline  ;; Powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package rainbow-delimiters  ;; "rainbow parentheses"
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package rjsx-mode
  :ensure t
  :mode (("\\.jsx$" . rjsx-mode)))

(use-package rubocop
	     :ensure t
	     :defer t
	     :bind
	     (([(M f12)] . rubocop-check-current-file)))

(use-package smart-mode-line-powerline-theme
  :ensure t
  :after powerline
  :after smart-mode-line
  :config
  (sml/setup)
  (sml/theme 'light)
  (sml/apply-theme 'powerline))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package tabbar   ;; tabs for emacs
	     :ensure t
	     :init
	     (progn (tabbar-mode t)))

(use-package tide
  :config
  (progn
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-hook 'js2-mode-hook #'setup-tide-mode)))

(defun setup-tide-mode ()
   (interactive)
   (tide-setup)
   (flycheck-mode +1)
   (setq flycheck-check-syntax-automatically '(save mode-enabled))
   (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
   (eldoc-mode +1)
   (company-mode +1))

;; TypeScript
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

(use-package undo-tree
  :ensure t)

(use-package uniquify   ;; Display dir if two files have the same name
	     :init
	     (progn
	       (setq uniquify-buffer-name-style 'reverse
		     uniquify-separator "|"
		     uniquify-after-kill-buffer-p t
		     uniquify-ignore-buffers-re "^\\*")))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2))

(use-package whitespace ;; shows Whitespaces
  :bind (("C-c T w" . whitespace-mode))
  :init
  (dolist (hook '(conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config (setq whitespace-line-column nil)
  :diminish whitespace-mode)

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" . yaml-mode))

(use-package yasnippet
  :ensure t
  :init
  (progn
    (yas-global-mode 1)
    (use-package clojure-snippets)))

;;;;;;;;;;;;;;   USE PACKAGE SECTION ENDS   ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; backup/autosave
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist '(("." . "~/.emacs.d/autosave/")))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

(global-set-key (kbd "M-g") 'goto-line)    ;; M-g  'goto-line
(global-set-key [(f7)]  'comment-region)
(global-set-key [(shift f7)] 'uncomment-region)
(global-set-key [(shift f5)] 'my-replace-string)
(global-set-key [(shift f12)] 'eshell)
(global-set-key [(C f11)] 'find-grep-dired)

;; Flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(setq flyspell-default-dictionary "castellano")

(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
(add-hook 'markdown-mode-hook 'turn-on-flyspell)

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
     (interactive "P")P
     (let* ((dir (dired-current-directory))
       	    (orig (current-buffer))
     	      (up (file-name-directory (directory-file-name dir))))
       (or (dired-goto-file (directory-file-name dir))
     	     ;; Only try dired-goto-subdir if buffer has more than one dir.
     	     (and (cdr dired-subdir-alist)
     		        (dired-goto-subdir up))
     	     ((progn )
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
  (kill-new (file-truename buffer-file-name)))

(global-set-key "\C-cz" 'show-file-name)

(defun search-selection (beg end)
      "search for selected text"
      (interactive "r")
      (let (
            (selection (buffer-substring-no-properties beg end))
           )
        (deactivate-mark)
        (isearch-mode t nil nil nil)
        (isearch-yank-string selection)))

(define-key global-map (kbd "<C-f3>") 'search-selection)

;; Powerline configuration
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-show-error-buffer nil)
 '(cider-use-tooltips t)
 '(column-number-mode t)
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(flycheck-typescript-tslint-config "~/entwicklung/chipotle/node/tslint.json")
 '(org-agenda-files
   '("~/Documents/personal/zeitplane/meine_schweren_Verpflichtungen.org"))
 '(package-selected-packages
   '(vterm vscode-icon gnu-elpa-keyring-update frog-jump-buffer rjsx-mode alect-themes apropospriate-theme anti-zenburn-theme ahungry-theme ace-jump-buffer better-jumper yaml-mode web-mode use-package-chords undo-tree transpose-frame tide tabbar solarized-theme smart-mode-line-powerline-theme rubocop rainbow-delimiters projectile popwin parseclj org-bullets neotree multiple-cursors markdown-mode majapahit-theme magit json-mode js2-mode ivy imenu-anywhere helm graphql-mode go-direx git-timemachine flycheck-pos-tip flycheck-clojure exec-path-from-shell discover dired-quick-sort dashboard company col-highlight clojurescript-mode clojure-snippets buffer-flip avy auctex all-the-icons))
 '(powerline-default-separator 'curve)
 '(show-paren-mode t)
 '(tramp-syntax 'default nil (tramp)))
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

(defun jpt-toggle-mark-word-at-point ()
  (interactive)
  (if hi-lock-interactive-patterns
      (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
    (highlight-symbol-at-point)))

(global-set-key (kbd "s-.") 'jpt-toggle-mark-word-at-point)

(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(global-set-key (kbd "M-m") 'beginning-of-line-or-indentation)

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
   This function is a custom function for tabbar-mode's tabbar-buffer-groups.
   This function group all buffers into 3 groups:
   Those Dired, those user buffer, and those emacs buffer.
   Emacs buffer are those starting with “*”."
  (list
    (cond
      ((string-equal "*" (substring (buffer-name) 0 1))
       "Emacs Buffer")
      ((eq major-mode 'dired-mode)
       "Dired")
      (t
       "User Buffer"))))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

(global-set-key [M-s-left] 'tabbar-backward)
(global-set-key [M-s-right] 'tabbar-forward)

;; CIDER
(global-set-key (kbd "M-s-o") 'cider-repl-clear-buffer)

(defun clj-connect ()
  "easy connect"
  (interactive)
  (defvar foo "Ich verbinde mich mit dem Nest!!!")
  (message "Hello (%s)" foo)
  (cider-connect '(:host "localhost" :port "7000")))

(global-set-key (kbd "M-s M-p") 'clj-connect)

(global-set-key (kbd "C-s-t") 'eval-buffer)

;; Keybinds for manipulating windows

(global-set-key (kbd "C-s-<left>")      'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>")     'enlarge-window-horizontally)

(global-set-key (kbd "C-s-<down>")      'shrink-window)
(global-set-key (kbd "C-s-<up>")        'enlarge-window)

(global-set-key (kbd "C-x K")         'kill-buffer-and-window)

(defun copy-line (arg)
      "Copy lines (as many as prefix argument) in the kill ring"
      (interactive "p")
      (kill-ring-save (line-beginning-position)
                      (line-beginning-position (+ 1 arg)))
      (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

;; optional key binding
(global-set-key "\C-c\C-a" 'copy-line)

(defun shell-command-to-pdf ()
  "Execute to pdf."
  (interactive)
  (shell-command
   "/usr/bin/pdflatex -interaction=nonstopmode /home/manuel/Documents/personal/Schriftstellerei/gypsys/gypsys.tex"))

(global-set-key (kbd "C-x C-w") 'shell-command-to-pdf)

;;; init.el file ends here
