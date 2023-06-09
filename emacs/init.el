;; Manuel Montoya init.el file 2006-2023

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- lexical-binding: t -*-
;; M-s h .  &  M-s h u  ;; Highlight and Unhighlight text

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

(defconst d/emacs-start-time (current-time))
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold 800000)))  ;; Better Garbage Collection

(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))


;; Force create a backup file always
(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook  'force-backup-of-buffer)

(tool-bar-mode -1) ;; icons sucks

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(setq package-check-signature nil)
(setq load-prefer-newer t)  ;; load newer
(package-initialize)

(add-to-list 'load-path "~/.config/emacs/mylibs/")

(require 'col-highlight)
(require 'toggle-quotes)
(require 'beitreten)

(global-set-key (kbd "C-'") 'toggle-quotes)

(global-set-key (kbd "C-<escape>") 'col-highlight-flash)

;; use trash
(setq delete-by-moving-to-trash t)  ;; look in ~/.local/share/Trash/files/

;; disable garbage collection when minibuffer is active
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook 'my-minibuffer-exit-hook)

;; disable confirmation if a file or buffer does not exist when you
;; use C-x C-f or C-x b
(setq confirm-nonexistent-file-or-buffer nil)

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(windmove-default-keybindings 'super)   ; bind windmove to s-{arrows}

(fset 'yes-or-no-p 'y-or-n-p)  ;; change all prompts to y or n

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
(set-frame-font "Inconsolata-12")
;; (set-default-font "Hack-11")
;; (set-frame-font "Hack-11" nil t)

(global-hi-lock-mode 1)
(setq hi-lock-file-patterns-policy '(lambda (dummy) t))

(require 'hi-lock)   ;; highlight a string in the current buffer.

(show-paren-mode 1)   ;; Show parentesis
(global-display-line-numbers-mode) ;; always show line numbers
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

;; (set-face-attribute 'whitespace-space nil :background nil :foreground "gray30")
(set-face-attribute 'trailing-whitespace nil
                      :foreground "OliveDrab2"
                      :inverse-video 'unspecified
                      :slant 'unspecified
                      :weight 'unspecified
                      :background "OliveDrab2")

(setq default-tab-width 2
      tab-width 2
      indent-tabs-mode t
      c-basic-offset 2
      default-major-mode 'text-mode
      transient-mark-mode t                  ;; Colors for selecting text (mark region)
      user-mail-address "mmontoya@gmail.com"
      user-full-name "Manuel Montoya"
      save-interprogram-paste-before-kill t
      inhibit-splash-screen t         ;; Begrüßungsbildschirm deaktivieren
      visible-bell t                  ;; Flashes on error
      TeX-PDF-mode t                  ;; PDF mode (rather than DVI-mode)
      standard-indent 2
      package-enable-at-startup nil
      split-width-threshold 9999     ;; split horizontal always
      scroll-conservatively 20       ;; move minimum when cursor exits view, instead of recentering
      load-prefer-newer t)           ;; Don't load outdated byte code

(set-face-attribute 'region nil :background "#ffd45e")

(add-hook 'haml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (define-key haml-mode-map "\C-m" 'newline-and-indent)))

;; Ändern Sie die Echonachricht
(defun display-startup-echo-area-message ()
  (message "Herrlicher Mann ist bereit, einen erstaunlichen Job zu liefern!"))

(setq home-directory "/home/xpsman/")
(setq default-directory (concat home-directory "entwicklung/chipotle/rdigital/"))

;;;;;;;;;;;;;;   VERWENDEN SIE PAKET ABSCHNITT BEGINNT   ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Completion

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

 ;;; Enhances completion at point see https://github.com/minad/cape
(use-package cape
  :ensure t)

(use-package all-the-icons
  :ensure t
  :defer t)

(use-package recentf
  :ensure t
  :defer t)

;; (use-package eglot
;;   :ensure t
;;   :commands (eglot eglot-ensure)
;; 	:hook (ruby-mode	. eglot-ensure))

(use-package web-mode
  :ensure t)

(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (setq solarized-distinct-fringe-background t)
;;   (setq solarized-use-variable-pitch nil)
;;   (setq solarized-scale-org-headlines nil)
;;   (setq solarized-high-contrast-mode-line t)
;;   (load-theme 'solarized-light t))

;; (use-package material-theme
;;   :init (progn (load-theme 'material-light t t)
;;                (enable-theme 'material-light))
;;   :defer t
;;   :ensure t)

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

(use-package dashboard  ;; An extensible emacs startup screen showing you what’s most important.
	     :ensure t
	     :config
	     (dashboard-setup-startup-hook)
       ;; Set the title
       (setq dashboard-banner-logo-title "Willkommen zu einem weiteren großen Tag des Erfolgs!!")
       ;; Set the banner
       (setq dashboard-startup-banner (concat home-directory
                                              "Documents/backups/dotfiles/emacs/themes/lisplogo_fancy_256.png"))
       (setq dashboard-items '((recents  . 5)
                               (bookmarks . 5)
                               (projects . 0)
                               (agenda . 5)
                               (registers . 5))))

(use-package better-jumper
  :ensure t
  :bind (("C-<f12>" . better-jumper-jump-backward)
         ("s-<f12>" . better-jumper-jump-forward)
				 ("C-<f8>" . better-jumper-set-jump))
  :config
  (better-jumper-mode +1))

(global-set-key (kbd "C-.")
	(lambda () (interactive "")
	  (switch-to-buffer (other-buffer (current-buffer) t))))

(use-package buffer-flip
  :ensure t
  :bind  (("M-<f1>" . buffer-flip)
          :map buffer-flip-map
          ( "M-<f1>" .   buffer-flip-forward)
          ( "M-S-<f1>" . buffer-flip-backward)
          ( "M-<ESC>" .     buffer-flip-abort))
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
					"^\\*sorbet\\*$"
          "^\\*swiper\\*$")))

;;  CLOJURE BLOCK STARTS

(use-package cider
  :ensure t
  :pin melpa-stable
  :bind (("C-x f" . cider-namespace-refresh)
         ("C-x C-d" . cider-connect-clj)) ;; C-c C-t p = run all tests, C-c C-t n = current NS
  :config
  (progn
    (flycheck-clojure-setup)
    (setq nrepl-hide-special-buffers t)
    (setq cider-popup-stacktraces-in-repl nil)
    (setq cider-repl-history-file "~/.config/emacs/nrepl-history")
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-repl-use-pretty-printing t)
    (setq cider-show-error-buffer nil)
    (setq cider-refresh-before-fn "reloaded.repl/suspend")
    (setq cider-refresh-after-fn "reloaded.repl/resume")
    (setq cider-cljs-lein-repl "(do (reloaded.repl/go) (user/cljs-repl))")))

(use-package vue-mode
  :mode "\\.vue\\'"
  :hook ((vue-mode . prettier-js-mode)
				 (vue-mode . eglot))
  :config
  (setq prettier-js-args '("--parser vue")))

(use-package clojure-snippets
  :ensure t)

(global-prettify-symbols-mode +1)

;; (use-package clojure-mode
;;   :ensure t
;;   :mode (("\\.edn$"  . clojure-mode)
;;          ("\\.clj$"  . clojure-mode))
;;   :bind (("C-c d f" . cider-code)
;;          ("C-c d g" . cider-grimoire)
;;          ("C-c d w" . cider-grimoire-web)
;;          ("C-c d c" . clojure-cheatsheet)
;;          ("C-c d d" . dash-at-point))
;;   :init
;;   (defconst clojure--prettify-symbols-alist
;;     '(("__"   . ?⁈)
;;       ("fn"   . ?λ)))
;;   :config
;;     (progn
;;       (setq clojure-align-forms-automatically t)
;;       (require 'flycheck-clj-kondo)
;;       (define-clojure-indent
;;         (defroutes 'defun)
;;         (GET 2)
;;         (POST 2)
;;         (PUT 2)
;;         (DELETE 2)
;;         (HEAD 2)
;;         (ANY 2)
;;         (context 2)
;;         (let-routes 1))

;;       (define-clojure-indent
;;         (s/fdef 1))

;;       (defun toggle-nrepl-buffer ()
;;         "Toggle the nREPL REPL on and off"
;;         (interactive)
;;         (if (string-match "cider-repl" (buffer-name (current-buffer)))
;;             (delete-window)
;;           (cider-switch-to-repl-buffer)))

;;       (defun cider-save-and-refresh ()
;;         (interactive)
;;         (save-buffer)
;;         (call-interactively 'cider-refresh))

;;       (defun cider-eval-last-sexp-and-append ()
;;         (interactive)
;;         (cider-eval-last-sexp '(1)))

;; 			(add-hook 'clojure-mode-hook
;;             (lambda ()
;;               (push clojure--prettify-symbols-alist prettify-symbols-alist)))
;;       (add-hook 'clojure-mode-hook 'global-prettify-symbols-mode)
;;       (add-hook 'clojure-mode-hook 'hs-minor-mode)))

;;  CLOJURE BLOCK ENDS

(use-package company  ;; Company is a text completion framework for Emacs.
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] 'company-complete company-mode-map)
    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

(global-set-key (kbd "C-c C-j") 'dired-jump)

(use-package diredfl                    ; Add colours to Dired
  :ensure t
  :config (diredfl-global-mode))

(use-package dired-icon
  :ensure t
  :config
	(progn
    (add-hook 'dired-mode-hook 'dired-icon-mode)
    (setq dired-icon-image-size 24)))

(use-package dired-narrow               ; Live-narrowing of search results
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-quick-sort
	:ensure t
  :init
  (dired-quick-sort-setup))

(use-package dired-ranger   ;; VIM like file manager
  :ensure t
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

(use-package dired-subtree
	:ensure t
  :after dired
  :bind  (("C-c C-q" . dired-subtree-toggle)
          ("C-c C-l"    . dired-subtree-cycle)))

(use-package eshell
  :init
	(progn
		(use-package em-smart :demand t)
		(use-package em-cmpl)
		(use-package em-prompt)
		(use-package em-tramp)
		(add-hook 'eshell-mode-hook
	          	(lambda ()
  		  				(add-to-list 'eshell-visual-commands "ssh")
					    	(add-to-list 'eshell-visual-commands "tail")
			      			(add-to-list 'eshell-visual-commands "top")))
		(add-hook 'eshell-mode-hook (lambda ()		       													(eshell/alias "e" "find-file $1")
    																(eshell/alias "ff" "find-file $1")
	   															(eshell/alias "emacs" "find-file $1")
																	(eshell/alias "ee" "find-file-other-window $1")
																	(eshell/alias "gd" "magit-diff-unstaged")
																	(eshell/alias "gds" "magit-diff-staged")
																	(eshell/alias "d" "dired $1")
																	;; The 'ls' executable requires the Gnu version on the Mac
																	(let ((ls (if (file-exists-p "/usr/local/bin/gls")
																								"/usr/local/bin/gls"
																							"/bin/ls")))
																		(eshell/alias "ll" (concat ls " -AlohG --color=awalys")))))
		(setq ;; eshell-buffer-shorthand t ...  Can't see Bug#19391
     eshell-scroll-to-bottom-on-input 'all
     eshell-error-if-no-glob t
     eshell-hist-ignoredups t
     eshell-save-history-on-exit t
     eshell-prefer-lisp-functions nil
     eshell-destroy-buffer-when-process-dies t)))

(use-package exec-path-from-shell
	:ensure t)

(use-package flycheck  ;; lint like tool
  :defer 1
  :init (setq
         flycheck-checkers
         '(
           ;; typescript-tslint
           ;; css-csslint
           emacs-lisp
           ruby-rubocop
           haml
           json-jsonlint
           yaml-jsyaml))
  :config (global-flycheck-mode))

(use-package flycheck-clojure
  :ensure t
  :commands (flycheck-clojure-setup)               ;; autoload
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (flycheck-clojure-setup))

(use-package flycheck-pos-tip :ensure t
  :after flycheck)

(use-package flycheck-clj-kondo
  :ensure t)

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))

(use-package frog-jump-buffer
  :ensure t
  :bind
  (([(?\s-<)] . frog-jump-buffer))
	:init
	(setq frog-jump-buffer-posframe-parameters '((left-fringe . 5)
																							 (right-fringe . 5)
																							 (internal-border-width . 5)
																							 (internal-border . "#6ba499")
																							 (internal-border-color . "#6ba499")
																							 (background-color . "#eee8d5")
                                               (background . "#eee8d5")))
  :config
  (progn
    (dolist (regexp '("TAGS" "^\\*Compile-log" "-debug\\*$" "^\\:" "helm\\*$" "errors\\*$" "^\\*Backtrace" "-ls\\*$" "^\\*dashboard"
                  "stderr\\*$" "^\\*Flymake" "^\\*vc" "^\\*Warnings" "^\\*Messages" "*cider-error*" "^\\*helm" "^\\*magit" "^\\*eldoc" "\\^*Shell Command"))
    (push regexp frog-jump-buffer-ignore-buffers))))

(use-package git-timemachine
  :ensure t)

(use-package graphql-mode
   :mode (("\\.graphql.ts\\'" . graphql-mode)
          ("\\.graphql\\'" . graphql-mode))
  :ensure t)

(use-package highlight-indentation
  :ensure t
  :config
  (set-face-background 'highlight-indentation-current-column-face "#fff7ba")
   :init
  (add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)
  (add-hook 'ruby-mode-hook 'highlight-indentation-current-column-mode))

(use-package helm
  :ensure t
  :bind
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list)
  ("C-x l" . 'helm-recentf)
  ("M-x" . 'helm-M-x)
  :config
  (defun daedreth/helm-hide-minibuffer ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        ;; (overlay-put ov 'face
        ;:             (let ((bg-color (face-background 'default nil)))
        ;;               `(:background ,bg-color :foreground ,bg-color)))
        ;; (setq-local cursor-type nil)
				)))
  (add-hook 'helm-minibuffer-set-up-hook 'daedreth/helm-hide-minibuffer)
  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 40
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-split-window-in-side-p nil
        helm-move-to-line-cycle-in-source nil
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-echo-input-in-header-line t)
  :init
  (helm-mode 1))

;; (define-key helm-find-files-map (kbd "C-b") 'helm-find-files-up-one-level)
;; (define-key helm-find-files-map (kbd "C-f") 'helm-execute-persistent-action)

(use-package helm-ag  ;; search a pattern in files and buffers
  :ensure t
  :commands (helm-ag)
  :bind
  (([(?\s-p)] . helm-ag-this-file)
   ("C-h C-y" . helm-ag))
  :init (setq helm-ag-insert-at-point 'symbol
	      helm-ag-command-option "--path-to-ignore ~/.agignore"))

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
    (setq js2-include-node-externs t)
    (add-hook 'js2-mode-hook 'hs-minor-mode)
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda ()
                               (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map)))))

(use-package magit    ;; git magic in Emacs
  :ensure t
  :bind (([(shift f6)] . magit-status)
         ("C-9" . magit-log-buffer-file)
         ("C-c m b" . magit-blame)
         ("C-c m q" . magit-blame-quit)))

(use-package markdown-mode
  :ensure t
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

(use-package org
  :ensure t
  :init (progn
          (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
          (setq org-todo-keywords
                '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "STAGING" "DONE" "CANCELED")))
          (setq org-clock-persist 'history)
          (setq org-support-shift-select 'always)
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
                '("~/entwicklung/chipotle/rdigital/notes/resolvedigi.org"))
          (org-clock-persistence-insinuate)
          )
  :bind  (("C-ñ" . org-todo)
          ("M-o p" . org-insert-heading)
          ("M-o l" . org-insert-subheading))
  :mode (("\\.org$" . org-mode)))

(use-package org-bullets  ;; for org-mode
  :ensure t)

(use-package page-break-lines ;; dashboard dependency
  :ensure t)

(use-package posframe
  :ensure t)

(use-package popwin ;; popwin
	     :ensure t
	     :config
	     (popwin-mode 1))

(use-package prettify-symbols-mode
	:commands (turn-on-pretty-mode global-prettify-symbols-mode)
  :bind ("C-c <C-return>" . prettify-symbols-mode)
  :config
	(progn (setq prettify-symbols-unprettify-at-point 'right-edge)
    (setq inhibit-compacting-font-caches t)))

(use-package projectile  ;; dashboard dependency
  :config
  (setq projectile-project-search-path '("~/entwicklung/chipotle/rdigital/txt2give/txt2give-app/"
                                         "~/entwicklung/chipotle/rdigital/icu/incartupsell"))
  :ensure t)

(use-package powerline  ;; Powerline Format for mode-line
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

(use-package centaur-tabs
  :ensure t
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "wave"
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-set-bar 'left
        centaur-tabs-set-bar 'over)
  :bind
  ("M-s-<left>" . centaur-tabs-backward)
  ("M-s-<right>" . centaur-tabs-forward))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (customize-set-variable 'js2-include-node-externs t))

(use-package tide                       ; https://github.com/ananthakumaran/tide
  :ensure t
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  (defun my/setup-tsx-mode ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode)))

  (defun my/setup-jsx-mode ()
    (when (string-equal "jsx" (file-name-extension buffer-file-name))
      (setup-tide-mode)))

  ;; (add-hook 'typescript-mode-hook 'setup-tide-mode)
  (add-hook 'js2-mode-hook 'setup-tide-mode)
  (add-hook 'web-mode-hook 'my/setup-tsx-mode)
  (add-hook 'rjsx-mode-hook 'my/setup-jsx-mode)
  :requires flycheck
  :config
  (add-to-list 'company-backends 'company-tide)
  ;; aligns annotation to the right hand side
  ;; (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  ;; (add-hook 'before-save-hook 'tide-format-before-save)

  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))

(use-package rjsx-mode
  :defer t)

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.inc\\'"
         "\\.tpl\\'"
         "\\.erb\\'"
         "\\.liquid\\'")
  :config
  ;; configure jsx-tide checker to run after your default jsx checker
  (add-hook 'web-mode-hook 'lsp-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; (flycheck-add-mode 'typescript-tslint 'web-mode)
  )

(use-package undo-tree
  :ensure t
  :init
  (setq undo-limit 78643200)
  (setq undo-outer-limit 104857600)
  (setq undo-strong-limit 157286400)
  (setq undo-tree-mode-lighter " UN")
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-history-directory-alist '(("." . "~/emacs.d/undo")))
 (add-hook 'undo-tree-visualizer-mode-hook (lambda ()
                                              (undo-tree-visualizer-selection-mode)
                                              (setq display-line-numbers nil)))
  :config
  (global-undo-tree-mode 1))

(use-package uniquify   ;; Display dir if two files have the same name
	     :init
	     (progn
	       (setq uniquify-buffer-name-style 'reverse
		     uniquify-separator "|"
		     uniquify-after-kill-buffer-p t
		     uniquify-ignore-buffers-re "^\\*")))

(use-package pug-mode
  :mode ("\\.vue\\'" . pug-mode)
  :config (add-hook 'vue-mode-hook 'lsp))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2))

;; (use-package whitespace ;; shows Whitespaces
;;   :bind (("C-c T w" . whitespace-mode))
;;   :init
;;   (dolist (hook '(conf-mode-hook))
;;     (add-hook hook 'whitespace-mode))
;;   :config (setq whitespace-line-column nil)
;;   :diminish whitespace-mode)

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
(setq auto-save-file-name-transforms
  `((".*" "~/.config/emacs/emacs-saves/" t)))
(defvar autosave-dir (expand-file-name "~/.saves/"))
(setq auto-save-file-name-transforms
  `((".*" "~/.saves/" t)))

(global-set-key (kbd "M-g") 'goto-line)    ;; M-g  'goto-line
(global-set-key [(f7)]  'comment-region)
(global-set-key [(shift f7)] 'uncomment-region)
(global-set-key [(shift f5)] 'my-replace-string)
(global-set-key [(shift f12)] 'eshell)
(global-set-key [(C f11)] 'find-grep-dired)
(global-set-key	[(C-escape)] 'col-highlight-flash)

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
  "Show the full path file current buffer name in the minibuffer."
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
 '(centaur-tabs-mode t nil (centaur-tabs))
 '(cider-show-error-buffer nil)
 '(cider-use-tooltips t)
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "eeb23ebf4a97b95a85f6f5e6b8524a9854da008f494828f0e78693675d6fc9ca" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(display-fill-column-indicator t)
 '(fci-rule-color "#eee8d5")
 '(fill-column 100)
 '(flycheck-typescript-tslint-config "~/entwicklung/chipotle/node/tslint.json")
 '(global-display-fill-column-indicator-mode t)
 '(helm-command-prefix-key "C-;")
 '(helm-ff-lynx-style-map t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#efe4da49afb1" "#cfc4e1acd08b" "#fe52c9e6b34e" "#dbb6d3c2dcf3" "#e183dee0b053" "#f944cc6dae47" "#d35fdac4e069"))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#b3c34d" . 20)
     ("#6ccec0" . 30)
     ("#74adf5" . 50)
     ("#e1af4b" . 60)
     ("#fb7640" . 70)
     ("#ff699e" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#e1af4b" "#fb7640" "#ff6849" "#ff699e" "#8d85e7" "#74adf5" "#6ccec0" "#b3c34d"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(js2-include-node-externs t)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(org-agenda-files nil)
 '(package-selected-packages
   '(dir-treeview-themes material-light cape corfu tab-bar-buffers lsp-treemacs all-the-icons-completion all-the-icons-dired all-the-icons-ibuffer nose highlight-indentation lsp-ui quelpa bookmark-view bm vdiff efar rvm smooth-scrolling color-theme-sanityinc-solarized nurumacs dired-subtree dired-icon vue-html-mode mmm-mode lsp-mode doom-themes eglot posframe pug-mode vue-mode rubocopfmt rubocop slim-mode jekyll-modes easy-jekyll coffee-mode comint-better-defaults esh-autosuggest eshell-prompt-extras cider ac-cider anakondo haml-mode flymake-haml modus-operandi-theme flycheck-clj-kondo helm-ag prettier-js rjsx-mode alect-themes apropospriate-theme anti-zenburn-theme ahungry-theme ace-jump-buffer better-jumper yaml-mode web-mode use-package-chords undo-tree transpose-frame tide tabbar solarized-theme smart-mode-line-powerline-theme rainbow-delimiters projectile popwin parseclj org-bullets neotree multiple-cursors markdown-mode majapahit-theme magit json-mode js2-mode ivy imenu-anywhere helm graphql-mode go-direx git-timemachine flycheck-pos-tip flycheck-clojure exec-path-from-shell discover dired-quick-sort dashboard company col-highlight clojurescript-mode clojure-snippets buffer-flip avy auctex all-the-icons))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(powerline-default-separator 'curve)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#657b83" 0.2))
 '(tabbar-background-color "gray20")
 '(tabbar-separator '(0.5))
 '(tabbar-use-images nil)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tramp-syntax 'default nil (tramp))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4366eb20b4")
     (60 . "#c1167942154f")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed892380000")
     (140 . "#96be94cf0000")
     (160 . "#8e5397440000")
     (180 . "#859900")
     (200 . "#77679bfc4635")
     (220 . "#6d449d465bfd")
     (240 . "#5fc09ea47092")
     (260 . "#4c68a01784aa")
     (280 . "#2aa198")
     (300 . "#303498e7affc")
     (320 . "#2fa1947cbb9b")
     (340 . "#2c879008c736")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types '((use-package) (use-package) (use-package)))
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#657b83" "#839496")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centaur-tabs-default ((t (:background "khaki" :foreground "linen"))))
 '(centaur-tabs-unselected ((t (:background "yellow green" :foreground "white smoke" :overline nil :underline nil))))
 '(centaur-tabs-unselected-modified ((t (:background "dark goldenrod" :foreground "cornsilk" :overline nil :underline nil))))
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

;; CIDER
(global-set-key (kbd "M-s-o") 'cider-repl-clear-buffer)

(defun clj-connect ()
  "easy connect"
  (interactive)
  (defvar foo "Ich verbinde mich mit dem Nest!!!")
  (message "Hello (%s)" foo)
  (cider-connect '(:host "localhost" :port "40019")))

(global-set-key (kbd "M-s M-p") 'clj-connect)

;; Keybinds for manipulating windows

(global-set-key (kbd "C-s-<left>")      'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>")     'enlarge-window-horizontally)

(global-set-key (kbd "C-s-<down>")      'shrink-window)
(global-set-key (kbd "C-s-<up>")        'enlarge-window)

(global-set-key (kbd "C-x K")         'kill-buffer-and-window)

;; defadvice: monkey patching on elisp
(defadvice kill-region (before slick-cut activate compile)   ;; C-w kills the current line
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-ring-save (before slick-copy activate compile)   ;; M-w copies the current line
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defun shell-command-to-pdf ()
  "Execute to pdf."
  (interactive)
  (shell-command
   "/usr/bin/pdflatex -interaction=nonstopmode /home/mmontoya/Documents/personal/Schriftstellerei/gypsys/gypsys.tex"))

(global-set-key (kbd "C-x .") 'shell-command-to-pdf)

;; after list-buffers is called, switch to it
(defadvice list-buffers (after jc-switch-to-other-win)
  (if (not (equalp (buffer-name (current-buffer))
                   "*Buffer List*"))
      (other-window 1))
  (goto-char (+ 4 (point))))

(defun org-make-olist (arg)
  (interactive "P")
  (let ((n (or arg 1)))
    (when (region-active-p)
      (setq n (count-lines (region-beginning)
                           (region-end)))
      (goto-char (region-beginning)))
    (dotimes (i n)
      (beginning-of-line)
      (insert (concat (number-to-string (1+ i)) ". "))
      (forward-line))
    (beginning-of-line)))

;; SMARTER ANFANG DER LINIE

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

   Move point to the first non-whitespace character on this line.
   If point is already there, move to the beginning of the line.
   Effectively toggle between the first non-whitespace character and
   the beginning of the line.

   If ARG is not nil or 1, move forward ARG - 1 lines first.  If
   point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "C-x p") 'eval-buffer)

;; Select line
(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(global-set-key (kbd "C-x o") 'select-current-line)

(defun ruby-mode-hook ()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.pryrc\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook 'hs-minor-mode)
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq ruby-deep-arglist t)
                               (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil))))

(ruby-mode-hook)

(add-hook 'ruby-mode-hook
  (lambda ()
    (setq-local flycheck-command-wrapper-function
                (lambda (command) (append '("/home/xpsman/.rvm/gems/default/bin/bundle" "exec") command)))))

(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))


(defun copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat "puts \"#### " (buffer-file-name) ":" (number-to-string (line-number-at-pos)) " #{var.inspect}\"")))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(define-key global-map (kbd "M-ñ") 'copy-current-line-position-to-clipboard)


;;; init.el file ends here
