;; Manuel Montoya .emacs file 2006-2018
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- lexical-binding: t -*-
;; M-s h .  &  M-s h u  ;; Highlight and Unhighlight text


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package)
(setq package-enable-at-startup nil)

(defconst d/emacs-start-time (current-time))
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold 800000)))  ;; Better Garbage Collection

(setq package-archives '(("gnu" .          "https://elpa.gnu.org/packages/")
                         ("marmalade" .    "https://marmalade-repo.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" .        "https://melpa.org/packages/")))
(package-initialize)

(defun acg-initial-buffer-choice ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*"))
  (get-buffer "*Messages*"))

(setq initial-buffer-choice 'acg-initial-buffer-choice)  ;; no scratch buffer

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(add-to-list 'exec-path "/home/manuel/.rvm/gems/ruby-2.1.3@thrive/bin:/home/manuel/.rvm/gems/ruby-2.1.3@global/bin:/home/manuel/.rvm/rubies/ruby-2.1.3/bin:/home/manuel/.rvm/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:/home/manuel/.rvm/gems/ruby-2.1.3@thrive:/home/manuel/.rvm/gems/ruby-2.1.3@global")

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

(setq default-directory (if (string= system-name "pav23")
			    "/home/manuel/entwicklung/chipotle/lisp/"
			  "/home/mmontoya/entwicklung/chipotle/lisp/"))

;; (load (concat default-directory "elisp/myfunctions"))

;;(use-package color-theme-sanityinc-solarized
;;  :ensure t
;;  :config (load-theme 'solarized t))

;; (use-package majapahit-theme
;;	     :ensure majapahit-theme
;;	     :config (load-theme 'majapahit-light t))

(use-package exec-path-from-shell
	     :ensure t)

(use-package page-break-lines
	     :ensure t)

(use-package projectile
	     :ensure t)

(use-package dashboard  ;; An extensible emacs startup screen showing you what’s most important.
	     :ensure t
	     :config
	     (dashboard-setup-startup-hook))
;; Set the title
(setq dashboard-banner-logo-title "Willkommen zu einem weiteren großen Tag des Erfolgs!!")
;; Set the banner
(setq dashboard-startup-banner "/home/manuel/Documents/images/lisplogo_fancy_256.png")

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 0)
                        (agenda . 5)
                        (registers . 5)))

(load-theme 'majapahit-light t)

;(use-package color-theme-sanityinc-solarized
;  :ensure t
;  :config (load-theme 'majapahit-light t))

(use-package buffer-flip
  :ensure t
  :bind  (("M-<tab>" . buffer-flip)
          :map buffer-flip-map
          ( "M-<tab>" .   buffer-flip-forward)
          ( "M-S-<tab>" . buffer-flip-backward)
          ( "M-ESC" .     buffer-flip-abort))
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*swiper\\*$")))

(use-package all-the-icons
  :ensure t) ;; various Icon and Fonts for Emacs

(use-package avy   ;; Jump to things in Emacs tree-style
  :ensure t
  :bind
  (("C-." . avy-goto-word-1)
   ("C-," . avy-goto-char-2))
  :config (setq avy-all-windows nil))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :bind (("C-a" . back-to-indentation-or-beginning-of-line)
         ("C-M-h" . backward-kill-word))
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda ()
                               (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map)))))

(use-package hs-minor-mode   ;; hide-show blocks
  :bind
  ("C-c T h" . hs-minor-mode)
  ("C-c h a" . hs-hide-all)
  ("C-c h s" . hs-show-all)
  ("C-c h h" . hs-toggle-hiding))

(use-package rainbow-delimiters  ;; "rainbow parentheses"
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

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

(use-package clojure-snippets
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

(use-package cider
  :ensure t
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

(use-package powerline  ;; Powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package yasnippet
  :ensure t)

(use-package magit
  :ensure t
  :bind (([(shift f6)] . magit-status)
         ("C-c m b" . magit-blame)
         ("C-c m q" . magit-blame-quit)))

(use-package whitespace ;; shows Whitespaces
  :bind (("C-c T w" . whitespace-mode))
  :init
  (dolist (hook '(conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config (setq whitespace-line-column nil)
  :diminish whitespace-mode)

(use-package imenu-anywhere  ;; Imenu
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
    ;;(("C->") . mc/mark-next-like-this)
    ;;(("C-<") . mc/mark-previous-like-this)
    ;;(("C-c C-<") . mc/mark-all-like-this))
  ))

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
  :init
    (progn
      (helm-mode 1)
      (require 'helm-config)
      (setq helm-candidate-number-limit 100))
  :config
    (setq helm-boring-buffer-regexp-list (list (rx "*scratch") (rx "*Messages") (rx "*magit") (rx "*Echo")(rx "*Complet")(rx "*code")(rx "*Mini") (rx "*helm")))
  :bind
   (([(?\s-q)] . helm-buffers-list)
    ("M-x" . helm-M-x)
    ("C-x C-f" . helm-find-files)
    :map helm-map
    ("C-j" . helm-next-line)
    ("C-k" . helm-previous-line)))

(use-package helm-files
  :bind
  (:map helm-find-files-map
   ("C-h" . helm-find-files-up-one-level)
   ("C-l" . helm-execute-persistent-action)))

(use-package undo-tree
  :ensure t)

(use-package neotree
  :ensure t
  :bind
  ([(f8)] . neotree-toggle))

(use-package rubocop
	     :ensure t
	     :defer t
	     :bind
	     (([(M f12)] . rubocop-check-current-file)))

(use-package col-highlight    ;; Column flash
	     :ensure t
	     :bind
	     ([(C-escape)] . col-highlight-flash))

(use-package uniquify   ;; Display dir if two files have the same name
	     :init
	     (progn
	       (setq uniquify-buffer-name-style 'reverse
		     uniquify-separator "|"
		     uniquify-after-kill-buffer-p t
		     uniquify-ignore-buffers-re "^\\*")))

(use-package markdown-mode
	     :config (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
	     (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
	     (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package ruby-mode
	     :ensure t
	     :mode "\\.rb\\'"
	     :mode "Rakefile\\'"
	     :mode "Gemfile\\'"
	     :mode "Berksfile\\'"
	     :mode "Vagrantfile\\'"
	     :interpreter "ruby"
	     :init
	     (setq ruby-indent-level 2
		   ruby-indent-tabs-mode nil)
	     (add-hook 'ruby-mode 'superword-mode)
	     (add-hook 'ruby-mode 'hs-minor-mode)

	     :bind
	     (([(meta down)] . ruby-forward-sexp)
	      ([(meta up)]   . ruby-backward-sexp)
	      (("C-c C-e"    . ruby-send-region))))  ;; Rebind since Rubocop uses C-c C-r

(use-package popwin ;; popwin
	     :ensure t
	     :config
	     (popwin-mode 1))

(use-package ruby-electric
	     :ensure t
	     :init
	     (progn
	       (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))))

(use-package rvm
	     :ensure t
	     :init
	     (progn (rvm-use-default)))  ;; use rvm's default ruby for the current Emacs session

(use-package tabbar
	     :ensure t
	     :init
	     (progn (tabbar-mode t)))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2))

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
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (helm-navi helm-pages popup-imenu popup-edit-menu ivy ace-link ace-jump-helm-line dashboard buffer-flip auto-complete ace-isearch ace-popup-menu ace-jump-buffer discover hs-minor-mode majapahit-theme cider exwm exec-path-from-shell all-the-icons latex-extra feature-mode flymake-ruby ztree highlight auto-highlight-symbol js2-mode avy org-bullets web-mode use-package undo-tree tabbar swap-buffers sublimity smooth-scrolling smart-mode-line slime slim-mode shell-switcher scss-mode sass-mode rvm ruby-electric ruby-block rspec-mode react-snippets projectile-speedbar powershell origami nurumacs neotree multiple-cursors mocha-snippets minimap markdown-mode magit light-soap-theme less-css-mode jsx-mode ivy-pages helm-rb helm-rails helm-git git-timemachine git-auto-commit-mode fountain-mode folding flyspell-lazy flymake-json flymake-jshint faff-theme dired+ color-theme-solarized col-highlight auctex airline-themes ac-inf-ruby)))
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

(defun my-run-latex ()
  (interactive)
  (let ((default-directory "/home/manuel/Documents/personal/Schriftstellerei/gypsys/")
        (gyp-file-path (expand-file-name "gypsys.tex"))
        (aux-files-path (expand-file-name "gypro/*.aux"))
        (aux-file-path (expand-file-name "*.aux"))
        (TeX-save-document (TeX-master-file))
        (delete-file aux-files-path)
        (delete-file aux-file-path)
        (TeX-command "LaTeX" gyp-file-path -1))))

(defun my-LaTeX-hook ()
 (local-set-key (kbd "C-c C-a") 'my-run-latex))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-hook)

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

(global-set-key (kbd "M-s-p") 'clj-connect)

(global-set-key (kbd "C-s-t") 'eval-buffer)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x l") 'helm-recentf)

;; Keybinds for manipulating windows

(global-set-key (kbd "C-s-<left>")      'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>")     'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>")      'shrink-window)
(global-set-key (kbd "C-s-<up>")        'enlarge-window)
(global-set-key (kbd "C-x K")         'kill-buffer-and-window)
