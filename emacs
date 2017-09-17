;; Manuel Montoya .emacs file 2006-2017
;; mmontoya_arroba_gmail_PUNTO_com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-directory "/home/manuel/entwicklung/chipotle/")
;;(set-default-font "Fira Mono-11")
(set-default-font "Inconsolata-12")
(require 'package)
(setq package-enable-at-startup nil)

(setq package-enable-at-startup nil)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
;; use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
(require 'use-package))
(require 'diminish)
(require 'bind-key)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(setq split-width-threshold 9999) ;; split horizontal always

(use-package smart-mode-line)

(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "~/.emacs.d/elpa/")

(setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
(setq mouse-wheel-scroll-amount '(1)) ;; mouse scroll moves 1 line at a time, instead of 5 lines
(setq mouse-wheel-progressive-speed nil) ;; on a long mouse scroll keep scrolling by 1 line

(require 'neotree)
  (global-set-key [f8] 'neotree-toggle)

;; Don't load outdated byte code
(setq load-prefer-newer t)

(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

(setq save-interprogram-paste-before-kill t)

(global-visual-line-mode 1)      ;; Proper line wrapping
(setq inhibit-splash-screen t)   ;; Disable splash screen
(setq visible-bell t)            ;; Flashes on error
(setq TeX-PDF-mode t)            ;; PDF mode (rather than DVI-mode)

(setq-default show-trailing-whitespace t)

;; Folding methods
(load "folding" 'nomessage 'noerror)
(folding-mode-add-find-file-hook)
(folding-add-to-marks-list 'ruby-mode "#{{{" "#}}}" nil t)

(require 'auto-complete)
(global-auto-complete-mode t)

; Powerline
(require 'powerline)
(powerline-default-theme)

(load-theme 'solarized t)

(require 'flymake-jshint)
(add-hook 'js-mode-hook 'flymake-mode)
(add-hook 'jsx-mode-hook 'js-mode)

;; Dired reuse bufffer
(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)

(require 'yasnippet)
(yas-global-mode 1)

;; use web-mode- for .js files
;;(add-to-list 'auto-mode-alist '("\\.js$" . web-mode-jshint))

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

(require 'recentf)
(recentf-mode 1)

(require 'helm-config)
(helm-mode 1)

(require 'undo-tree)

;; Flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(setq flyspell-default-dictionary "castellano")

(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

(require 'multiple-cursors)
;; Multiple continum lines
(global-set-key [(super shift f10)] 'mc/edit-lines)

;; Column flash
(global-set-key [(C-escape)] 'col-highlight-flash)

;; Rubocop
(global-set-key (kbd "C-M-ñ") 'rubocop-check-current-file)

;; Multiple cursors not based on continuous lines
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'swbuff)
(global-set-key [(shift f8)]  'swbuff-switch-to-next-buffer)
(global-set-key [(shift f10)] 'swbuff-switch-to-previous-buffer)

(global-set-key (kbd "C-x C-a")  'recentf-open-files)
(global-set-key [(shift f6)] 'magit-status)
(global-set-key [(shift f5)] 'my-replace-string)

(defun my-replace-string ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (call-interactively 'replace-string)))

(global-set-key [(?\s-q)] 'helm-buffers-list)

(setq helm-boring-buffer-regexp-list (list (rx "*scratch") (rx "*Messages") (rx "*magit-") (rx "*helm")))

;; Display dir if two files have the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(setq helm-boring-buffer-regexp-list (list (rx "*scratch") (rx "*Messages") (rx "*magit-") (rx "*helm")))

;; Apostrophe, do not evaluate the name rubocop and replace it with its value; I really mean the name rubocop
(require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)
(setq rubocop-check-command "/usr/bin/rubocop --format emacs")
(defun rubocop-ensure-installed () )
;; rubocop
(global-set-key (kbd "C-c r") 'rubocop-check-current-file)

;; Completion for Ruby
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'inf-ruby-mode))
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)

;; Show parentesis
(show-paren-mode 1)

;; Show line-number in the mode line
(global-linum-mode 1) ; always show line numbers
;; Show column-number in the mode line
(column-number-mode 1)
;; Spaces nos real tabs
(setq-default indent-tabs-mode nil);
;;  Change Tab Width
(setq default-tab-width 2
      indent-tabs-mode t
      c-basic-offset 2)
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;;Set standard indent size
(setq standard-indent 2)

(global-hl-line-mode 1)

;(global-hl-line-mode t) ; turn it on for all modes by default
;; Make Text mode the default mode for new buffers
(setq default-major-mode 'text-mode)
;;Colors for selections (mark region)
(setq transient-mark-mode t)

(add-to-list 'load-path "~/.emacs.d/plugins")
(setq load-path (append load-path (list "~/.emacs.d/plugins")))

;; (run-at-time (current-time) 3000 'recentf-save-list)

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

(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "C-<up>")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 ))

;; Folding ruby
(add-hook 'ruby-mode-hook
  (lambda () (hs-minor-mode)))

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))

(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c s") 'hs-show-block)

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
 '(powerline-default-separator (quote curve))
 '(show-paren-mode t))
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

;; Kill all other buffers
(defun kill-other-buffers ()
  "Kill all buffers but the current one.
   Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(global-set-key (kbd "C-x C-b") 'kill-other-buffers)

