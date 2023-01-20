;;; beitreten.el --- Group non-system buffers.
;;
;; Filename: beitreten.el
;; Description: Group non-system buffers.
;; Author: Manuel Montoya
;; Maintainer: Manuel Montoya (concat "mmontoya" "@" "gmail" ".com")
;; Copyright (C) 2023, Manuel Montoya, all rights reserved.

(require 'dash)
(require 's)

(defun group-by-no-asterisks ()
   "Use these extensions to group non-system buffers"
   (let (
         (myTxtBuffers (--filter (not (or (s-starts-with-p " " it) (s-starts-with-p "*" it))) (mapcar #'buffer-name (buffer-list))))
         )
     (delete nil
             (mapcar (lambda (filename)
                       (message "filename `%s'..." filename)
                       (get-file-buffer filename))
                     myTxtBuffers)
             )
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'beitreten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; col-highlight.el ends here


;; (defun my/group-by-buffer-extensions ()
;;   "Use these extensions to group buffers"
;;   (interactive)
;;   (let
;;       (
;;        ;; (my-current-buffers (list (buffer-list)))
;;        (my-current-buffers (buffer-list))
;;        (myFiles (mapcar (function buffer-name) (buffer-list)))
;;        (myExtensions '(".coffee" ".clj" ".rb" ".el" ".txt" ".md" ".haml" ".js" ".erb" ".html")))
;; (--filter (not (or (s-starts-with-p " " it) (s-starts-with-p "*" it))) (mapcar #'buffer-name (buffer-list)))
;;     (mapcar (lambda (arg)
;;                (message "Reverting `%s'..." arg)
;;                ;; (buffer-file-name arg)
;;                ;; (member (file-name-extension (buffer-file-name arg)) myExtensions)
;;             )
;;             myFiles
;;  ))

;; ;; (progn (print (my/group-by-buffer-extensions)))
;; (my/group-by-buffer-extensions)

;; (cl-remove-if-not 'buffer-file-name (buffer-list))

;;(message "Reverting `%s'..." (buffer-name))

;; (let (
;;       (myBuffers (mapcar  #'buffer-name (buffer-list)))
;;      )
;;   (mapcar (lambda (filename)
;;             ;; (message "filname `%s'..." filname)
;;             ;; (member (file-name-extension filename) myExtensions)
;;             (--filter (not (or (s-starts-with-p " " it) (s-starts-with-p "*" it))) (mapcar #'buffer-name (buffer-list)))
;;          (mapcar (get-file-buffer it) my-buffer-names)
;;             )
;;            myBuffers )
;;   )
;; ;; (mapcar 'kill-buffer (delq (current-buffer) (buffer-list))
;; ;;      '(tab-line-tabs-function 'tab-line-tabs-buffer-groups)
;; (mapcar 'kill-buffer (delq (current-buffer) (buffer-list))


































