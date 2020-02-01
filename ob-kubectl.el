;;; ob-kubectl.el --- org-babel functions for kubectl evaluation

;; Copyright (C) Dr. Ian FitzPatrick

;; Author: Dr. Ian FitzPatrick
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; call kubectl apply on org babel block
;;

;;; Requirements:


;;; Code:
;(require 'ob)
;(require 'ob-ref)
;(require 'ob-comint)
;(require 'ob-eval)
;(require 's)

;; possibly require modes required for your language
(define-derived-mode kubectl-mode yaml-mode "kubectl"
  "Major mode for editing kubectl templates."
  )



;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("kubectl" . "kubectl"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:kubectl '()) ; TODO use this for input in stead of variable

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:kubectl' function below.
(defun org-babel-expand-body:kubectl (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  ;(require 'inf-kubectl) : TODO check if needed
  body ; TODO translate params to yaml variables
)

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:kubectl (body params)
  "Execute a block of kubectl code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing kubectl source code block")
    (org-babel-eval-kubectl "kubectl apply -f" body)
    ;; when forming a shell command, or a fragment of code in some
    ;; other language, please preprocess any file names involved with
    ;; the function `org-babel-process-file-name'. (See the way that
    ;; function is used in the language files)
    )


(defun org-babel-eval-kubectl (cmd yaml)
  "Run CMD on BODY.
If CMD succeeds then return its results, otherwise display
STDERR with `org-babel-eval-error-notify'."
  (let ((err-buff (get-buffer-create " *Org-Babel Error*"))
	(yaml-file (org-babel-temp-file "ob-kubectl-yaml-"))
	(output-file (org-babel-temp-file "ob-kubectl-out-"))
	exit-code)
    (with-temp-file yaml-file (insert yaml))
    (with-current-buffer err-buff (erase-buffer))
    (setq exit-code
	  (shell-command (concat cmd " " yaml-file) output-file err-buff)
	  )
      (if (or (not (numberp exit-code)) (> exit-code 0))
	  (progn
	    (with-current-buffer err-buff
	      (org-babel-eval-error-notify exit-code (buffer-string)))
	    (save-excursion
	      (when (get-buffer org-babel-error-buffer-name)
		(with-current-buffer org-babel-error-buffer-name
		  (unless (derived-mode-p 'compilation-mode)
		    (compilation-mode))
		  ;; Compilation-mode enforces read-only, but Babel expects the buffer modifiable.
		  (setq buffer-read-only nil))))
	    nil)
	; return the contents of output file
	(with-current-buffer output-file (buffer-string)))))


;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:kubectl (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-kubectl-var-to-kubectl (var)
  "Convert an elisp var into a string of kubectl source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-kubectl-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-kubectl-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    ))

(provide 'ob-kubectl)
;;; ob-kubectl.el ends here
