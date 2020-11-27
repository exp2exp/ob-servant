;;; ob-stream.el --- Babel Functions for Stream Evaluation

;; Copyright (C) 2009-2020 Free Software Foundation, Inc.

;; Author: Ray Puzio and Joe Corneli
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating shell source code.

;;; Code:
(require 'ob)
(require 'ob-calc)
(require 'ob-servant)

;; Initialize for this demo
(org-babel-servant-setup)

;;; DEFINE A CALCULATOR BACKEND — IS A STANDIN FOR PROCESS-LIST

;; We need to be able to get the callback from the process list

(setq calc-proc
  (start-process-shell-command
   "calculator"
   "calcout"
   "bc -q"))
(set-process-filter calc-proc 'org-babel-servant-callback)

;;; Integrate the calculator with Ob-servant

(puthash '("calculator" :preproc)
	 (lambda (x params) (concat x "\n"))
	 org-babel-servant-info)
(puthash '("calculator" :postproc) nil org-babel-servant-info)
(puthash '("calculator" :timeout) 2.0 org-babel-servant-info)

;;; Maxima example

(get-buffer-create "maxima-error")
(setq maxima-proc
      (make-process
       :name "maxima-proc"
       :command '("maxima" "--very-quiet") 
       :stderr "maxima-error"
       :filter #'org-babel-servant-callback))

(puthash '("maxima-proc" :preproc)
	 (lambda (x params) (concat x "\n"))
	 org-babel-servant-info)

(puthash '("maxima-proc" :preproc)
	 (lambda (x params) (concat "tex(" x ");\n"))
	 org-babel-servant-info)

(puthash '("maxima-proc" :postproc) 
	 (lambda (x params err) (substring x nil -6))
	 org-babel-servant-info)

(puthash '("maxima-proc" :timeout) 2.0 org-babel-servant-info)
(gethash '("maxima-proc" :lastout) org-babel-servant-info)

;; Now use :lastout key
;;(defvar org-stream-number-output 0)

;; For now, we will try this without

;;(defun org-stream-number-callback (prc str)
;;    (setq org-stream-number-output (string-to-number str)))

(defvar org-stream-string-output "")

(defun org-stream-string-callback (prc str)
    (setq org-stream-string-output str))

(set-process-filter calc-proc 'org-stream-number-callback)

;;; THIS WILL BE GENERALISED TO WORK WITH ANY PROCESS

;; TODO: `org-stream-output' should be generalized to "any output"
;; since we may have several different processes outputting
;; information to different places...

(defun org-babel-number-stream (body params)
  "Evaluates BODY using stream defined within PARAMS."
  (let ((proc (get-process (cdr (assoc :stream params)))))
    (process-send-string proc (concat body "\n"))
    (accept-process-output proc 2.0)
     org-stream-number-output))

(defun org-babel-string-stream (body params)
  "Evaluates BODY using stream defined within PARAMS."
  (let ((proc (get-process (cdr (assoc :stream params)))))
    (process-send-string proc (concat body "\n"))
    (accept-process-output proc 2.0)
    (list org-stream-string-output)))

;; default setting is to work with numbers if you want to change
;; it you can it work with strings
(defalias #'org-babel-stream #'org-babel-number-stream)
(fset #'org-babel-stream #'org-babel-string-stream)
(fset #'org-babel-stream #'org-babel-number-stream)

(provide 'ob-stream)

;;; ob-stream.el ends here
