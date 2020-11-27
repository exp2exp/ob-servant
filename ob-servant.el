;;; Code
(require 'ob)

(defvar org-babel-servant-info nil
  "Hash table which contains the information needed to interact
with a servant properly.")
;;
;; A key is of the form (proc keyword) where proc is the name of a
;; process and keyword is one of the following:
;;
;; :preproc A preprocessing function which takes the body and params
;;          as input and returns a modified body to be sent to the
;;          process.  Default is no preprocessing.
;;
;; :postproc A postprocessing function which takes the body, params
;;          and the last error
;;          as input and returns a modified body to be sent as a
;;          return value.  Default is no postprocessing.
;;
;; :timeout Timeout when accepting output.  Default is one second.
;;
;; :lastout The output from latest call to the process.
;;
;; :lasterr Any error messages from the last call to the process.
;;
;; :timestamp The last time the process was called.

(defun org-babel-servant-setup ()
  (setq org-babel-servant-info
	(make-hash-table :test 'equal)))

(defun org-babel-servant-callback (prc str)
  (let ((name (process-name prc)))
    (puthash `(,name :lastout)
	     (concat 
	      (gethash `(,name :lastout)
		       org-babel-servant-info)
	      str)
	     org-babel-servant-info)))

(defun org-babel-servant-error-callback (prc str)
  (let ((name (process-name prc)))
    (puthash `(,name :lasterr)
	     (concat 
	      (gethash `(,name :lasterr)
		       org-babel-servant-info)
	      str)
	     org-babel-servant-info)))

(defun org-babel-servant (body params)
  (let*
      ;; Extract the process from the param list.
      ((proc (cdr (assoc :servant params)))
       ;; Preprocess the body if possible.
       (pre (gethash `(,proc :preproc)
		     org-babel-servant-info
		     nil))
       (arg (if pre (funcall pre body params) body))
       ;; Set the timeout.
       (timeout (gethash `(,proc :timeout)
			 org-babel-servant-info
			 1.0)))
    ;; Should make sure we really have a process before
    ;; proceeding further!
    ;; Clear the last output.  Later on, we might want to allow the
    ;; option of archiving the old ouput for safe keeping.
    (puthash `(,proc :lastout) "" org-babel-servant-info)
    (puthash `(,proc :lasterr) "" org-babel-servant-info)
    ;; Record the time
    (puthash `(,proc :timestamp) (current-time) org-babel-servant-info)
    ;; Send out a request.
    (process-send-string (get-process proc) arg)
    ;; Wait for the replies... except it doesn't presently work
    ;;(while (accept-process-output (get-process proc)))
    (accept-process-output (get-process proc))
    ;;; TBA
    ;;(while (accept-process-output (get-process stderr-process)))
    ;;; Coda for post-processing (when required)
    (let ((post (gethash `(,proc :postproc)
			 org-babel-servant-info
			 nil))
	  (lastout (gethash `(,proc :lastout)
			   org-babel-servant-info
			   nil))
	  (lasterr (gethash `(,proc :lasterr)
			   org-babel-servant-info
			   nil)))
      (if post (funcall post lastout params lasterr) lastout))))

(provide 'ob-servant)
