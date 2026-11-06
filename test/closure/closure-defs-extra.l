;;; closure-defs-extra.l -- cases kept in a separate file because the
;;; original compiler stops compiling the whole file on them
;;; (loaded after closure-defs.l)

;; 23 return-from the name of a labels function (implicit block)
(defun make-finder ()
  (labels ((find-neg (l)
             (dolist (x l) (if (< x 0) (return-from find-neg x)))
             nil))
    #'find-neg))
(defun test-labels-block ()
  (let ((f (make-finder))) (clobber 40) (list (funcall f '(1 2 -3 4)) (funcall f '(1 2)))))


;; 24 slot access (obj . slot) whose object variable has a macro name (pf):
;;    the capture analysis must not macroexpand it (found in kxreus)
(defclass cdx-pf :super object :slots (val))
(defun make-pf-reader (pf) #'(lambda () (pf . val)))
(defun test-ovaf-macro-name ()
  (let ((o (instantiate cdx-pf)))
    (setq (o . val) 42)
    (let ((f (make-pf-reader o))) (clobber 40) (funcall f))))
