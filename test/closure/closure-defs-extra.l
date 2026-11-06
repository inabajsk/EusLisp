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

