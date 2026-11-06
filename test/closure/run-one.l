;;; usage: TEST=test-counter MODE=interp|comp [CLOSURE_MODE=nil] eus run-one.l
(if (equal (unix:getenv "CLOSURE_MODE") "nil") (setq *closure-mode* nil))
(let ((mode (unix:getenv "MODE")))
  (if (equal mode "comp")
      (load (format nil "~A/closure-defs.so" (unix:getenv "TESTDIR")))
    (load (format nil "~A/closure-defs.l" (unix:getenv "TESTDIR")))))
;; extra cases: if compiling them failed, only those cases fail
(let ((f (format nil "~A/closure-defs-extra.~A" (unix:getenv "TESTDIR")
                 (if (equal (unix:getenv "MODE") "comp") "so" "l"))))
  (if (probe-file f) (load f)))
(let* ((name (read-from-string (unix:getenv "TEST")))
       (ent (assoc name *closure-tests*))
       (got (funcall name)))
  (format t "~A ~A got=~S expected=~S~%"
          (if (equal got (cadr ent)) "PASS" "FAIL") name got (cadr ent)))
(exit 0)
