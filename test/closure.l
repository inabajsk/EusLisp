;;; closure.l -- closures with indefinite extent (closure branch)
;;;   Runs the cases in closure/closure-defs.l twice: interpreted, and
;;;   compiled with compile-file.  Each case creates closures, overwrites
;;;   the stack (clobber) and then calls them.
(require :unittest "lib/llib/unittest.l")
(init-unit-test)

(setq *closure-dir*
      (concatenate string (or (car *loader-current-directory*) "./") "closure/"))
(setq *closure-src* (concatenate string *closure-dir* "closure-defs.l"))

(defun run-closure-cases (label)
  (dolist (c *closure-tests*)
    (let ((got (funcall (car c))))
      (format t ";; ~A ~A -> ~S~%" label (car c) got)
      (assert (equal got (cadr c))
              (format nil "~A ~A: got ~S expected ~S" label (car c) got (cadr c))))))

(deftest closure-interpreted
  (load *closure-src*)
  (run-closure-cases "interp"))

(deftest closure-compiled
  (let* ((tmp (format nil "/tmp/eus-closure-~A/" (unix:getpid)))
         (src (concatenate string tmp "closure-defs.l")))
    (unix:system (format nil "mkdir -p ~A && cp ~A ~A" tmp *closure-src* src))
    (compile-file src)
    (load (concatenate string tmp "closure-defs.so"))
    (run-closure-cases "comp")
    (unix:system (format nil "rm -rf ~A" tmp))))

(run-all-tests)
(exit)
