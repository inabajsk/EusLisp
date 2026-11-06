;;; closure-defs.l -- closure test cases shared by jskeus(closure branch) and affonso eus10
;;;   Every test is a defun TEST-xxx returning a value compared (equal) with
;;;   the expected value in *closure-tests*.  The file is loaded either as
;;;   source (interpreter) or compiled with compile-file (compiler).
;;;   CLOBBER overwrites the Lisp stack between closure creation and call,
;;;   so a closure that still points into a dead stack frame is detected.

(defun clobber (n &optional (a 'junk1) (b 'junk2) (c 'junk3))
  (let ((x (list a b c)) (y (make-list 3 :initial-element n)))
    (if (> n 0) (clobber (1- n) y x n) (list x y))))

;; 1 upward closure (counter)
(defun make-counter (&optional (n 0)) #'(lambda () (setq n (1+ n))))
(defun test-counter ()
  (let ((c (make-counter 10)))
    (clobber 40)
    (list (funcall c) (funcall c) (progn (clobber 40) (funcall c)))))

;; 2 two closures share one variable
(defun make-account (balance)
  (list #'(lambda (x) (setq balance (+ balance x)))
        #'(lambda () balance)))
(defun test-shared ()
  (let* ((acc (make-account 100)) (dep (first acc)) (bal (second acc)))
    (clobber 40)
    (funcall dep 20) (clobber 40) (funcall dep 5)
    (funcall bal)))

;; 3 creator mutates the variable after the closure was made
(defun make-late ()
  (let ((x 1) f)
    (setq f #'(lambda () x))
    (setq x 2)
    f))
(defun test-late-mutation () (let ((f (make-late))) (clobber 40) (funcall f)))

;; 4 three nested levels (curried add)
(defun curry3 () #'(lambda (a) #'(lambda (b) #'(lambda (c) (list a b c)))))
(defun test-nested3 ()
  (let* ((f1 (funcall (curry3) 1)) (f2 (progn (clobber 30) (funcall f1 2))))
    (clobber 30)
    (funcall f2 3)))

;; 5 adders made by mapcar
(defun make-adder (n) #'(lambda (x) (+ x n)))
(defun test-adders ()
  (let ((fs (mapcar #'make-adder '(1 10 100))))
    (clobber 40)
    (mapcar #'(lambda (f) (funcall f 1000)) fs)))

;; 6 closure over an argument that it mutates (accumulator generator)
(defun make-acc (n) #'(lambda (i) (setq n (+ n i))))
(defun test-accumulator ()
  (let ((a (make-acc 100)))
    (funcall a 10) (clobber 40)
    (list (funcall a 10) (funcall a 1))))

;; 7 &optional &rest &key parameters captured
(defun make-params (a &optional (b 2) &rest r &key (k 9) &allow-other-keys)
  #'(lambda () (list a b r k)))
(defun test-params ()
  (let ((f (make-params 1 20 :k 30)))
    (clobber 40)
    (funcall f)))

;; 8 let* sequential bindings, inner closure refers to earlier binding
(defun make-letstar ()
  (let* ((a 1) (f #'(lambda () a)) (b (+ a 1)))
    (setq a 10)
    #'(lambda () (list (funcall f) b))))
(defun test-letstar () (let ((f (make-letstar))) (clobber 40) (funcall f)))

;; 9 labels function escapes via #'
(defun make-fact ()
  (labels ((fact (n) (if (<= n 1) 1 (* n (fact (1- n)))))) #'fact))
(defun test-labels-escape () (let ((f (make-fact))) (clobber 40) (funcall f 10)))

;; 10 escaping lambda calls an flet function and a captured var
(defun make-sq-plus (k)
  (flet ((sq (x) (* x x)))
    #'(lambda (y) (+ (sq y) k))))
(defun test-flet-capture () (let ((f (make-sq-plus 1))) (clobber 40) (funcall f 7)))

;; 11 one fresh binding per iteration -> distinct values
(defun make-list-of-thunks ()
  (let (r)
    (dolist (i '(1 2 3)) (let ((j i)) (push #'(lambda () j) r)))
    (nreverse r)))
(defun test-loop-binding ()
  (let ((fs (make-list-of-thunks))) (clobber 40) (mapcar #'funcall fs)))

;; 12 closure over self / slot inside a method
(defclass cell :super object :slots (val))
(defmethod cell
  (:init (v) (setq val v) self)
  (:getter () #'(lambda () val))
  (:setter () #'(lambda (v) (setq val v))))
(defun test-method-slot ()
  (let* ((c (instance cell :init 5)) (g (send c :getter)) (s (send c :setter)))
    (clobber 40)
    (funcall s 42) (clobber 40)
    (list (funcall g) (send c :get-val))))
(defmethod cell (:get-val () val))

;; 13 downward closure still updates the outer variable
(defun test-downward ()
  (let ((sum 0)) (mapc #'(lambda (x) (setq sum (+ sum x))) '(1 2 3 4)) sum))

;; 14 many closures survive a GC
(defun test-gc ()
  (let ((fs nil))
    (dotimes (i 200) (push (make-adder i) fs))
    (clobber 40) (sys:gc) (clobber 40)
    (apply #'+ (mapcar #'(lambda (f) (funcall f 0)) fs))))

;; 15 unwind-protect cleanup sees variable changed in the body
(defun test-unwind ()
  (let ((x 1) (log nil))
    (catch 'done
      (unwind-protect (progn (setq x 2) (throw 'done nil))
        (push x log)))
    log))

;; 16 fibonacci generator
(defun make-fib ()
  (let ((a 0) (b 1))
    #'(lambda () (prog1 a (psetq a b b (+ a b))))))
(defun test-generator ()
  (let ((g (make-fib)) r)
    (dotimes (i 10) (clobber 5) (push (funcall g) r))
    (nreverse r)))

;; 17 recursion through a variable that is assigned after closure creation
(defun make-self-ref ()
  (let (f)
    (setq f #'(lambda (n) (if (= n 0) 1 (* n (funcall f (1- n))))))
    f))
(defun test-self-ref () (let ((f (make-self-ref))) (clobber 40) (funcall f 5)))

;; 18 special variables stay dynamic inside closures
(defvar *dyn* 1)
(defun make-dyn () #'(lambda () *dyn*))
(defun test-special ()
  (let ((f (make-dyn)))
    (list (funcall f) (let ((*dyn* 2)) (funcall f)))))

;; 19 closure stored in a global and called from another toplevel form
(defun install-global () (let ((k 7)) (setq *gclo* #'(lambda (x) (* k x)))) t)
(defun test-global () (install-global) (clobber 40) (funcall *gclo* 6))

;; 20 closure captures another closure (compose)
(defun compose (f g) #'(lambda (x) (funcall f (funcall g x))))
(defun test-compose ()
  (let ((h (compose (make-adder 1) (make-adder 10))))
    (clobber 40) (funcall h 100)))

;; 21 heap closure called where no lexical binding is active
;;    (in the interpreter the body of a function without parameters runs
;;    with bindfp = NULL, like forms typed at the toplevel REPL)
(defun test-toplevel ()
  (setq *top-c* (make-counter 0))
  (funcall *top-c*)
  (funcall *top-c*)
  (setq *top-l* (list 1 2 3))
  (list (funcall *top-c*) (length *top-l*)))

;; 22 a variable referenced only through a macro inside the closure
(defmacro hidden-ref () 'hidden-var)
(defun make-hidden ()
  (let ((hidden-var 5) f)
    (setq f #'(lambda () (setq hidden-var (+ (hidden-ref) 1))))
    (setq hidden-var 10)
    f))
(defun test-macro-hidden ()
  (let ((f (make-hidden))) (clobber 40) (list (funcall f) (funcall f))))

(setq *closure-tests*
  '((test-counter (11 12 13))
    (test-shared 125)
    (test-late-mutation 2)
    (test-nested3 (1 2 3))
    (test-adders (1001 1010 1100))
    (test-accumulator (120 121))
    (test-params (1 20 (:k 30) 30))
    (test-letstar (10 2))
    (test-labels-escape 3628800)
    (test-flet-capture 50)
    (test-loop-binding (1 2 3))
    (test-method-slot (42 42))
    (test-downward 10)
    (test-gc 19900)
    (test-unwind (2))
    (test-generator (0 1 1 2 3 5 8 13 21 34))
    (test-self-ref 120)
    (test-special (1 2))
    (test-global 42)
    (test-compose 111)
    (test-toplevel (3 3))
    (test-macro-hidden (11 12))
    (test-labels-block (-3 nil))))
