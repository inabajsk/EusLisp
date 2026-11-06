;;; loop.l -- Common Lisp extended LOOP for EusLisp
;;;
;;;   (load "lib/llib/loop.l")  replaces LOOP.  A loop whose body consists only
;;;   of compound forms is expanded exactly as the original EusLisp LOOP
;;;   (an infinite loop in a block nil), so existing code is not affected.
;;;
;;;   Supported (ANSI CL 6.1):
;;;     named, with ... [= ...] {and ...}, initially, finally,
;;;     for/as  v [type] from/upfrom/downfrom e  to/upto/below/downto/above e  by e
;;;             v in list [by fn],  v on list [by fn],  v = e [then e],
;;;             v across vector,
;;;             v being {the|each} {hash-key[s]|hash-value[s]} {in|of} h
;;;                     [using (hash-value|hash-key w)]
;;;             v being {the|each} {symbol[s]|present-symbol[s]|external-symbol[s]} {in|of} pkg
;;;             {and ...} parallel stepping, destructuring patterns, of-type
;;;     repeat, while, until, always, never, thereis,
;;;     do/doing, return, collect/append/nconc/count/sum/maximize/minimize [into v],
;;;     when/if/unless ... [and ...] [else ...] [end], it,
;;;     loop-finish
;;;
;;;   The expansion is a BLOCK around a LET* (all variables) and a TAGBODY:
;;;     (block name (let* (vars..) (tagbody init.. initially..
;;;        next (header: first-time tests / steps and tests) body.. (go next)
;;;        end finally.. (return-from name result))))
;;;   Loop keywords are recognized by their symbol names, in any package.

(in-package "LISP")
(export '(loop-finish))

(defvar *loop-end-tag* (intern "%LOOP-END" "SYSTEM"))
(defmacro loop-finish () `(go ,*loop-end-tag*))

;; note: in EusLisp (return) inside dolist leaves only the inner while,
;; and dolist then returns its result form -- so member is used here
(defun loop-kw-p (x &rest names)
  (and (symbolp x) x
       (member (symbol-name x) names :test #'string=) t))

(defvar *loop-keywords*
  '("NAMED" "WITH" "INITIALLY" "FINALLY" "FOR" "AS" "REPEAT" "WHILE" "UNTIL"
    "ALWAYS" "NEVER" "THEREIS" "DO" "DOING" "RETURN" "COLLECT" "COLLECTING"
    "APPEND" "APPENDING" "NCONC" "NCONCING" "COUNT" "COUNTING" "SUM" "SUMMING"
    "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING" "WHEN" "IF" "UNLESS"
    "ELSE" "END" "AND"))
(defun loop-keyword-p (x)
  (and (symbolp x) x (member (symbol-name x) *loop-keywords* :test #'string=)))

;; ---------------------------------------------------------------- state
(defvar *loop-tokens*)		; remaining clause tokens
(defvar *loop-name*)
(defvar *loop-vars*)		; ((var init) ...) reversed
(defvar *loop-init*)		; forms run once before initially (reversed)
(defvar *loop-initially*)
(defvar *loop-finally*)
(defvar *loop-first*)		; first-iteration header code (reversed)
(defvar *loop-step*)		; later-iteration header code (reversed)
(defvar *loop-body*)		; body code (reversed)
(defvar *loop-result*)		; default result form
(defvar *loop-accums*)		; ((name var tail kind) ...)
(defvar *loop-it*)		; variable holding the value of a when test
(defvar *loop-par-pre* nil)	; parallel (and) steps: values computed first
(defvar *loop-user-vars* nil)	; variables named by the user (duplicates are errors)
(defvar *loop-kinds* nil)	; :value (default accumulation) / :boolean (always ...)

(defun loop-pop () (pop *loop-tokens*))
(defun loop-peek () (car *loop-tokens*))
(defun loop-var (var &optional init)
  (unless (assoc var *loop-vars*) (push (list var init) *loop-vars*))
  var)
(defun loop-gensym (s) (loop-var (gensym s)))

;; compound forms up to the next loop keyword (do, initially, finally)
(defun loop-compound-forms ()
  (let (r)
    (while (and *loop-tokens* (consp (loop-peek))) (push (loop-pop) r))
    (nreverse r)))

;; optional type spec after a variable:  of-type x | fixnum | float | t | nil
(defun loop-skip-type ()
  (cond ((loop-kw-p (loop-peek) "OF-TYPE") (loop-pop) (loop-pop))
        ((loop-kw-p (loop-peek) "FIXNUM" "FLOAT" "T" "NIL" "INTEGER" "NUMBER")
         (if (not (eq (loop-peek) nil)) (loop-pop)))
        ((and (consp (loop-peek)) (consp *loop-tokens*)
              (every #'(lambda (x) (loop-kw-p x "FIXNUM" "FLOAT" "T" "NIL"))
                     (let ((l (loop-peek)) r) (while (consp l) (push (car l) r) (setq l (cdr l))) r)))
         (loop-pop))))

;; destructuring: declare the variables of PATTERN, return code that sets
;; them from the value of FORM
(defun loop-user-var (v)
  ;; ANSI 6.1.1.7: a variable may be bound only once in a loop
  (if (member v *loop-user-vars*)
      (error "loop: variable ~s is bound more than once" v))
  (push v *loop-user-vars*)
  (loop-var v))
(defun loop-declare-pattern (pat)
  (cond ((null pat) nil)
        ((symbolp pat) (loop-user-var pat))
        ((consp pat) (loop-declare-pattern (car pat)) (loop-declare-pattern (cdr pat)))))
(defun loop-destructure (pat form)
  (cond ((null pat) nil)
        ((symbolp pat) (list `(setq ,pat ,form)))
        (t (let ((tmp (loop-gensym "D")))
             (append (list `(setq ,tmp ,form))
                     (loop-destructure (car pat) `(car ,tmp))
                     (loop-destructure (cdr pat) `(cdr ,tmp)))))))
(defun loop-setter (pat form)		; one form
  (let ((l (loop-destructure pat form)))
    (if (cdr l) (cons 'progn l) (or (car l) form))))

(defun loop-end-test (test) `(when ,test (go ,*loop-end-tag*)))

;; ---------------------------------------------------------------- for / as
;; each for-clause adds to *loop-init* (set up), *loop-first* (first time)
;; and returns step code; 'and'-connected clauses step in parallel
(defun loop-for-clause ()
  (let* ((var (loop-pop)) kw)
    (loop-skip-type)
    (loop-declare-pattern var)
    (setq kw (loop-pop))
    (cond
     ((loop-kw-p kw "FROM" "UPFROM" "DOWNFROM" "TO" "UPTO" "BELOW" "DOWNTO" "ABOVE" "BY")
      (push kw *loop-tokens*)
      (loop-for-arith var))
     ((loop-kw-p kw "IN") (loop-for-in var nil))
     ((loop-kw-p kw "ON") (loop-for-in var t))
     ((loop-kw-p kw "=")
      (let ((init (loop-pop)))
        (if (loop-kw-p (loop-peek) "THEN")
            (let ((then (progn (loop-pop) (loop-pop))) (tmp (loop-gensym "NEW")))
              (push (loop-setter var init) *loop-first*)
              ;; the new value is computed before any variable of an
              ;; 'and' group is changed
              (push `(setq ,tmp ,then) *loop-par-pre*)
              (list (loop-setter var tmp)))
          (progn (push (loop-setter var init) *loop-first*)
                 (list (loop-setter var init))))))
     ((loop-kw-p kw "ACROSS")
      (let ((vec (loop-gensym "V")) (idx (loop-gensym "I")) (len (loop-gensym "N")))
        (push `(setq ,vec ,(loop-pop) ,idx 0) *loop-init*)
        (push `(setq ,len (length ,vec)) *loop-init*)
        (push (loop-end-test `(>= ,idx ,len)) *loop-first*)
        (push (loop-setter var `(aref ,vec ,idx)) *loop-first*)
        (list `(setq ,idx (1+ ,idx)) (loop-end-test `(>= ,idx ,len))
              (loop-setter var `(aref ,vec ,idx)))))
     ((loop-kw-p kw "BEING") (loop-for-being var))
     (t (error "loop: unknown for clause keyword ~s" kw)))))

(defun loop-for-arith (var)
  (let ((start 0) end by (dir nil) (incl t) kw
        (endv (loop-gensym "END")) (byv (loop-gensym "BY")))
    (while (loop-kw-p (loop-peek) "FROM" "UPFROM" "DOWNFROM" "TO" "UPTO" "BELOW" "DOWNTO" "ABOVE" "BY")
      (setq kw (loop-pop))
      (cond ((loop-kw-p kw "FROM") (setq start (loop-pop)))
            ((loop-kw-p kw "UPFROM") (setq start (loop-pop) dir :up))
            ((loop-kw-p kw "DOWNFROM") (setq start (loop-pop) dir :down))
            ((loop-kw-p kw "TO" "UPTO") (setq end (loop-pop)) (if (loop-kw-p kw "UPTO") (setq dir :up)))
            ((loop-kw-p kw "BELOW") (setq end (loop-pop) incl nil dir :up))
            ((loop-kw-p kw "DOWNTO") (setq end (loop-pop) dir :down))
            ((loop-kw-p kw "ABOVE") (setq end (loop-pop) incl nil dir :down))
            ((loop-kw-p kw "BY") (setq by (loop-pop)))))
    (let* ((down (eq dir :down))
           (test (if end
                     (if down
                         (if incl `(< ,var ,endv) `(<= ,var ,endv))
                       (if incl `(> ,var ,endv) `(>= ,var ,endv))))))
      (push `(setq ,var ,start) *loop-init*)
      (if end (push `(setq ,endv ,end) *loop-init*))
      (push `(setq ,byv ,(or by 1)) *loop-init*)
      (if test (push (loop-end-test test) *loop-first*))
      (append (list `(setq ,var ,(if down `(- ,var ,byv) `(+ ,var ,byv))))
              (if test (list (loop-end-test test)))))))

(defun loop-for-in (var on)
  (let* ((tail (loop-gensym "L")) (lst (loop-pop))
         (by (if (loop-kw-p (loop-peek) "BY") (progn (loop-pop) (loop-pop))))
         (stepf (if by `(funcall ,by ,tail) `(cdr ,tail))))
    (push `(setq ,tail ,lst) *loop-init*)
    (push (loop-end-test `(atom ,tail)) *loop-first*)
    (push (loop-setter var (if on tail `(car ,tail))) *loop-first*)
    (list `(setq ,tail ,stepf) (loop-end-test `(atom ,tail))
          (loop-setter var (if on tail `(car ,tail))))))

;; hash tables and packages are turned into a list first
(defun loop-for-being (var)
  (if (loop-kw-p (loop-peek) "THE" "EACH") (loop-pop))
  (let ((what (loop-pop)) src other lst)
    (if (loop-kw-p (loop-peek) "IN" "OF") (loop-pop))
    (setq src (loop-pop))
    (when (loop-kw-p (loop-peek) "USING")
      (loop-pop)
      (setq other (cadr (loop-pop)))
      (loop-declare-pattern other))
    (cond
     ((loop-kw-p what "HASH-KEY" "HASH-KEYS" "HASH-VALUE" "HASH-VALUES")
      (let ((keyp (loop-kw-p what "HASH-KEY" "HASH-KEYS")))
        (setq lst `(let ((r nil)) (maphash #'(lambda (k v) (push (cons k v) r)) ,src) (nreverse r)))
        (let ((pair (gensym "P")))
          (loop-declare-pattern pair)
          ;; iterate over the (key . value) list like FOR pair IN list
          (let ((steps (progn (push lst *loop-tokens*) (loop-for-in pair nil))))
            (let ((extract (list (loop-setter var (if keyp `(car ,pair) `(cdr ,pair))))))
              (if other (push (loop-setter other (if keyp `(cdr ,pair) `(car ,pair))) extract))
              (dolist (e extract) (push e *loop-first*))
              (append steps extract))))))
     ((loop-kw-p what "SYMBOL" "SYMBOLS" "PRESENT-SYMBOL" "PRESENT-SYMBOLS" "EXTERNAL-SYMBOL" "EXTERNAL-SYMBOLS")
      (setq lst (if (loop-kw-p what "EXTERNAL-SYMBOL" "EXTERNAL-SYMBOLS")
                    `(let ((r nil)) (do-external-symbols (s (find-package ,src)) (push s r)) r)
                  `(let ((r nil)) (do-symbols (s (find-package ,src)) (push s r)) r)))
      (push lst *loop-tokens*)
      (loop-for-in var nil))
     (t (error "loop: unknown being clause ~s" what)))))

;; ---------------------------------------------------------------- accumulation
(defun loop-accumulator (kind into)
  ;; kind: :list (collect/append/nconc), :count, :sum, :max, :min
  (let ((a (assoc (or into :default) *loop-accums*)))
    (unless a
      (if into (loop-user-var into))	; into var must not be bound by with/for
      (unless into (loop-kind :value))
      (let ((var (or into (gensym "ACC"))) (tail (gensym "TAIL")))
        (loop-var var (case kind ((:count :sum) 0) (t nil)))
        (loop-var tail nil)
        (setq a (list (or into :default) var tail kind))
        (push a *loop-accums*)
        (unless into (setq *loop-result* var))))
    a))

(defun loop-kind (k)
  ;; a loop may not both accumulate a default value and use always/never/thereis
  (if (and *loop-kinds* (not (eq *loop-kinds* k)))
      (error "loop: value accumulation and always/never/thereis in one loop"))
  (setq *loop-kinds* k))

(defun loop-accum-code (kw form into)
  (let* ((kind (cond ((loop-kw-p kw "COLLECT" "COLLECTING" "APPEND" "APPENDING" "NCONC" "NCONCING") :list)
                     ((loop-kw-p kw "COUNT" "COUNTING") :count)
                     ((loop-kw-p kw "SUM" "SUMMING") :sum)
                     ((loop-kw-p kw "MAXIMIZE" "MAXIMIZING") :max)
                     (t :min)))
         (a (loop-accumulator kind into)) (var (cadr a)) (tail (caddr a)) (c (gensym "C")))
    (case kind
      (:list
       (let ((piece (cond ((loop-kw-p kw "COLLECT" "COLLECTING") `(list ,form))
                          ((loop-kw-p kw "APPEND" "APPENDING") `(copy-list ,form))
                          (t form))))
         `(let ((,c ,piece))
            (when ,c
              (if ,var (rplacd (last ,(if into var tail)) ,c) (setq ,var ,c))
              (setq ,tail (last ,c))))))
      (:count `(when ,form (setq ,var (1+ ,var))))
      (:sum `(setq ,var (+ ,var ,form)))
      (:max `(let ((,c ,form)) (setq ,var (if ,var (max ,var ,c) ,c))))
      (:min `(let ((,c ,form)) (setq ,var (if ,var (min ,var ,c) ,c)))))))

;; ---------------------------------------------------------------- main clauses
(defun loop-form-arg ()		; the form after do-like keywords; IT inside when
  (let ((f (loop-pop)))
    (if (and *loop-it* (loop-kw-p f "IT")) *loop-it* f)))

;; one selectable clause -> code
(defun loop-selectable ()
  (let ((kw (loop-pop)))
    (cond
     ((loop-kw-p kw "DO" "DOING") `(progn ,@(loop-compound-forms)))
     ((loop-kw-p kw "RETURN")
      (if (loop-kw-p (loop-peek) "IT")
          (progn (loop-pop) `(return-from ,*loop-name* ,*loop-it*))
        `(return-from ,*loop-name* ,(loop-pop))))
     ((loop-kw-p kw "COLLECT" "COLLECTING" "APPEND" "APPENDING" "NCONC" "NCONCING" "COUNT" "COUNTING"
                 "SUM" "SUMMING" "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING")
      (let ((form (loop-form-arg)) into)
        (when (loop-kw-p (loop-peek) "INTO") (loop-pop) (setq into (loop-pop)))
        (loop-skip-type)
        (loop-accum-code kw form into)))
     ((loop-kw-p kw "WHEN" "IF" "UNLESS") (loop-conditional kw))
     (t (error "loop: unknown clause ~s" kw)))))

(defun loop-clause-group ()		; clause {and clause}*
  (let ((r (list (loop-selectable))))
    (while (loop-kw-p (loop-peek) "AND") (loop-pop) (push (loop-selectable) r))
    `(progn ,@(nreverse r))))

(defun loop-conditional (kw)
  (let* ((test (loop-pop)) (itv (gensym "IT")) then else)
    (loop-var itv)
    (let ((*loop-it* itv))
      (setq then (loop-clause-group))
      (when (loop-kw-p (loop-peek) "ELSE")
        (loop-pop)
        (setq else (loop-clause-group))))
    (if (loop-kw-p (loop-peek) "END") (loop-pop))
    (if (loop-kw-p kw "UNLESS")
        `(if (setq ,itv ,test) ,else ,then)
      `(if (setq ,itv ,test) ,then ,else))))

(defun loop-parse-clause ()
  (let ((kw (loop-peek)))
    (cond
     ((loop-kw-p kw "NAMED") (loop-pop) (setq *loop-name* (loop-pop)))
     ((loop-kw-p kw "WITH")
      (loop-pop)
      (let (steps)
        (loop
          (let ((var (loop-pop)))
            (loop-skip-type)
            (loop-declare-pattern var)
            (if (loop-kw-p (loop-peek) "=")
                (progn (loop-pop) (push (loop-setter var (loop-pop)) steps))))
          (if (loop-kw-p (loop-peek) "AND") (loop-pop) (return)))
        (dolist (s (nreverse steps)) (push s *loop-init*))))
     ((loop-kw-p kw "INITIALLY") (loop-pop) (setq *loop-initially* (append *loop-initially* (loop-compound-forms))))
     ((loop-kw-p kw "FINALLY") (loop-pop) (setq *loop-finally* (append *loop-finally* (loop-compound-forms))))
     ((loop-kw-p kw "FOR" "AS")
      (loop-pop)
      (let ((*loop-par-pre* nil) (groups nil))
        (push (loop-for-clause) groups)
        (while (loop-kw-p (loop-peek) "AND")
          (loop-pop) (push (loop-for-clause) groups))
        ;; parallel stepping: the 'then' values of the group first
        (dolist (f (reverse *loop-par-pre*)) (push f *loop-step*))
        (dolist (g (nreverse groups)) (dolist (f g) (push f *loop-step*)))))
     ((loop-kw-p kw "REPEAT")
      (loop-pop)
      (let ((cnt (loop-gensym "R")))
        (push `(setq ,cnt ,(loop-pop)) *loop-init*)
        (push (loop-end-test `(<= ,cnt 0)) *loop-first*)
        (push `(setq ,cnt (1- ,cnt)) *loop-step*)
        (push (loop-end-test `(<= ,cnt 0)) *loop-step*)))
     ((loop-kw-p kw "WHILE") (loop-pop) (push (loop-end-test `(not ,(loop-pop))) *loop-body*))
     ((loop-kw-p kw "UNTIL") (loop-pop) (push (loop-end-test (loop-pop)) *loop-body*))
     ((loop-kw-p kw "ALWAYS")
      (loop-pop) (loop-kind :boolean) (setq *loop-result* t)
      (push `(unless ,(loop-pop) (return-from ,*loop-name* nil)) *loop-body*))
     ((loop-kw-p kw "NEVER")
      (loop-pop) (loop-kind :boolean) (setq *loop-result* t)
      (push `(when ,(loop-pop) (return-from ,*loop-name* nil)) *loop-body*))
     ((loop-kw-p kw "THEREIS")
      (loop-pop) (loop-kind :boolean)
      (let ((v (gensym "T")))
        (push `(let ((,v ,(loop-pop))) (when ,v (return-from ,*loop-name* ,v))) *loop-body*)))
     ((loop-keyword-p kw) (push (loop-selectable) *loop-body*))
     (t (error "loop: unknown clause ~s" kw)))))

(defun loop-expand-extended (forms)
  (let ((*loop-tokens* forms) (*loop-name* nil) (*loop-vars* nil) (*loop-init* nil)
        (*loop-initially* nil) (*loop-finally* nil) (*loop-first* nil) (*loop-step* nil)
        (*loop-body* nil) (*loop-result* nil) (*loop-accums* nil) (*loop-it* nil)
        (*loop-user-vars* nil) (*loop-kinds* nil)
        (next (gensym "NEXT")) (first (gensym "FIRST")))
    (while *loop-tokens* (loop-parse-clause))
    `(block ,*loop-name*
       (let* (,@(reverse *loop-vars*) (,first t))
         (tagbody
            ,@(reverse *loop-init*)
            ,@*loop-initially*
          ,next
            (if ,first
                (progn (setq ,first nil) ,@(reverse *loop-first*))
              (progn ,@(reverse *loop-step*)))
            ,@(reverse *loop-body*)
            (go ,next)
          ,*loop-end-tag*
            ,@*loop-finally*
            (return-from ,*loop-name* ,*loop-result*))))))

(defmacro loop (&rest forms)
  (if (every #'consp forms)
      ;; the original EusLisp loop: an infinite loop
      (let ((tag (gensym "LOOP")))
        `(block nil (tagbody ,tag ,@forms (go ,tag))))
    (loop-expand-extended forms)))

(provide :loop)
