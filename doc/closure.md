# Closures with indefinite extent (branch `closure`)

Before this branch, EusLisp closures were only valid while the function
that created them was still running: `(lambda-closure name bindfp fletfp . body)`
kept raw stack addresses, and compiled closures (`makeclosure`) kept `argv`
and `local` pointers into the creator's stack frame.  Returning a closure
from a function and calling it later crashed (Segmentation Fault) or read
garbage.

This branch makes both the interpreter and the compiler create closures
that live in the heap, so they can be returned, stored and called later
(counters, generators, `compose`, `labels` functions returned with `#'`, ...).

## Switching: `*closure-mode*`

```lisp
*closure-mode*            ; => t   (default) closures of indefinite extent
(setq *closure-mode* nil) ; original stack-based closures
(let ((*closure-mode* nil)) ...) ; only inside this form
```

* Interpreter: checked each time `(function (lambda ...))` / `#'local-fn`
  is evaluated, so it takes effect immediately.
* Compiler: checked at **compile time**.  Code compiled with `nil` is the
  same as the original compiler's output (`makeclosure`, no boxes).
* Closures already made keep working after the mode is changed.
* With `nil`, `test/closure/run-tests.sh` gives exactly the same
  PASS / FAIL / CRASH pattern as the original jskeus build (8 / 42).

## Interpreter (`lisp/c/eval.c`, `specials.c`)

* `(function (lambda ...))` now builds
  `(lambda-closure name ENV 0 formals . body)`, where `ENV` is a
  `label-reference` object labeled `sys::closure-env`:
  * `value`    = `((var . box) ...)`  lexical variables, innermost first
  * `unsolved` = `((fname . closure) ...)` flet/labels functions
* Only variables (and local functions) whose names occur in the lambda are
  captured.  Each captured variable is moved into a **box**
  (`label-reference` labeled `sys::closure-box`).  The creator's stack
  frame is updated to hold the same box, so assignments by either side are
  visible to the other.  `getval`/`setval` dereference boxes.
* `#'f` for a flet/labels function makes that function heap-resident
  (`heapify_flet`); labels functions can refer to themselves.
* When a heap closure is called, `ufuncall` rebuilds the captured frames
  on the stack (`rebuild_benv`, `rebuild_fenv`) and runs the body with them.
* Closures made by `makeflet` and the `unwind-protect` cleaner keep the old
  stack-address form (they have dynamic extent) until they are captured.

## Compiler (`lisp/comp/comp.l`, `trans.l`) and runtime (`makes.c`, `memory.c`)

* Before compiling a function body, `comp-captured-vars` walks it
  (expanding macros in evaluated positions) and collects the symbols that
  appear inside inner closures (`#'(lambda ..)`, flet/labels definitions,
  `((lambda ..) ..)`).
* A captured local variable is **boxed**: its stack slot holds a cons
  cell `(value . nil)`; loads and stores use `->c.cons.car`.  A captured
  required argument is first copied into a new local slot.
* `#'(lambda ...)`, `flet` and `labels` create closures with
  `makeclosure3`: `argv` (for `self` / slot access) is copied into a heap
  vector `ENVVEC`, and only the boxes and local functions of the current
  level are copied into a compact frame (`env2[0..n)`); the inner
  compiler's offsets are renumbered accordingly (`:compact-closure-frame`).
  For `labels`, the frames are filled after all sibling closures exist.
* `struct closure` has a new slot `envvec`; the GC marks `envvec` and `env0`
  of heap closures (stack-based closures are still skipped).
* `unwind-protect` cleaners still use the stack-based `makeclosure`.

## Tests

* `test/closure.l` (unittest): 21 cases, interpreted and compiled.
* `test/closure/run-tests.sh <env.sh> eus <label>` runs every case in its
  own process (a crash does not hide the other cases), interpreted and
  compiled.  The same `closure-defs.l` also runs on other EusLisp builds.
* `test/closure/bench/bench.sh <env.sh> <label>`: micro benchmarks.

Results (2026-11-06, WSL2 Ubuntu 24.04, x86_64):

| build | closure cases (interp + comp) |
|---|---|
| jskeus master (EusLisp 9.32) | 8 / 42 |
| this branch | 42 / 42 (`*closure-mode*` nil: 8 / 42, same as master) |
| affonso-gui/EusLisp `eus10` | 42 / 42 |

`test/*.l` of EusLisp and `irteus/test` give the same results as the
original build.

## Known limitations

* An `unwind-protect` cleanup form that creates a closure which escapes
  after the function returns may still refer to the dead stack frame.
* The capture analysis is textual: a macro that expands into a reference
  to a variable that does not appear in the source of the lambda is not
  seen (the closure then gets a copy, not a shared box, in compiled code).
* The RGC collector (`collector.c`) was not updated.
