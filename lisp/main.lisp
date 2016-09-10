(load "utility.lisp")
(load "puzzle.lisp")
(load "moves.lisp")
(load "heuristic.lisp")
(load "solver.lisp")

(defvar grid '((1 2 3 4) (5 6 7 8) (9 10 11 12) (0 13 14 15)))
; (defvar grid '((6 7 2 3) (1 4 8 12) (5 10 14 0) (9 13 15 11)))
; (defvar grid '((14 6 9 2) (10 4 13 3) (1 7 0 12) (5 15 11 8)))

(defun main ()
  ; (write (solve grid))
  (trace-solution grid (solve grid))
  (terpri))
