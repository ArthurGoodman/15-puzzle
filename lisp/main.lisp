(defparameter verbose nil)
(defparameter use-counter nil)

(setf *random-state* (make-random-state t))

(load "utility.lisp")
(load "puzzle.lisp")
(load "moves.lisp")
(load "heuristic.lisp")
(load "solver.lisp")

(defvar grid '((1 2 3 4) (5 6 7 8) (9 10 11 12) (0 13 14 15)))
; (defvar grid '((1 2 3 4) (5 6 7 8) (9 11 10 0) (12 13 14 15)))
; (defvar grid '((6 7 2 3) (1 4 8 12) (5 10 14 0) (9 13 15 11)))
; (defvar grid '((2 3 4 10) (1 11 12 15) (9 14 6 7) (5 13 8 0)))
; (defvar grid '((1 2 12 8) (10 0 4 9) (13 6 3 7) (14 5 15 11)))
; (defvar grid '((14 6 9 2) (10 4 13 3) (1 7 0 12) (5 15 11 8)))
; (defvar grid '((4 15 13 8) (6 11 10 2) (7 5 1 9) (14 3 12 0)))

(defun main ()
  ; (write (solve grid))
  (trace-solution grid (solve grid))
  (terpri))
