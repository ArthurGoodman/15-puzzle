(load "utility.lisp")
(load "puzzle.lisp")
(load "heuristic.lisp")
(load "solver.lisp")

(defvar state '((1 2 3 4) (5 6 7 8) (9 10 11 12) (0 13 14 15) (0 3)))
; (defvar state '((6 7 2 3) (1 4 8 12) (5 10 14 0) (9 13 15 11) (3 2)))
; (defvar state '((14 6 9 2) (10 4 13 3) (1 7 0 12) (5 15 11 8) (2 2)))

(defun trace-solution (solution)
  (let ((s state))
    (loop for move in solution do
      (write s)
      (terpri)
      (setf s (make-move s move)))
    (write s)))

(defun main ()
  ; (write (solve state))
  (trace-solution (solve state))
  (terpri))
