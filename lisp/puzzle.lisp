(defvar goal '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 0) (3 3)))

(defun get-0-x (state)
  (car (get-element state 4)))

(defun get-0-y (state)
  (cadr (get-element state 4)))

(defun set-0-x (state value)
  (set-element state 4 (set-element (get-element state 4) 0 value))) 

(defun set-0-y (state value)
  (set-element state 4 (set-element (get-element state 4) 1 value))) 

(defun get-tile (grid x y)
  (cond
    ((null grid) grid)
    ((eq y 0) (get-element (car grid) x))
    (t (get-tile (cdr grid) x (- y 1)))))

(defun set-tile (grid x y value)
  (cond
    ((null grid) grid)
    ((eq y 0) (cons (set-element (car grid) x value) (set-tile (cdr grid) x (- y 1) value)))
    (t (cons (car grid) (set-tile (cdr grid) x (- y 1) value)))))

(defun get-x (state value)
  (pos (get-element state (get-y state value)) value))

(defun get-y (state value)
  (cond
    ((null state) nil)
    ((find value (car state)) 0)
    (t (+ (get-y (cdr state) value) 1))))

(defun find-zero (grid)
  (dotimes (x 4)
    (dotimes (y 4)
      (if (eq (get-tile grid x y) 0)
        (return-from find-zero `(,x ,y))))))

(defun grid-to-state (grid)
  (append grid `(,(find-zero grid))))
