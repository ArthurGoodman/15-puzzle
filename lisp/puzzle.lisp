(defvar goal '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 0) (3 3)))
(defvar move-symbols '(up left down right))

(defun get-0-x (state)
  (car (get-element state (- (length state) 1))))

(defun get-0-y (state)
  (cadr (get-element state (- (length state) 1))))

(defun set-0-x (state value)
  (set-element state (- (length state) 1) (set-element (get-element state (- (length state) 1)) 0 value))) 

(defun set-0-y (state value)
  (set-element state (- (length state) 1) (set-element (get-element state (- (length state) 1)) 1 value))) 

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

(defun move-up (state)
  (let ((x (get-0-x state))
        (y (get-0-y state))
        (result state))
    (if (eq y 0)
      (return-from move-up nil))
    (setf result (set-tile result x y (get-tile result x (- y 1))))
    (setf result (set-tile result x (- y 1) 0))
    (setf result (set-0-y result (- y 1)))
    result))

(defun move-left (state)
  (let ((x (get-0-x state))
        (y (get-0-y state))
        (result state))
    (if (eq x 0)
      (return-from move-left nil))
    (setf result (set-tile result x y (get-tile result (- x 1) y)))
    (setf result (set-tile result (- x 1) y 0))
    (setf result (set-0-x result (- x 1)))
    result))

(defun move-down (state)
  (let ((x (get-0-x state))
        (y (get-0-y state))
        (result state))
    (if (eq y (- (length result) 2))
      (return-from move-down nil))
    (setf result (set-tile result x y (get-tile result x (+ y 1))))
    (setf result (set-tile result x (+ y 1) 0))
    (setf result (set-0-y result (+ y 1)))
    result))

(defun move-right (state)
  (let ((x (get-0-x state))
        (y (get-0-y state))
        (result state))
    (if (eq x (- (length (car result)) 1))
      (return-from move-right nil))
    (setf result (set-tile result x y (get-tile result (+ x 1) y)))
    (setf result (set-tile result (+ x 1) y 0))
    (setf result (set-0-x result (+ x 1)))
    result))

(defun make-move (state move)
  (case move
    ('up (move-up state))
    ('left (move-left state))
    ('down (move-down state))
    ('right (move-right state))))
