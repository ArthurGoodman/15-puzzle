(defun get-element (lst i)
  (cond
    ((eq i 0) (car lst))
    (t (get-element (cdr lst) (- i 1)))))

(defun set-element (lst i value)
  (cond
    ((null lst) lst)
    ((eq i 0) (cons value (set-element (cdr lst) (- i 1) value)))
    (t (cons (car lst) (set-element (cdr lst) (- i 1) value)))))

(defun pos (lst value)
  (cond
    ((equal value (car lst)) 0)
    (t (+ (pos (cdr lst) value) 1))))
