(defun manhattan (state)
  (let ((sum 0))
    (loop for i from 0 to 15 do
      (setf sum (+ sum (+ (abs (- (get-y state i) (get-y goal i))) (abs (- (get-x state i) (get-x goal i)))))))
    sum))
