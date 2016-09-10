(defun get-state (node)
  (first node))

(defun get-move (node)
  (second node))

(defun get-parent (node)
  (third node))

(defun get-heuristic (node)
  (fourth node))

(defun solution (node)
  (cond
    ((null node) nil)
    (t (append (solution (get-parent node)) `(,(get-move node))))))

(defun print-state (state)
  (write (reverse (cdr (reverse state))))
  (terpri))

(defun trace-solution (start solution)
  (let ((s (grid-to-state start)))
    (loop for move in solution do
      (print-state s)
      (setf s (make-move s move)))
    (print-state s)))

(defun filter (nodes visited)
  (cond
    ((or (null nodes) (null visited)) nodes)
    ((member (get-state (car nodes)) visited :test #'equal) (filter (cdr nodes) visited))
    (t (cons (car nodes) (filter (cdr nodes) visited)))))

(defun successors (node)
  (let ((new-state nil)
        (new-nodes nil)
        (state (get-state node)))
    (loop for move in move-symbols do
      (setf new-state (make-move state move))
      (if new-state
        (setf new-nodes (append new-nodes `((,new-state ,move ,node ,(manhattan new-state)))))))
    new-nodes))

(defun solve (grid)
  (let ((state (grid-to-state grid))
        (frontier nil)
        (visited nil)
        (node nil))
    (setf frontier `((,state ,nil ,nil ,(manhattan state))))
    (loop
      (if (null frontier)
        (return-from solve nil))
      (setf node (pop frontier))
      (if (equal (get-state node) goal)
        (return-from solve (remove nil (solution node))))
      (push (get-state node) visited)
      (setf frontier (append frontier (filter (successors node) visited)))
      (setf frontier (sort frontier (lambda (node1 node2)
        (< (get-heuristic node1) (get-heuristic node2))))))))
