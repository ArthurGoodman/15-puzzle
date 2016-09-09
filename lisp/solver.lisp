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

(defun successors (node)
  (let ((new-state nil)
        (new-nodes nil)
        (state (get-state node)))
    (loop for move in move-symbols do
      (setf new-state (make-move state move))
      (if new-state
        (setf new-nodes (append new-nodes `((,new-state ,move ,node ,(manhattan new-state)))))))
    new-nodes))

(defun solve (state)
  (let ((frontier `((,state ,nil ,nil ,(manhattan state)))) 
        (visited nil)
        (node nil)
        (children nil))
    (loop
      (if (null frontier)
        (return-from solve nil))
      (setf node (pop frontier))
      (if (equal (get-state node) goal)
        (return-from solve (remove nil (solution node))))
      (push (get-state node) visited)
      (setf children (remove nil (successors node)))
      (setf children (set-difference children visited))
      (setf frontier (append frontier children))
      (setf frontier (sort frontier (lambda (node1 node2) (< (get-heuristic node1) (get-heuristic node2))))))))
