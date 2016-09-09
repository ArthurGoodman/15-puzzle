(defun get-state (node) (first node))
(defun get-move (node) (second node))
(defun get-parent (node) (third node))
(defun get-heuristic (node) (fourth node))

(defun solution (node)
  (cond
    ((null node) nil)
    (t (append (solution (get-parent node)) `(,(get-move node))))))

(defun filter (lst1 lst2)
  (cond
    ((or (null lst1) (null lst2)) lst1)
    ((member-state (get-state (car lst1)) lst2) (filter (cdr lst1) lst2))
    (t (cons (car lst1) (filter (cdr lst1) lst2)))))

(defun member-state (state nodes)
  (cond
    ((null nodes) nil)
    ((equal (get-state (car nodes)) state) nodes)
    (t (member-state state (cdr nodes)))))

(defun successors (node)
  (let ((new-states nil)
        (new-nodes nil)
        (state (get-state node)))
    (loop for move in move-symbols do
      (setf new-states `(,(make-move state move)))
      (setf new-nodes (append new-nodes
        (mapcar (lambda (new-state)
          (cond
            ((null new-state) nil)
            (t `(,new-state ,move ,node ,(manhattan new-state)))))
        new-states))))
    new-nodes))

(defun solve (state)
  (let ((ahead `((,state ,nil ,nil ,(manhattan state)))) 
        (behind nil)
        (node nil)
        (children nil))
    (loop
      (if (null ahead)
        (return-from solve nil))
      (setf node (pop ahead))
      (if (equal (get-state node) goal)
        (return-from solve (remove nil (solution node))))
      (push node behind)
      (setf children (remove nil (successors node)))
      (setf children (filter children behind))
      (setf ahead (append ahead children))
      (setf ahead (sort ahead (lambda (node1 node2) (< (get-heuristic node1) (get-heuristic node2))))))))
