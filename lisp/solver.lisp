(load "params.lisp")

(defun get-state (node)
  (first node))

(defun get-move (node)
  (second node))

(defun get-parent (node)
  (third node))

(defun get-heuristic (node)
  (fourth node))

(defun get-counter (node)
  (fifth node))

(defun solution (node)
  (cond
    ((null node) nil)
    (t (append (solution (get-parent node)) `(,(get-move node))))))

(defun print-state (state)
  (write (reverse (cdr (reverse state)))))

(defun trace-solution (grid solution)
  (if verbose
    (terpri))
  (let ((state (grid-to-state grid)))
    (loop for move in solution do
      (print-state state)
      (terpri)
      (setf state (make-move state move)))
    (print-state state)))

(defun filter (nodes visited)
  (cond
    ((or (null nodes) (null visited)) nodes)
    ((member (get-state (car nodes)) visited :test #'equal) (filter (cdr nodes) visited))
    (t (cons (car nodes) (filter (cdr nodes) visited)))))

(defun shuffle (data)
  (sort (copy-list data) (lambda (a b) (if (eq (random 2) 0) t nil))))

(defun successors (node)
  (let ((new-state nil)
        (new-nodes nil)
        (state (get-state node))
        (counter (+ (get-counter node) 1)))
    (loop for move in (shuffle move-symbols) do
      (setf new-state (make-move state move))
      (if new-state
        (setf new-nodes (append new-nodes `((,new-state ,move ,node ,(+ (manhattan new-state) (if use-counter counter 0)) ,counter))))))
    new-nodes))

(defun solve (grid)
  (let ((state (grid-to-state grid))
        (frontier nil)
        (visited nil)
        (node nil))
    (setf frontier `((,state ,nil ,nil ,(manhattan state) ,0)))
    (loop
      (if (null frontier)
        (return-from solve nil))
      (setf node (pop frontier))
      (cond (verbose
        (print-state (get-state node))
        (format t " ~a" (get-heuristic node))
        (terpri)))
      (if (equal (get-state node) goal)
        (return-from solve (remove nil (solution node))))
      (push (get-state node) visited)
      (setf frontier (append frontier (filter (successors node) visited)))
      (setf frontier (sort frontier (lambda (node1 node2)
        (< (get-heuristic node1) (get-heuristic node2))))))))
