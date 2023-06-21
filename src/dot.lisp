(in-package :digraph.dot)


(defun find-dot-roots (digraph)
  (let ((nodes (vertices digraph))
        (roots nil)
        (test (digraph::digraph-test digraph)))
    (labels ((descendents (vertex)
               (map-depth-first #'identity digraph vertex))
             (prune (vertex)
               (setf nodes (set-difference nodes (descendents vertex)
                                           :test test)))
             (mark-root-and-prune (vertex)
               (push vertex roots)
               (prune vertex)))
      (mapc #'mark-root-and-prune (digraph::roots digraph))
      (loop :while nodes :do (mark-root-and-prune (pop nodes))))
    roots))


(defparameter *current-digraph* nil)
(defparameter *shape* nil)
(defparameter *label* #'princ-to-string)


(defmethod cl-dot:graph-object-node ((graph (eql 'digraph)) (vertex t))
  (make-instance 'cl-dot:node
    :attributes `(:label ,(funcall *label* vertex) :shape ,*shape*)))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'digraph)) (vertex t))
  (successors *current-digraph* vertex))


(defun draw (digraph &key (filename "digraph.png") (format :png)
                     (shape :circle) (label #'princ-to-string))
  "Draw `digraph` with cl-dot."
  (let ((*current-digraph* digraph)
        (*shape* shape)
        (*label* label))
    (cl-dot:dot-graph
      (cl-dot:generate-graph-from-roots 'digraph (find-dot-roots digraph))
      filename
      :format format))
  digraph)

