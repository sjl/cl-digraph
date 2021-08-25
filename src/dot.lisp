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
(defparameter *vertex-shape* :circle
  "The shape to use for vertex nodes.  Must be a valid cl-dot :shape node attribute.")


(defmethod cl-dot:graph-object-node ((graph (eql 'digraph)) (vertex t))
  (make-instance 'cl-dot:node
    :attributes `(:label ,(format nil "~A" vertex) :shape ,*vertex-shape*)))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'digraph)) (vertex t))
  (successors *current-digraph* vertex))


(defun draw (digraph &key (filename "digraph.png") (format :png))
  "Draw `digraph` with cl-dot."
  (let ((*current-digraph* digraph))
    (cl-dot:dot-graph
      (cl-dot:generate-graph-from-roots 'digraph (find-dot-roots digraph))
      filename
      :format format))
  digraph)

