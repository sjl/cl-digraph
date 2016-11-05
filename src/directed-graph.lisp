(in-package :directed-graph)


;;;; Data ---------------------------------------------------------------------
(defclass digraph ()
  ((nodes :initarg :nodes :accessor digraph-nodes)
   (test :initarg :test :accessor digraph-test)))

(defun make-digraph (&key (test #'eql) initial-vertices)
  (let ((digraph (make-instance 'digraph
                   :nodes (make-hash-table :test test
                                           :size (length initial-vertices))
                   :test test)))
    (mapc (curry #'insert-vertex digraph) initial-vertices)
    digraph))

(defmethod print-object ((d digraph) stream)
  (print-unreadable-object (d stream :type t :identity t)
    (format stream "~:S" (hash-table-keys (digraph-nodes d)))))


(defmacro pred (digraph object)
  `(car (gethash ,object (digraph-nodes ,digraph))))

(defmacro succ (digraph object)
  `(cdr (gethash ,object (digraph-nodes ,digraph))))


(defun copy-digraph (digraph)
  ;; todo make this faster, but at least this works
  (let ((copy (make-digraph :test (digraph-test digraph)
                            :initial-vertices (vertices digraph))))
    (do-edges (p s digraph) (insert-edge digraph p s))
    copy))


;;;; Basic API ----------------------------------------------------------------
(defun vertices (digraph)
  (hash-table-keys (digraph-nodes digraph)))

(defun predecessors (digraph object)
  (pred digraph object))

(defun successors (digraph object)
  (succ digraph object))

(defun neighbors (digraph object)
  (union (predecessors digraph object)
         (predecessors digraph object)
         :test (digraph-test digraph)))


(defun contains-vertex-p (digraph object)
  (nth-value 1 (gethash object (digraph-nodes digraph))))

(defun contains-edge-p (digraph predecessor successor)
  (ensure-boolean (member successor (succ digraph predecessor)
                          :test (digraph-test digraph))))


(defun insert-vertex (digraph object)
  (nth-value 1 (ensure-gethash object (digraph-nodes digraph)
                               (cons nil nil))))

(defun insert-edge (digraph predecessor successor)
  (assert (contains-vertex-p digraph predecessor) (predecessor)
      "Cannot add edge with predecessor ~S because it is not in the graph"
      predecessor)
  (assert (contains-vertex-p digraph successor) (successor)
      "Cannot add edge with successor ~S because it is not in the graph"
      successor)
  (pushnew predecessor (pred digraph successor) :test (digraph-test digraph))
  (pushnew successor (succ digraph predecessor) :test (digraph-test digraph))
  (values))


(defun remove-edge (digraph predecessor successor)
  (removef (succ digraph predecessor) successor :test (digraph-test digraph))
  (removef (pred digraph successor) predecessor :test (digraph-test digraph))
  (values))

(defun remove-vertex (digraph object)
  (let ((ps (predecessors digraph object))
        (ss (successors digraph object))
        (test (digraph-test digraph)))
    (loop :for p :in ps :do (removef (succ digraph p) object :test test))
    (loop :for s :in ss :do (removef (pred digraph s) object :test test)))
  (remhash object (digraph-nodes digraph))
  (values))


(defun degree (digraph object)
  (length (neighbors digraph object)))

(defun degree-in (digraph object)
  (length (predecessors digraph object)))

(defun degree-out (digraph object)
  (length (successors digraph object)))


(defun size (digraph)
  (hash-table-count (digraph-nodes digraph)))


;;;; Iteration ----------------------------------------------------------------
(defmacro do-vertices ((symbol digraph) &body body)
  `(loop :for ,symbol :being :the hash-keys :of (digraph-nodes ,digraph)
    :do (progn ,@body)))

(defmacro do-edges ((predecessor-symbol successor-symbol digraph) &body body)
  (with-gensyms (preds succs)
    `(loop
      :for ,predecessor-symbol :being :the hash-keys :of (digraph-nodes ,digraph)
      :using (hash-value (,preds . ,succs))
      :do (loop :for ,successor-symbol :in ,succs ; i miss u, iterate
                :do (progn ,@body)))))


(defun mapc-vertices (function digraph)
  (do-vertices (v digraph) (funcall function v)))

(defun mapc-edges (function digraph)
  (do-edges (p s digraph) (funcall function p s)))


(defun map-vertices (function digraph)
  (let ((result nil))
    (do-vertices (v digraph) (push (funcall function v) result))
    result))

(defun map-edges (function digraph)
  (let ((result nil))
    (do-edges (p s digraph) (push (funcall function p s) result))
    result))


(defun dump (digraph)
  (format t "Digraph :TEST ~A~%:CONTENTS " (digraph-test digraph))
  (finish-output)
  (ql:quickload :losh :silent t)
  (funcall (intern "PRINT-HASH-TABLE" (find-package :losh))
           (digraph-nodes digraph))
  (values))


;;;; Scratch ------------------------------------------------------------------
(defparameter *d* (make-digraph))


(insert-vertex *d* 'a)
(insert-vertex *d* 'b)
(insert-vertex *d* 'c)

(insert-edge *d* 'b 'c)

(remove-edge *d* 'a 'a)
(remove-vertex *d* 'a)
(dump *d*)
