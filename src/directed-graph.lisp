(in-package :digraph)

;;;; Utils --------------------------------------------------------------------
(defun make-hash-table-portably (&key (size 0) test hash-function)
  (apply #'make-hash-table
    :test test
    :size size
    ;; Don't explode if the implementation doesn't support :hash-function.
    :allow-other-keys t
    (when hash-function
      (list :hash-function hash-function))))


;;;; Errors -------------------------------------------------------------------
(defgeneric vertex-involved (condition)
  (:documentation "Retrieve the vertex involved in the condition."))

(define-condition digraph-error (error) ()
  (:documentation "Base condition for digraph-related errors."))

(define-condition topological-sort-cycle (digraph-error)
  ((vertex-involved% :initarg :vertex-involved :reader vertex-involved))
  (:report
   (lambda (c stream)
     (format stream "Cycle detected during topological sort involving vertex ~S."
             (vertex-involved c))))
  (:documentation
    "An error signaled when topologically sorting a graph that contains a cycle.

   `vertex-involved` can be used to retrieve one of the vertices involved in a
   cycle.  Which vertex in the cycle is chosen is arbitrary."))

(define-condition missing-vertex (digraph-error)
  ((vertex-involved% :initarg :vertex-involved :reader vertex-involved))
  (:documentation "Base condition for errors signaled when inserting an edge with a vertex missing."))

(define-condition missing-predecessor (missing-vertex) ()
  (:report
   (lambda (c stream)
     (format stream
             "Cannot add edge with predecessor ~S because it is not in the graph."
             (vertex-involved c))))
  (:documentation
    "An error signaled when trying to insert an edge whose predecessor is not in the graph.

   `vertex-involved` can be used to retrieve the offending predecessor."))

(define-condition missing-successor (missing-vertex) ()
  (:report
   (lambda (c stream)
     (format stream
             "Cannot add edge with successor ~S because it is not in the graph."
             (vertex-involved c))))
  (:documentation
    "An error signaled when trying to insert an edge whose successor is not in the graph.

   `vertex-involved` can be used to retrieve the offending successor."))


;;;; Data ---------------------------------------------------------------------
(defclass digraph ()
  ((nodes :initarg :nodes :reader digraph-nodes)
   (test :initarg :test :reader digraph-test)
   (hash-function :initarg :hash-function :reader digraph-hash-function))
  (:documentation "A directed graph.  Use `make-digraph` to create one."))

(defun make-digraph (&key initial-vertices
                     (test #'eql)
                     (hash-function nil))
  "Create and return a new digraph.

  `initial-vertices` can be a sequence of vertices to add to the graph.

  `test` should be one of the hash table equality predicates.

  If your Lisp implementation supports the `:hash-function` argument for
  creating hash tables with custom predicates, you can specify one with
  `hash-function`.

  "
  (let ((digraph (make-instance 'digraph
                   :nodes (make-hash-table-portably
                            :test test
                            :size (length initial-vertices)
                            :hash-function hash-function)
                   :test test
                   :hash-function hash-function)))
    (map nil (curry #'insert-vertex digraph) initial-vertices)
    digraph))

(defmethod print-object ((d digraph) stream)
  (print-unreadable-object (d stream :type t :identity t)
    (format stream "~:S" (hash-table-keys (digraph-nodes d)))))


(defmacro pred (digraph object)
  `(car (gethash ,object (digraph-nodes ,digraph))))

(defmacro succ (digraph object)
  `(cdr (gethash ,object (digraph-nodes ,digraph))))


(defmacro do-vertices ((symbol digraph) &body body)
  `(loop :for ,symbol :being :the hash-keys :of (digraph-nodes ,digraph)
         :do (progn ,@body)))

(defmacro do-edges ((predecessor-symbol successor-symbol digraph) &body body)
  (with-gensyms (succs)
    `(loop
       :for ,predecessor-symbol :being :the hash-keys :of (digraph-nodes ,digraph)
       :using (hash-value (nil . ,succs))
       :do (loop :for ,successor-symbol :in ,succs :do (progn ,@body)))))


;;;; Basic API ----------------------------------------------------------------
(defun emptyp (digraph)
  "Return `t` if `digraph` has no vertices or edges, `nil` otherwise."
  (zerop (hash-table-count (digraph-nodes digraph))))


(defun vertices (digraph)
  "Return a fresh list of the vertices of `digraph`."
  (hash-table-keys (digraph-nodes digraph)))

(defun edges (digraph)
  "Return a fresh list of the edges of `digraph`.

  Each edge will be a cons of the form `(predecessor . successor)`.

  "
  (map-edges #'cons digraph))


(defun predecessors (digraph vertex)
  "Return a fresh list of the predecessors of `vertex`."
  (copy-list (pred digraph vertex)))

(defun successors (digraph vertex)
  "Return a fresh list of the successors of `vertex`."
  (copy-list (succ digraph vertex)))

(defun neighbors (digraph vertex)
  "Return a fresh list of the neighbors of `vertex`."
  (union (predecessors digraph vertex)
         (successors digraph vertex)
         :test (digraph-test digraph)))


(defun contains-vertex-p (digraph vertex)
  "Return whether the graph contains `vertex`."
  (nth-value 1 (gethash vertex (digraph-nodes digraph))))

(defun contains-edge-p (digraph predecessor successor)
  "Return whether the graph contains an edge from `predecessor` to `successor`."
  (ensure-boolean (member successor (succ digraph predecessor)
                          :test (digraph-test digraph))))


(defun insert-vertex (digraph vertex)
  "Insert `vertex` into the graph if it is not already a member.

  Returns `t` if the vertex was already in the graph, or `nil` if it was
  inserted.

  "
  (nth-value 1 (ensure-gethash vertex (digraph-nodes digraph)
                               (cons nil nil))))

(defun insert-edge (digraph predecessor successor)
  "Insert an edge from `predecessor` to `successor` if not already present.

  Returns `t` if the edge was already in the graph, or `nil` if it was
  inserted.

  The `predecessor` and `successor` vertices must already exist in the graph.
  If `predecessor` is not in the graph a `missing-predecessor` error will be
  signaled.  Otherwise, if `successor` is not in the graph, a `missing-successor`
  error will be signaled.

  "
  (unless (contains-vertex-p digraph predecessor)
    (error 'missing-predecessor :vertex-involved predecessor))
  (unless (contains-vertex-p digraph successor)
    (error 'missing-successor :vertex-involved successor))
  (prog1
      (contains-edge-p digraph predecessor successor)
    (pushnew predecessor (pred digraph successor) :test (digraph-test digraph))
    (pushnew successor (succ digraph predecessor) :test (digraph-test digraph))))

(defun insert-chain (digraph predecessor successor &rest later-successors)
  "Insert edges between a series of vertices.

  Give a series of vertices `V0 V1 ... Vn`, edges between each will be inserted
  if not already present:

    V0 -> V1 -> ... -> Vn

  All vertices must exist in the graph already.

  Returns `nil`.

  "
  (insert-edge digraph predecessor successor)
  (when later-successors
    (apply #'insert-chain digraph successor later-successors)))


(defun arbitrary-vertex (digraph)
  "Return an arbitrary vertex of `digraph` and `t`.

  If the digraph is empty, `(values nil nil)` will be returned instead.

  "
  (do-vertices (vertex digraph)
    (return-from arbitrary-vertex (values vertex t)))
  (values nil nil))


(defun remove-edge (digraph predecessor successor)
  "Remove an edge from `predecessor` to `successor` if present.

  Returns `t` if there was such an edge, or `nil` if not.

  "
  (if (contains-edge-p digraph predecessor successor)
    (progn
      (removef (succ digraph predecessor) successor :test (digraph-test digraph))
      (removef (pred digraph successor) predecessor :test (digraph-test digraph))
      t)
    nil))

(defun remove-vertex (digraph vertex)
  "Remove `vertex` from the graph if present.

  If there are any edges to/from `vertex` they will be automatically removed.

  Returns `t` if there was such a vertex, or `nil` if not.

  "
  (if (contains-vertex-p digraph vertex)
    (let ((ps (pred digraph vertex))
          (ss (succ digraph vertex))
          (test (digraph-test digraph)))
      (loop :for p :in ps :do (removef (succ digraph p) vertex :test test))
      (loop :for s :in ss :do (removef (pred digraph s) vertex :test test))
      (remhash vertex (digraph-nodes digraph))
      t)
    nil))


(defun degree (digraph vertex)
  "Return the number of neighbors of `vertex`."
  (length (neighbors digraph vertex)))

(defun degree-in (digraph vertex)
  "Return the number of predecessors of `vertex`."
  (length (pred digraph vertex)))

(defun degree-out (digraph vertex)
  "Return the number of successors of `vertex`."
  (length (succ digraph vertex)))


(defun count-vertices (digraph)
  "Return the number of vertices in `digraph`."
  (hash-table-count (digraph-nodes digraph)))

(defun count-edges (digraph)
  "Return the number of edges in `digraph`."
  (let ((result 0))
    (do-edges (nil nil digraph) (incf result))
    result))


(defun rootp (digraph vertex)
  "Return whether `vertex` is a root vertex in `digraph`."
  (null (pred digraph vertex)))

(defun leafp (digraph vertex)
  "Return whether `vertex` is a leaf vertex in `digraph`."
  (null (succ digraph vertex)))


;;;; Build --------------------------------------------------------------------
(defun build-from-roots (roots successor-function &key (test #'eql) (hash-function nil))
  "Build a fresh `digraph` starting from `roots` using `successor-function`.

  This is a convenience function to build a digraph object if you have some
  roots and a function that can find their children.

  `roots` must be a list.

  `successor-function` must be a function that takes a vertex and returns a list
  of its successors.

  "
  (let ((result (make-digraph :test test :hash-function hash-function)))
    (labels ((recur (node)
               (insert-vertex result node)
               (dolist (succ (funcall successor-function node))
                 (insert-vertex result succ)
                 (insert-edge result node succ)
                 (recur succ))))
      (map nil #'recur roots))
    result))

(defun build-from-leafs (leafs predecessor-function &key (test #'eql) (hash-function nil))
  "Build a fresh `digraph` starting from `leafs` using `predecessor-function`.

  This is a convenience function to build a digraph object if you have some
  leafs and a function that can find their parents.

  `leafs` must be a list.

  `predecessor-function` must be a function that takes a vertex and returns
  a list of its predecessors.

  "
  (let ((result (make-digraph :test test :hash-function hash-function)))
    (labels ((recur (node)
               (insert-vertex result node)
               (dolist (pred (funcall predecessor-function node))
                 (insert-vertex result pred)
                 (insert-edge result pred node)
                 (recur pred))))
      (map nil #'recur leafs))
    result))


;;;; Iteration ----------------------------------------------------------------
(defun mapc-vertices (function digraph)
  "Call `function` on each vertex in `digraph`.

  The order in which the vertices are processed is unspecified.

  Returns `nil`.

  "
  (do-vertices (v digraph) (funcall function v)))

(defun mapc-edges (function digraph)
  "Call `function` on each edge in `digraph`.

  For each edge, `function` will be called once with two arguments:

    (function predecessor successor)

  The order in which the edges are processed is unspecified.

  Returns `nil`.

  "
  (do-edges (p s digraph) (funcall function p s)))


(defun map-vertices (function digraph)
  "Return a fresh list with the results of calling `function` on each vertex.

  The order of the resulting list is unspecified.

  "
  (let ((result nil))
    (do-vertices (v digraph) (push (funcall function v) result))
    result))

(defun map-edges (function digraph)
  "Return a fresh list with the results of calling `function` on each edge.

  For each edge, `function` will be called once with two arguments:

    (function predecessor successor)

  The order of the resulting list is unspecified.

  "
  (let ((result nil))
    (do-edges (p s digraph) (push (funcall function p s) result))
    result))


(defun find-vertex-if (function digraph)
  (do-vertices (v digraph)
    (when (funcall function v)
      (return-from find-vertex-if v))))


;;;; Copying ------------------------------------------------------------------
(defun copy-digraph (digraph)
  "Create a fresh copy of `digraph`.

  The vertex objects themselves are not copied, but everything else is.

  "
  ;; todo make this faster, but at least this works
  (let ((copy (make-digraph :test (digraph-test digraph)
                            :hash-function (digraph-hash-function digraph)
                            :initial-vertices (vertices digraph))))
    (do-edges (p s digraph) (insert-edge copy p s))
    copy))


;;;; Traversal ----------------------------------------------------------------
;;; Adapted from http://algorithms.wtf/

(defun mapc-depth-first (function digraph start-vertex)
  "Apply `function` to the vertices of a depth-first traversal of `digraph`.

  Returns `nil`.

  Vertices are processed in depth-first order, beginning at `start-vertex`.

  Cycles in the graph will not be traversed into.

  "
  (let ((seen nil))
    (labels ((recur (vertex)
               (when (not (member vertex seen :test (digraph-test digraph)))
                 (push vertex seen)
                 (funcall function vertex)
                 (mapcar #'recur (succ digraph vertex)))))
      (when (contains-vertex-p digraph start-vertex)
        (recur start-vertex))))
  nil)

(defun mapc-breadth-first (function digraph start-vertex)
  "Apply `function` to the vertices of a breadth-first traversal of `digraph`.

  Returns `nil`.

  Vertices are processed in breadth-first order, beginning at `start-vertex`.

  Cycles in the graph will not be traversed into.

  "
  (let ((seen nil)
        (remaining nil))
    (labels ((recur (vertex)
               (when (not (member vertex seen :test (digraph-test digraph)))
                 (push vertex seen)
                 (funcall function vertex)
                 ;;; todo maybe use jpl queues here...
                 (appendf remaining (succ digraph vertex)))
               (when remaining
                 (recur (pop remaining)))))
      (when (contains-vertex-p digraph start-vertex)
        (recur start-vertex))))
  nil)


(defun map-depth-first (function digraph start-vertex)
  "Apply `function` to the vertices of a breadth-first traversal of `digraph`.

  Returns a fresh list with the results.

  Vertices are processed in depth-first order, beginning at `start-vertex`, and
  the resulting list has this order as well.

  Cycles in the graph will not be traversed into.

  "
  (let ((result nil))
    (mapc-depth-first (lambda (v) (push (funcall function v) result))
                      digraph start-vertex)
    (nreverse result)))

(defun map-breadth-first (function digraph start-vertex)
  "Apply `function` to the vertices of a breadth-first traversal of `digraph`.

  Returns a fresh list with the results.

  Vertices are processed in breadth-first order, beginning at `start-vertex`,
  and the resulting list has this order as well.

  Cycles in the graph will not be traversed into.

  "
  (let ((result nil))
    (mapc-breadth-first (lambda (v) (push (funcall function v) result))
                        digraph start-vertex)
    (nreverse result)))


(defun roots (digraph)
  "Return all root vertices in `digraph`.

  This is currently O(vertices).

  A root is a vertex with no incoming edges (i.e. in-degree 0).

  "
  (remove-if-not (curry #'rootp digraph)
                 (vertices digraph)))

(defun leafs (digraph)
  "Return all leaf vertices in `digraph`.

  This is currently O(vertices).

  A root is a vertex with no outgoing edges (i.e. out-degree 0).

  "
  (remove-if-not (curry #'leafp digraph)
                 (vertices digraph)))


(declaim (inline topological-sort%))
(defun topological-sort% (function digraph)
  (let ((status (make-hash-table-portably
                  :test (digraph-test digraph)
                  :hash-function (digraph-hash-function digraph))))
    (labels
        ((visit (vertex)
           (ecase (gethash vertex status :new)
             (:active (error 'topological-sort-cycle :vertex-involved vertex))
             (:new (recur vertex))
             (:done nil)))
         (recur (vertex)
           (setf (gethash vertex status) :active)
           (mapc #'visit (succ digraph vertex))
           (setf (gethash vertex status) :done)
           (funcall function vertex)))
      (mapc #'visit (roots digraph)))
    status))

(defun topological-sort (digraph)
  "Return a fresh list of the vertices of `digraph` in topological order.

  Edges are treated as meaning \"depends on\", so an edge `A --> B` means \"A
  depends on B\" and that B must come before A in the resulting list.  Aside
  from this restriction, the order of the resulting list is arbitrary.

  The order in which the vertices are processed is unspecified.

  A `topological-sort-cycle` error will be signaled if the graph contains
  a cycle.

  "
  (let* ((result nil)
         (seen (topological-sort% (lambda (v) (push v result)) digraph)))
    ;; Make sure there are no rootless cycles.
    (if (= (hash-table-count seen) (count-vertices digraph))
      (nreverse result)
      (error 'topological-sort-cycle
             :vertex-involved (find-vertex-if
                                (lambda (v) (not (gethash v seen)))
                                digraph)))))


(defun reachablep (digraph start target &key (strategy :breadth-first))
  "Return `t` if it is possible to reach `target` from `start`, otherwise `nil`.

  All vertices are reachable from themselves.

  Otherwise a `target` is reachable from `start` if a directed path exists from
  the start to the target.

  `strategy` will be used to determine how to traverse the graph when searching
  for a path, and can be one of `:breadth-first` or `:depth-first`.

  "
  (let* ((traverse (ccase strategy
                     (:breadth-first #'mapc-breadth-first)
                     (:depth-first #'mapc-depth-first)))
         (test (digraph-test digraph))
         (check (lambda (vertex)
                  (when (funcall test vertex target)
                    (return-from reachablep t)))))
    (funcall traverse check digraph start)
    nil))

