(in-package :digraph)



;;;; Utils --------------------------------------------------------------------
(defun make-hash-table-portably (&key (size 0) test hash-function)
  ;; Only try to pass :hash-function if we were given it, so we don't explode in
  ;; implementations that don't support it.
  ;;
  ;; Also, use `apply` instead of a simple `if` because we don't want spurious
  ;; compiler warnings...  This is ugly.
  (apply #'make-hash-table :test test :size size
         (if hash-function
           (list :hash-function hash-function)
           '())))


;;;; Data ---------------------------------------------------------------------
(defclass digraph ()
  ((nodes :initarg :nodes :accessor digraph-nodes)
   (test :initarg :test :accessor digraph-test)
   (hash-function :initarg :hash-function :accessor digraph-hash-function)))

(defun make-digraph (&key initial-vertices
                     (test #'eql)
                     (hash-function nil))
  (let ((digraph (make-instance 'digraph
                   :nodes (make-hash-table-portably
                            :test test
                            :size (length initial-vertices)
                            :hash-function hash-function)
                   :test test
                   :hash-function hash-function)))
    (mapc (curry #'insert-vertex digraph) initial-vertices)
    digraph))

(defmethod print-object ((d digraph) stream)
  (print-unreadable-object (d stream :type t :identity t)
    (format stream "~:S" (hash-table-keys (digraph-nodes d)))))


(defmacro pred (digraph object)
  `(car (gethash ,object (digraph-nodes ,digraph))))

(defmacro succ (digraph object)
  `(cdr (gethash ,object (digraph-nodes ,digraph))))


;;;; Basic API ----------------------------------------------------------------
(defun vertices (digraph)
  (hash-table-keys (digraph-nodes digraph)))

(defun edges (digraph)
  (map-edges #'cons digraph))


(defun predecessors (digraph object)
  (copy-list (pred digraph object)))

(defun successors (digraph object)
  (copy-list (succ digraph object)))

(defun neighbors (digraph object)
  (union (predecessors digraph object)
         (successors digraph object)
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

(defun insert-chain (digraph predecessor successor &rest later-successors)
  (insert-edge digraph predecessor successor)
  (when later-successors
    (apply #'insert-chain digraph successor later-successors)))


(defun remove-edge (digraph predecessor successor)
  (removef (succ digraph predecessor) successor :test (digraph-test digraph))
  (removef (pred digraph successor) predecessor :test (digraph-test digraph))
  (values))

(defun remove-vertex (digraph object)
  (let ((ps (pred digraph object))
        (ss (succ digraph object))
        (test (digraph-test digraph)))
    (loop :for p :in ps :do (removef (succ digraph p) object :test test))
    (loop :for s :in ss :do (removef (pred digraph s) object :test test)))
  (remhash object (digraph-nodes digraph))
  (values))


(defun degree (digraph object)
  (length (neighbors digraph object)))

(defun degree-in (digraph object)
  (length (pred digraph object)))

(defun degree-out (digraph object)
  (length (succ digraph object)))


(defun size (digraph)
  (hash-table-count (digraph-nodes digraph)))


;;;; Iteration ----------------------------------------------------------------
(defmacro do-vertices ((symbol digraph) &body body)
  `(loop :for ,symbol :being :the hash-keys :of (digraph-nodes ,digraph)
    :do (progn ,@body)))

(defmacro do-edges ((predecessor-symbol successor-symbol digraph) &body body)
  (with-gensyms (succs)
    `(loop
      :for ,predecessor-symbol :being :the hash-keys :of (digraph-nodes ,digraph)
      :using (hash-value (nil . ,succs))
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


;;;; Copying ------------------------------------------------------------------
(defun copy-digraph (digraph)
  ;; todo make this faster, but at least this works
  (let ((copy (make-digraph :test (digraph-test digraph)
                            :initial-vertices (vertices digraph))))
    (do-edges (p s digraph) (insert-edge digraph p s))
    copy))


;;;; Traversal ----------------------------------------------------------------
;;; Adapted from http://algorithms.wtf/

(defun mapc-depth-first (function digraph start-vertex)
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
  (let ((result nil))
    (mapc-depth-first (lambda (v) (push (funcall function v) result))
                      digraph start-vertex)
    (nreverse result)))

(defun map-breadth-first (function digraph start-vertex)
  (let ((result nil))
    (mapc-breadth-first (lambda (v) (push (funcall function v) result))
                        digraph start-vertex)
    (nreverse result)))


(defun roots (digraph)
  (remove-if-not (lambda (v) (null (pred digraph v)))
                 (vertices digraph)))

(defun leafs (digraph)
  (remove-if-not (lambda (v) (null (succ digraph v)))
                 (vertices digraph)))


(defun mapc-topological (function digraph)
  (let ((status (make-hash-table-portably
                  :test (digraph-test digraph)
                  :hash-function (digraph-hash-function digraph))))
    (labels
        ((visit (vertex)
           (ecase (gethash vertex status :new)
             (:active
              (error "Cycle detected during topological map involving vertex ~S"
                     vertex))
             (:new (recur vertex))
             (:done nil)))
         (recur (vertex)
           (setf (gethash vertex status) :active)
           (mapc #'visit (succ digraph vertex))
           (setf (gethash vertex status) :done)
           (funcall function vertex)))
      (mapc #'visit (roots digraph))))
  nil)

(defun map-topological (function digraph)
  (let ((result nil))
    (mapc-topological (lambda (v) (push (funcall function v) result)) digraph)
    (nreverse result)))


;;;; Scratch ------------------------------------------------------------------
(defun make-test-digraph ()
  ;; a ---->  middle  ----> z         ORPHAN
  ;; ^          ^  ^
  ;; |          |  |
  ;; B ---------+  |
  ;; |             |          +-------------------+
  ;; v             |          |                   v
  ;; c --------> dogs        FOO ----> bar ----> baz
  ;; ^                        |
  ;; |                        |
  ;; +------------------------+
  (let ((g (make-digraph
             :initial-vertices
             '(a b c dogs middle z orphan foo bar baz))))
    (insert-edge g 'a 'middle)
    (insert-edge g 'b 'middle)
    (insert-edge g 'b 'a)
    (insert-edge g 'middle 'z)
    ; (insert-edge g 'z 'z)
    (insert-edge g 'b 'c)
    (insert-edge g 'c 'dogs)
    (insert-edge g 'dogs 'middle)
    ; (insert-edge g 'dogs 'c)
    (insert-edge g 'foo 'baz)
    (insert-edge g 'foo 'bar)
    (insert-edge g 'bar 'baz)
    g))


#+scratch
(progn
  (defparameter *d* (make-test-digraph))
  (setf cl-dot:*dot-path* "/usr/local/bin/dot")
  (digraph.dot:draw *d*))
