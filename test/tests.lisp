(in-package :digraph.test)


;;;; Utils --------------------------------------------------------------------
(defmacro define-test (name &body body)
  `(test ,(symb 'test/ name)
    (let ((*package* ,*package*))
      ,@body)))


(defun same (a b)
  (null (set-exclusive-or a b :test #'equal)))

(defun run-tests ()
  (1am:run))


;;;; Tests --------------------------------------------------------------------
(define-test make-digraph
  (let ((g (make-digraph)))
    (is (zerop (count-vertices g)))
    (is (same () (vertices g)))
    (is (same () (edges g)))
    (is (emptyp g)))
  (let ((g (make-digraph :initial-vertices '(a b c))))
    (is (= 3 (count-vertices g)))
    (is (same '(a b c) (vertices g)))
    (is (same () (edges g)))
    (is (not (emptyp g))))
  (let ((g (make-digraph :initial-vertices '(a b a c a a))))
    (is (= 3 (count-vertices g)))
    (is (same '(a b c) (vertices g)))
    (is (same () (edges g)))
    (is (not (emptyp g)))))

(define-test copy-digraph
  (let ((g (make-digraph :initial-vertices '(a b c)))
        (h nil))
    (insert-edge g 'a 'b)
    (is (same '(a b c) (vertices g)))
    (is (same '((a . b)) (edges g)))
    (setf h (copy-digraph g))
    (is (same '(a b c) (vertices h)))
    (is (same '((a . b)) (edges h)))
    (remove-edge h 'a 'b)
    (remove-vertex h 'c)
    (is (same '(a b) (vertices h)))
    (is (same '() (edges h)))
    ;; make sure the original didn't change
    (is (same '(a b c) (vertices g)))
    (is (same '((a . b)) (edges g)))))


(define-test roots-and-leafs
  (let ((g (make-digraph)))
    (is (same () (roots g)))
    (is (same () (leafs g)))
    (insert-vertex g 'a)
    (insert-vertex g 'b)
    (is (same '(a b) (roots g)))
    (is (same '(a b) (leafs g)))
    (insert-edge g 'a 'b)
    (is (same '(a) (roots g)))
    (is (same '(b) (leafs g)))
    (insert-edge g 'b 'a)
    (is (same () (roots g)))
    (is (same () (leafs g)))))


(define-test insert-vertex
  (let ((g (make-digraph)))
    (is (= 0 (count-vertices g)))
    (is (same '() (vertices g)))

    (insert-vertex g 'a)
    (is (= 1 (count-vertices g)))
    (is (same '(a) (vertices g)))

    (insert-vertex g 'b)
    (is (= 2 (count-vertices g)))
    (is (same '(a b) (vertices g)))

    (insert-vertex g 'a) ; dup
    (is (= 2 (count-vertices g)))
    (is (same '(a b) (vertices g)))))

(define-test insert-edge
  (let ((g (make-digraph :initial-vertices '(a b c))))
    (insert-edge g 'a 'b)
    (is (same '((a . b))
              (edges g)))

    (insert-edge g 'b 'c)
    (is (same '((a . b) (b . c))
              (edges g)))))


(define-test missing-vertices
  (let ((g (make-digraph :initial-vertices '(a b))))
    (insert-edge g 'a 'b)
    (signals missing-vertex (insert-edge g 'x 'y))
    (signals missing-predecessor (insert-edge g 'x 'a))
    (signals missing-successor (insert-edge g 'a 'x))))


(define-test remove-vertex
  (let ((g (make-digraph :initial-vertices '(a b c))))
    (insert-edge g 'a 'b)
    (insert-edge g 'b 'c)
    (is (same '(a b c) (vertices g)))
    (is (same '((a . b) (b . c)) (edges g)))

    (remove-vertex g 'c)
    (is (same '(a b) (vertices g)))
    (is (same '((a . b)) (edges g)))

    (remove-vertex g 'c)
    (is (same '(a b) (vertices g)))
    (is (same '((a . b)) (edges g)))

    (remove-vertex g 'b)
    (is (same '(a) (vertices g)))
    (is (same '() (edges g)))))

(define-test remove-edge
  (let ((g (make-digraph :initial-vertices '(a b c))))
    (insert-edge g 'a 'b)
    (insert-edge g 'b 'c)
    (is (same '((a . b) (b . c)) (edges g)))

    (remove-edge g 'a 'b)
    (is (same '((b . c)) (edges g)))

    (remove-edge g 'a 'b)
    (is (same '((b . c)) (edges g)))

    (remove-edge g 'b 'c)
    (is (same '() (edges g)))))


(defun make-simple-digraph ()
  ;; a ---->  middle  ----> z <-+     orphan
  ;; ^          ^           |   |
  ;; |          |           +---+
  ;; b ---------+
  (let ((g (make-digraph :initial-vertices '(a b middle z orphan))))
    (insert-edge g 'a 'middle)
    (insert-edge g 'b 'middle)
    (insert-edge g 'b 'a)
    (insert-edge g 'middle 'z)
    (insert-edge g 'z 'z)
    g))


(define-test neighbors
  (let ((g (make-simple-digraph)))
    (is (same '(b middle) (neighbors g 'a)))
    (is (same '(a middle) (neighbors g 'b)))
    (is (same '(a b z) (neighbors g 'middle)))
    (is (same '(middle z) (neighbors g 'z)))
    (is (same '() (neighbors g 'orphan)))))

(define-test predecessors
  (let ((g (make-simple-digraph)))
    (is (same '(b) (predecessors g 'a)))
    (is (same '() (predecessors g 'b)))
    (is (same '(a b) (predecessors g 'middle)))
    (is (same '(middle z) (predecessors g 'z)))
    (is (same '() (predecessors g 'orphan)))))

(define-test successors
  (let ((g (make-simple-digraph)))
    (is (same '(middle) (successors g 'a)))
    (is (same '(a middle) (successors g 'b)))
    (is (same '(z) (successors g 'middle)))
    (is (same '(z) (successors g 'z)))
    (is (same '() (successors g 'orphan)))))


(define-test contains-vertex-p ()
  (let ((g (make-digraph :initial-vertices '(a b c))))
    (is (contains-vertex-p g 'a))
    (is (contains-vertex-p g 'b))
    (is (contains-vertex-p g 'c))

    (is (null (contains-vertex-p g 'd)))
    (insert-vertex g 'd)
    (is (contains-vertex-p g 'd))
    (remove-vertex g 'd)
    (is (null (contains-vertex-p g 'd)))))

(define-test contains-edge-p ()
  (let ((g (make-digraph :initial-vertices '(a b c))))
    (is (null (contains-edge-p g 'a 'b)))
    (is (null (contains-edge-p g 'c 'c)))

    (insert-edge g 'a 'b)
    (is (contains-edge-p g 'a 'b))
    (is (null (contains-edge-p g 'c 'c)))

    (insert-edge g 'c 'c)
    (is (contains-edge-p g 'a 'b))
    (is (contains-edge-p g 'c 'c))

    (remove-edge g 'a 'b)
    (is (null (contains-edge-p g 'a 'b)))
    (is (contains-edge-p g 'c 'c))

    (remove-edge g 'c 'c)
    (is (null (contains-edge-p g 'a 'b)))
    (is (null (contains-edge-p g 'c 'c)))))


(define-test degree
  (let ((g (make-simple-digraph)))
    (is (= 2 (degree g 'a)))
    (is (= 2 (degree g 'b)))
    (is (= 3 (degree g 'middle)))
    (is (= 2 (degree g 'z)))
    (is (= 0 (degree g 'orphan)))))

(define-test degree-in
  (let ((g (make-simple-digraph)))
    (is (= 1 (degree-in g 'a)))
    (is (= 0 (degree-in g 'b)))
    (is (= 2 (degree-in g 'middle)))
    (is (= 2 (degree-in g 'z)))
    (is (= 0 (degree g 'orphan)))))

(define-test degree-out
  (let ((g (make-simple-digraph)))
    (is (= 1 (degree-out g 'a)))
    (is (= 2 (degree-out g 'b)))
    (is (= 1 (degree-out g 'middle)))
    (is (= 1 (degree-out g 'z)))
    (is (= 0 (degree g 'orphan)))))


(define-test reachablep
  (let ((g (make-simple-digraph)))
    (is (reachablep g 'orphan 'orphan))
    (is (reachablep g 'b 'a))
    (is (reachablep g 'b 'z))
    (is (not (reachablep g 'a 'b)))
    (is (not (reachablep g 'z 'orphan)))))


(define-test abitrary-vertex
  (let ((g (make-simple-digraph)))
    (is (member (arbitrary-vertex g) '(a b middle z orphan)))
    (remove-vertex g 'b)
    (is (member (arbitrary-vertex g) '(a middle z orphan)))
    (remove-vertex g 'middle)
    (is (member (arbitrary-vertex g) '(a z orphan)))
    (remove-vertex g 'z)
    (is (member (arbitrary-vertex g) '(a orphan)))
    (remove-vertex g 'a)
    (is (member (arbitrary-vertex g) '(orphan)))
    (remove-vertex g 'orphan)
    (is (null (arbitrary-vertex g)))
    (insert-vertex g 'new)
    (is (member (arbitrary-vertex g) '(new)))))


(defmacro has-topo-error (graph involving)
  (alexandria:once-only (graph involving)
    `(handler-case (topological-sort ,graph)
       (topological-sort-cycle (c)
         (is (member (vertex-involved c) ,involving))))))

(define-test topological-sort-cycle-detection
  ;; a    b    c    d
  (let ((g (make-digraph :initial-vertices '(a b c d))))
    (is (= 4 (length (topological-sort g))))

    ;; a--->b    c    d
    (insert-edge g 'a 'b)
    (is (= 4 (length (topological-sort g))))

    ;; a--->b--->c    d
    (insert-edge g 'b 'c)
    (is (= 4 (length (topological-sort g))))

    ;; a--->b--->c    d
    ;; ^         |
    ;; |_________|
    (insert-edge g 'c 'a)
    (has-topo-error g '(a b c))
    (remove-edge g 'c 'a)

    ;; a--->b--->c    d--+
    ;;                ^  |
    ;;                |  |
    ;;                +--+
    (insert-edge g 'd 'd)
    (has-topo-error g '(d))
    (remove-edge g 'd 'd)

    ;; a--->b--->c--->d
    (insert-edge g 'c 'd)
    (is (= 4 (length (topological-sort g))))

    ;; a--->b--->c--->d--+
    ;;      ^            |
    ;;      |            |
    ;;      +------------+
    (insert-edge g 'd 'b)
    (has-topo-error g '(b c d))
    (remove-edge g 'd 'b)))


(define-test convenience-builders
  ;; a --> b --> c
  ;;       |
  ;;       v     e --> f
  ;;       d
  (let ((g (build-from-roots '(a e) (lambda (v) (ecase v
                                                  (a '(b))
                                                  (b '(c d))
                                                  (c '())
                                                  (d '())
                                                  (e '(f))
                                                  (f '()))))))
    (is (same '(a b c d e f) (vertices g)))
    (is (same '((a . b)
                (b . c) (b . d)
                (e . f))
              (edges g))))
  (let ((g (build-from-leafs '(c d f) (lambda (v) (ecase v
                                                    (a '())
                                                    (b '(a))
                                                    (c '(b))
                                                    (d '(b))
                                                    (e '())
                                                    (f '(e)))))))
    (is (same '(a b c d e f) (vertices g)))
    (is (same '((a . b)
                (b . c) (b . d)
                (e . f))
              (edges g))))
  (let ((g (build-from-roots '() (lambda (v) (ecase v)))))
    (is (same '() (vertices g)))
    (is (same '() (edges g))))
  (let ((g (build-from-leafs '() (lambda (v) (ecase v)))))
    (is (same '() (vertices g)))
    (is (same '() (edges g)))))
