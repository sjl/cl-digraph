(in-package :digraph-test)


;;;; Utils --------------------------------------------------------------------
(defmacro define-test (name &body body)
  `(test ,(symb 'test- name)
    (let ((*package* ,*package*))
      ,@body)))


(defun same (a b)
  (null (set-exclusive-or a b :test #'equal)))

(defun run-tests ()
  (1am:run))


;;;; Tests --------------------------------------------------------------------
(define-test make-digraph
  (let ((g (make-digraph)))
    (is (zerop (size g)))
    (is (same () (vertices g)))
    (is (same () (edges g))))
  (let ((g (make-digraph :initial-vertices '(a b c))))
    (is (= 3 (size g)))
    (is (same '(a b c) (vertices g)))
    (is (same () (edges g))))
  (let ((g (make-digraph :initial-vertices '(a b a c a a))))
    (is (= 3 (size g)))
    (is (same '(a b c) (vertices g)))
    (is (same () (edges g)))))


(define-test insert-vertex
  (let ((g (make-digraph)))
    (insert-vertex g 'a)
    (insert-vertex g 'b)
    (insert-vertex g 'c)
    (insert-vertex g 'a) ; dup
    (is (= 3 (size g)))
    (is (same '(a b c) (vertices g)))
    (is (same () (edges g)))))

(define-test insert-edge
  (let ((g (make-digraph :initial-vertices '(a b c))))
    (insert-edge g 'a 'b)
    (is (same '((a . b))
              (edges g)))

    (insert-edge g 'b 'c)
    (is (same '((a . b) (b . c))
              (edges g)))))


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

