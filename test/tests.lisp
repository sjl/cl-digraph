(in-package :digraph-test)

(defmacro define-test (name &body body)
  `(test ,name
    (let ((*package* ,*package*))
      ,@body)))

(defun run-tests ()
  (1am:run))

(define-test foo
  (is (= 1 2)))
