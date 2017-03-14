(asdf:defsystem :cl-digraph.test
  :description "Test suite for cl-digraph"

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (:cl-digraph
               :1am)

  :serial t
  :components ((:file "package.test")
               (:module "test"
                :serial t
                :components ((:file "tests"))))

  :perform (asdf:test-op
             (op system)
             (uiop:symbol-call :digraph.test :run-tests)))
