(asdf:defsystem :cl-digraph
  :name "digraph"
  :description "Simple directed graphs for Common Lisp."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on ()

  :in-order-to ((asdf:test-op (asdf:test-op :digraph-test)))

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "directed-graph")))))



(asdf:defsystem #:digraph-test
  :name "digraph-test"

  :depends-on (#:1am)

  :serial t
  :components ((:file "package-test")
               (:module "test"
                :serial t
                :components ((:file "tests"))))

  :perform (asdf:test-op
             (op system)
             (uiop:symbol-call :digraph-test :run-tests)))
