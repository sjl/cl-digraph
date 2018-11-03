(asdf:defsystem :cl-digraph
  :description "Simple directed graphs for Common Lisp."
  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://sjl.bitbucket.io/cl-digraph/"

  :license "MIT/X11"
  :version "1.2.1"

  :depends-on ()

  :in-order-to ((asdf:test-op (asdf:test-op :cl-digraph.test)))

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "directed-graph")))))
