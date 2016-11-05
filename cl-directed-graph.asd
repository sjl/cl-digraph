(asdf:defsystem :cl-directed-graph
  :name "directed-graph"
  :description "Simple directed graphs in Common Lisp."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on ()

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "directed-graph")))))
