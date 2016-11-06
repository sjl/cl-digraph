(asdf:defsystem :cl-digraph.dot
  :description "cl-dot support for cl-digraph"

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT/X11"

  :depends-on (:cl-digraph
               :cl-dot)

  :serial t
  :components ((:file "package.dot")
               (:module "src" :serial t
                :components ((:file "dot")))))
