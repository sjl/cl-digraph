(ql:quickload "cl-d-api")

(defparameter *header*
  "The following is a list of all user-facing parts of cl-digraph.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

")

(d-api:generate-documentation
  :cl-digraph
  #p"docs/03-reference.markdown"
  (list "DIGRAPH")
  *header*
  :title "API Reference")

(d-api:generate-documentation
  :cl-digraph.dot
  #p"docs/04-reference-dot.markdown"
  (list "DIGRAPH.DOT")
  "cl-digraph includes support for drawing digraphs with Graphviz using `cl-dot`
  in the `cl-digraphs.dot` system.

  "
  :title "cl-dot Support")

(d-api:draw-class-hierarchy
  "docs/static/conditions.svg"
  '(digraph::digraph-error
    digraph::missing-vertex
    digraph::missing-predecessor
    digraph::missing-successor
    digraph::topological-sort-cycle)
  :abstract-classes
  '(digraph::digraph-error
    digraph::missing-vertex))

