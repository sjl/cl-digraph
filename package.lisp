(defpackage :digraph
  (:use :cl :digraph.quickutils)
  (:export
    :digraph
    :make-digraph

    :vertices
    :edges

    :predecessors
    :successors
    :neighbors

    :contains-vertex-p
    :contains-edge-p

    :insert-vertex
    :insert-edge

    :remove-edge
    :remove-vertex

    :degree
    :degree-in
    :degree-out
    :size

    :do-vertices
    :do-edges
    :mapc-vertices
    :mapc-edges
    :map-vertices
    :map-edges

    :copy-digraph
    ))
