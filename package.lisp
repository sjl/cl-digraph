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

    :count-vertices
    :count-edges

    :do-vertices
    :do-edges
    :mapc-vertices
    :mapc-edges
    :map-vertices
    :map-edges

    :map-depth-first
    :map-breadth-first
    :mapc-depth-first
    :mapc-breadth-first

    :copy-digraph))
