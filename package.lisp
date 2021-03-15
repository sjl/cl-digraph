(defpackage :digraph
  (:use :cl :digraph.quickutils)
  (:export
    :digraph
    :make-digraph

    :emptyp

    :vertices
    :edges

    :arbitrary-vertex

    :roots
    :leafs

    :rootp
    :leafp

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

    :mapc-vertices
    :mapc-edges
    :map-vertices
    :map-edges

    :map-depth-first
    :map-breadth-first
    :mapc-depth-first
    :mapc-breadth-first

    :topological-sort

    :reachablep

    :copy-digraph

    :digraph-error
    :missing-vertex
    :missing-predecessor
    :missing-successor
    :topological-sort-cycle
    :vertex-involved))
