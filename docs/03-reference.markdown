# API Reference

The following is a list of all user-facing parts of cl-digraph.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `DIGRAPH`

### `CONTAINS-EDGE-P` (function)

    (CONTAINS-EDGE-P DIGRAPH PREDECESSOR SUCCESSOR)

Return whether the graph contains an edge from `predecessor` to `successor`.

### `CONTAINS-VERTEX-P` (function)

    (CONTAINS-VERTEX-P DIGRAPH VERTEX)

Return whether the graph contains `vertex`.

### `COPY-DIGRAPH` (function)

    (COPY-DIGRAPH DIGRAPH)

Create a fresh copy of `digraph`.

  The vertex objects themselves are not copied, but everything else is.

  

### `COUNT-EDGES` (function)

    (COUNT-EDGES DIGRAPH)

Return the number of edges in `digraph`.

### `COUNT-VERTICES` (function)

    (COUNT-VERTICES DIGRAPH)

Return the number of vertices in `digraph`.

### `DEGREE` (function)

    (DEGREE DIGRAPH VERTEX)

Return the number of neighbors of `vertex`.

### `DEGREE-IN` (function)

    (DEGREE-IN DIGRAPH VERTEX)

Return the number of predecessors of `vertex`.

### `DEGREE-OUT` (function)

    (DEGREE-OUT DIGRAPH VERTEX)

Return the number of successors of `vertex`.

### `DIGRAPH` (class)

A directed graph.  Use `make-digraph` to create one.

### `EDGES` (function)

    (EDGES DIGRAPH)

Return a fresh list of the edges of `digraph`.

  Each edge will be a cons of the form `(predecessor . successor)`.

  

### `EMPTYP` (function)

    (EMPTYP DIGRAPH)

Return `t` if `digraph` has no vertices or edges, `nil` otherwise.

### `INSERT-EDGE` (function)

    (INSERT-EDGE DIGRAPH PREDECESSOR SUCCESSOR)

Insert an edge from `predecessor` to `successor` if not already present.

  The `predecessor` and `successor` vertices must exist in the graph already.

  Returns `t` if the edge was already in the graph, or `nil` if it was
  inserted.

  

### `INSERT-VERTEX` (function)

    (INSERT-VERTEX DIGRAPH VERTEX)

Insert `vertex` into the graph if it is not already a member.

  Returns `t` if the vertex was already in the graph, or `nil` if it was
  inserted.

  

### `LEAFS` (function)

    (LEAFS DIGRAPH)

Return all leaf vertices in `digraph`.

  This is currently O(vertices).

  A root is a vertex with no outgoing edges (i.e. out-degree 0).

  

### `MAKE-DIGRAPH` (function)

    (MAKE-DIGRAPH &KEY INITIAL-VERTICES (TEST #'EQL) (HASH-FUNCTION NIL))

Create and return a new digraph.

  `initial-vertices` can be a sequence of vertices to add to the graph.

  `test` should be one of the hash table equality predicates.

  If your Lisp implementation supports the `:hash-function` argument for
  creating hash tables with custom predicates, you can specify one with
  `hash-function`.

  

### `MAP-BREADTH-FIRST` (function)

    (MAP-BREADTH-FIRST FUNCTION DIGRAPH START-VERTEX)

Apply `function` to the vertices of a breadth-first traversal of `digraph`.

  Returns a fresh list with the results.

  Vertices are processed in breadth-first order, beginning at `start-vertex`,
  and the resulting list has this order as well.

  Cycles in the graph will not be traversed into.

  

### `MAP-DEPTH-FIRST` (function)

    (MAP-DEPTH-FIRST FUNCTION DIGRAPH START-VERTEX)

Apply `function` to the vertices of a breadth-first traversal of `digraph`.

  Returns a fresh list with the results.

  Vertices are processed in depth-first order, beginning at `start-vertex`, and
  the resulting list has this order as well.

  Cycles in the graph will not be traversed into.

  

### `MAP-EDGES` (function)

    (MAP-EDGES FUNCTION DIGRAPH)

Return a fresh list with the results of calling `function` on each edge.

  For each edge, `function` will be called once with two arguments:

    (function predecessor successor)

  The order of the resulting list is unspecified.

  

### `MAP-VERTICES` (function)

    (MAP-VERTICES FUNCTION DIGRAPH)

Return a fresh list with the results of calling `function` on each vertex.

  The order of the resulting list is unspecified.

  

### `MAPC-BREADTH-FIRST` (function)

    (MAPC-BREADTH-FIRST FUNCTION DIGRAPH START-VERTEX)

Apply `function` to the vertices of a breadth-first traversal of `digraph`.

  Returns `nil`.

  Vertices are processed in breadth-first order, beginning at `start-vertex`.

  Cycles in the graph will not be traversed into.

  

### `MAPC-DEPTH-FIRST` (function)

    (MAPC-DEPTH-FIRST FUNCTION DIGRAPH START-VERTEX)

Apply `function` to the vertices of a depth-first traversal of `digraph`.

  Returns `nil`.

  Vertices are processed in depth-first order, beginning at `start-vertex`.

  Cycles in the graph will not be traversed into.

  

### `MAPC-EDGES` (function)

    (MAPC-EDGES FUNCTION DIGRAPH)

Call `function` on each edge in `digraph`.

  For each edge, `function` will be called once with two arguments:

    (function predecessor successor)

  The order in which the edges are processed is unspecified.

  Returns `nil`.

  

### `MAPC-VERTICES` (function)

    (MAPC-VERTICES FUNCTION DIGRAPH)

Call `function` on each vertex in `digraph`.

  The order in which the vertices are processed is unspecified.

  Returns `nil`.

  

### `NEIGHBORS` (function)

    (NEIGHBORS DIGRAPH VERTEX)

Return a fresh list of the neighbors of `vertex`.

### `PREDECESSORS` (function)

    (PREDECESSORS DIGRAPH VERTEX)

Return a fresh list of the predecessors of `vertex`.

### `REACHABLEP` (function)

    (REACHABLEP DIGRAPH START TARGET &KEY (STRATEGY :BREADTH-FIRST))

Return `t` if it is possible to reach `target` from `start`, otherwise `nil`.

  All vertices are reachable from themselves.

  Otherwise a `target` is reachable from `start` if a directed path exists from
  the start to the target.

  `strategy` will be used to determine how to traverse the graph when searching
  for a path, and can be one of `:breadth-first` or `:depth-first`.

  

### `REMOVE-EDGE` (function)

    (REMOVE-EDGE DIGRAPH PREDECESSOR SUCCESSOR)

Remove an edge from `predecessor` to `successor` if present.

  Returns `t` if there was such an edge, or `nil` if not.

  

### `REMOVE-VERTEX` (function)

    (REMOVE-VERTEX DIGRAPH VERTEX)

Remove `vertex` from the graph if present.

  If there are any edges to/from `vertex` they will be automatically removed.

  Returns `t` if there was such a vertex, or `nil` if not.

  

### `ROOTS` (function)

    (ROOTS DIGRAPH)

Return all root vertices in `digraph`.

  This is currently O(vertices).

  A root is a vertex with no incoming edges (i.e. in-degree 0).

  

### `SUCCESSORS` (function)

    (SUCCESSORS DIGRAPH VERTEX)

Return a fresh list of the successors of `vertex`.

### `TOPOLOGICAL-SORT` (function)

    (TOPOLOGICAL-SORT DIGRAPH)

Return a fresh list of the vertices of `digraph` in topological order.

  Edges are treated as meaning "depends on", so an edge `A --> B` means "A
  depends on B" and that B must come before A in the resulting list.  Aside
  from this restriction, the order of the resulting list is arbitrary.

  The order in which the vertices are processed is unspecified.

  An error will be signaled if the graph contains a cycle.

  

### `VERTICES` (function)

    (VERTICES DIGRAPH)

Return a fresh list of the vertices of `digraph`.

