Usage
=====

cl-digraph is a simple library for working with [directed graphs][] in Common
Lisp.

[directed graphs]: https://en.wikipedia.org/wiki/Directed_graph

[TOC]

Package
-------

All core cl-digraph functions are in the `digraph` package.  You can `:use` that
if you really want to, but it's probably clearer to use namespaced `digraph:...`
symbols.

Creating Digraphs
-----------------

Digraphs can be created with `make-digraph`:

    :::lisp
    (digraph:make-digraph)
    ; => #<DIGRAPH:DIGRAPH () {1002CFD343}>

Working with Vertices
---------------------

Vertices can be added to a digraph with `insert-vertex`, and a list of all
vertices in the graph retrieved with `vertices`:

    :::lisp
    (defparameter *d* (digraph:make-digraph))

    (digraph:vertices *d*)
    ; => ()

    (digraph:insert-vertex *d* 'foo)
    (digraph:vertices *d*)
    ; => (foo)

    (digraph:insert-vertex *d* 'bar)
    (digraph:vertices *d*)
    ; => (bar foo)

The order of vertices returned in the list is arbitrary.  We'll see how to
retrieve vertices in specific orders later.

Duplicate vertices are silently ignored:

    :::lisp
    (defparameter *d* (digraph:make-digraph))

    (digraph:insert-vertex *d* 'foo)
    (digraph:insert-vertex *d* 'foo)
    (digraph:insert-vertex *d* 'foo)
    (digraph:vertices *d*)
    ; => (foo)

You can also specify some initial vertices directly in the `make-digraph` call
if you want:

    :::lisp
    (defparameter *d*
      (digraph:make-digraph :initial-vertices '(a b c)))

    (digraph:vertices *d*)
    ; => (a c b)

    (digraph:insert-vertex *d* 'foo)
    (digraph:vertices *d*)
    ; => (a c foo b)

You can remove vertices with `remove-vertex`.  Removing a vertex that's not in
the graph is silently ignored:

    :::lisp
    (defparameter *d*
      (digraph:make-digraph :initial-vertices '(a b c)))

    (digraph:vertices *d*)
    ; => (a c b)

    (digraph:remove-vertex *d* 'a)
    (digraph:vertices *d*)
    ; => (c b)

    (digraph:remove-vertex *d* 'cats)
    (digraph:vertices *d*)
    ; => (c b)

Equality
--------

By default cl-digraph compares vertices for equality with `eql`.  You can
specify a different equality predicate with the `:test` argument to
`make-digraph`:

    :::lisp
    (defparameter *d*
      (digraph:make-digraph :test #'equal))

    (digraph:insert-vertex *d* (list 1 2))
    (digraph:insert-vertex *d* (list 3 4))
    (digraph:vertices *d*)
    ; => ((1 2) (3 4))

    (digraph:insert-vertex *d* (list 1 2))
    (digraph:vertices *d*)
    ; => ((1 2) (3 4))

    (digraph:remove-vertex *d* (list 1 2))
    (digraph:vertices *d*)
    ; => ((3 4))

cl-digraph stores data in hash tables internally, so `test` must be one of the
predicates supported as a hash table test (`eq`, `eql`, `equal`, or `equalp`).

If your Lisp implementation supports creating hash tables with custom hash
functions with the `:hash-function` argument to `make-hash-table`, you can use
them with cl-digraph as well:

    :::lisp
    (defparameter *d*
      (digraph:make-digraph :test #'some-predicate
                            :hash-function #'custom-hash-function))

This should work in SBCL, LispWorks, Allegro, CCL, and possibly others.

Working with Edges
------------------

Once you've got some vertices in a digraph you can add edges between them.  The
vertex that an edge goes *out of* is called the **predecessor**, and the vertex
the edge goes *into* is called the **successor**:

    ┌─────────────┐      ┌─────────────┐
    │ predecessor │─────▶│  successor  │
    └─────────────┘      └─────────────┘

Edges are added with `insert-edge`.  A list of edges in a digraph can be
retrieved with `edges`:

    :::lisp
    (defparameter *d*
      (digraph:make-digraph :initial-vertices '(a b c)))

    (digraph:edges *d*)
    ; => ()

    (digraph:insert-edge *d* 'a 'b) ; a -> b
    (digraph:edges *d*)
    ; => ((a . b))

    (digraph:insert-edge *d* 'b 'c) ; b -> c
    (digraph:edges *d*)
    ; => ((b . c) (a . b))

Duplicate edges are silently ignored.  The predecessor and successor must both
exist in the graph already, or an error will be signaled:

    :::lisp
    (defparameter *d*
      (digraph:make-digraph :initial-vertices '(a b c)))

    (digraph:insert-edge *d* 'a 'b) ; a -> b
    (digraph:insert-edge *d* 'a 'b) ; ignored
    (digraph:insert-edge *d* 'a 'b) ; ignored
    (digraph:edges *d*)
    ; => ((a . b))

    (digraph:insert-edge *d* 'cats 'dogs)
    ; =>
    ; Cannot add edge with predecessor CATS because it is not in the graph
    ;    [Condition of type DIGRAPH::MISSING-PREDECESSOR]
    ;
    ; Restarts:
    ;   R 0. CONTINUE - Retry assertion with new value for DIGRAPH::PREDECESSOR.
    ;   R 1. ABORT    - Exit debugger, returning to top level.

See the [Conditions](#conditions) section for more information about the error
hierarchy.

Edges can be removed with `remove-edge`.  Removing an edge that's not in the
graph is silently ignored:

    :::lisp
    (defparameter *d*
      (digraph:make-digraph :initial-vertices '(a b c)))

    (digraph:insert-edge *d* 'a 'b) ; a -> b
    (digraph:edges *d*)
    ; => ((a . b))

    (digraph:remove-edge *d* 'a 'b) ; removes a -> b
    (digraph:remove-edge *d* 'a 'b) ; ignored
    (digraph:edges *d*)
    ; => ()

Retrieving Digraph Information
------------------------------

Once you've got a digraph you might want to ask it about itself.  Let's consider
a simple digraph as an example:

    :::lisp
    ;            ┌───┐      ┌───┐
    ;   ┌───────▶│ B │─────▶│ D │
    ;   │        └───┘      └───┘
    ; ┌───┐
    ; │ A │               ┌─────┐       ┌─────┐
    ; └───┘               │ FOO │──────▶│ BAR │──┐
    ;   │        ┌───┐    └─────┘       └─────┘  │
    ;   └───────▶│ C │                     ▲     │
    ;            └───┘                     │     │
    ;                                      └─────┘
    (defparameter *d*
      (digraph:make-digraph :initial-vertices '(a b c d foo bar)))

    (loop :for (from to) :in '((a b) (a c) (b d) (foo bar) (bar bar))
          :do (digraph:insert-edge *d* from to))

Notice that digraphs don't have to be connected, and vertices can have edges to
themselves.

### Vertices and Edges

We've already seen `vertices` and `edges`:

    :::lisp
    (digraph:vertices *d*)
    ; => (BAR FOO D C B A)

    (digraph:edges *d*)
    ; => ((BAR . BAR) (FOO . BAR) (B . D) (A . B) (A . C))

These functions return their results in an arbitrary order — don't rely on it
being anything in particular.

### Neighboring Vertices

The `predecessors` and `successors` functions return a list of vertices with
edges to/from a particular vertex:

    :::lisp
    (digraph:predecessors *d* 'a) ; => ()
    (digraph:successors *d* 'a)   ; => (b c)

    (digraph:predecessors *d* 'bar) ; => (foo bar)
    (digraph:successors *d*   'bar) ; => (bar)

`neighbors` returns all vertices that are a predecessor *or* successor of the
given vertex:

    :::lisp
    (digraph:neighbors *d* 'b) ; => (a d)

### Membership

To check whether a digraph contains a particular edge or vertex use
`contains-vertex-p` and `contains-edge-p`:

    :::lisp
    (digraph:contains-vertex-p *d* 'a)      ; => t
    (digraph:contains-vertex-p *d* 'horses) ; => nil

    (digraph:contains-edge-p *d* 'a 'b)     ; => t
    (digraph:contains-edge-p *d* 'a 'foo)   ; => nil

### Sizes and Counts

If you just want the *number* of vertices or edges in a digraph and don't need
a list of them, use `count-vertices` and `count-edges`:

    :::lisp
    (digraph:count-vertices *d*) ; => 6
    (digraph:count-edges *d*)    ; => 5

Similarly, if you want to know the number of edges into/out of/involving
a vertex use `degree`, `degree-in`, and `degree-out`:

    :::lisp
    (digraph:predecessors *d* 'a) ; => ()
    (digraph:degree-in    *d* 'a) ; = 0

    (digraph:successors *d* 'bar) ; => (bar)
    (digraph:degree-out *d* 'bar) ; => 1

    (digraph:neighbors  *d* 'b) ; => (a d)
    (digraph:degree-out *d* 'b) ; => 2

Mapping, Traversal, and Sorting
-------------------------------

Sometimes you may want to perform an action on each vertex or edge in a directed
graph, possibly in a specific order.

### Unordered Mapping

If you don't care about the order the items are processed/returned in, use one
of the unordered mapping functions:

* `(digraph:mapc-vertices function digraph)`
* `(digraph:mapc-edges function digraph)`
* `(digraph:map-vertices function digraph)`
* `(digraph:map-edges function digraph)`

The `map-` variants return a fresh list of the results of calling `function` on
the argument(s).

The `mapc-` variants return `nil`, so you'd want to use them for the side
effects of `function`.

The `-vertices` variants call `function` with a single argument: the vertex.

The `-edges` variants call `function` with two arguments: the predecessor and
successor.

### Ordered Traversal

Sometimes you may want to traverse the vertices of a digraph in depth-first or
breadth-first order.  You can use the ordered mapping functions for this:

* `(digraph:mapc-depth-first function digraph start-vertex)`
* `(digraph:mapc-breadth-first function digraph start-vertex)`
* `(digraph:map-depth-first function digraph start-vertex)`
* `(digraph:map-breadth-first function digraph start-vertex)`

If a traversal contains a cycle the traversal will stop that line of traversing
instead of looping infinitely.

### Topological Sorting

One common use of (acyclic) digraphs is to represent graphs of dependencies,
e.g. library `foo` depends on library `bar`, and `bar` depends on `baz`.

Often the end goal of constructing such a graph is to produce a [topologically
sorted][] list of the vertices — a list where each vertex comes after the ones
it depends on.  cl-digraph can produce a list in this order with the
`topological-sort` function:

    :::lisp
    (defparameter *d*
      (digraph:make-digraph :initial-vertices '(a b c d)))

    (digraph:insert-edge *d* 'a 'b) ; a depends on b
    (digraph:insert-edge *d* 'a 'c) ; a depends on c
    (digraph:insert-edge *d* 'd 'a) ; d depends on a

    (digraph:topological-sort *d*)
    ; => one of
    ; (C B A D)
    ; (B C A D)

A `digraph:topological-sort-cycle` will be signaled if the digraph
contains a cycle:

    :::lisp
    (defparameter *d*
      (digraph:make-digraph :initial-vertices '(a b c d)))

    (digraph:insert-edge *d* 'a 'b) ; a depends on b
    (digraph:insert-edge *d* 'b 'c) ; b depends on c
    (digraph:insert-edge *d* 'c 'a) ; c depends on a

    (digraph:topological-sort *d*)
    ; =>
    ; Cycle detected during topological sort involving vertex A
    ;     [Condition of type DIGRAPH:TOPOLOGICAL-SORT-CYCLE]
    ;
    ; Restarts:
    ;   R 0. ABORT - Exit debugger, returning to top level.

See the [Conditions](#conditions) section for more information about the error
hierarchy.

[topologically sorted]: https://en.wikipedia.org/wiki/Topological_sorting

Conditions
----------

The following condition types are defined by cl-digraph:

[![condition type hierarchy](../static/conditions.svg)](../static/conditions.svg)

Dotted outlines denote abstract types that are never actually instantiated, but
can be useful for handling whole classes of errors.

* `digraph-error`: abstract type for digraph-related errors.
* `missing-vertex`: abstract type for errors signaled when trying to insert an edge involving a vertex that is not in the graph.
* `missing-predecessor`: error signaled when trying to insert an edge whose predecessor is not in the graph.
* `missing-successor`: error signaled when trying to insert an edge whose successor is not in the graph.
* `topological-sort-cycle`: error signaled when trying to topologically sort a graph involving a cycle.

For `missing-vertex` errors of both kinds you can use the `vertex-involved`
reader to retrieve the offending vertex from the condition object.

For `topological-sort-cycle` errors you can use the `vertex-involved` reader to
retrieve one of the vertices involved in a cycle from the condition object.
*Which* vertex of the cycle is returned is arbitrary:

    :::lisp
    (defparameter *d*
      (digraph:make-digraph :initial-vertices '(a b c d)))

    (digraph:insert-edge *d* 'a 'b) ; a depends on b
    (digraph:insert-edge *d* 'b 'c) ; b depends on c
    (digraph:insert-edge *d* 'c 'a) ; c depends on a

    (handler-case (digraph:topological-sort *d*)
      (digraph:topological-sort-cycle (c)
        (list :cyclic (digraph:vertex-involved c))))
    ; =>
    ; (:CYCLIC A)

Drawing
-------

If you have [Graphviz][] installed, you can draw digraph objects to images with
the [cl-dot][] library by loading the optional `cl-digraph.dot` system:

    :::lisp
    (ql:quickload 'cl-digraph.dot)

    (defparameter *d*
      (digraph:make-digraph :initial-vertices '(a b c d foo bar)))

    (loop :for (from to) :in '((a b) (a c) (b d) (foo bar) (bar bar))
          :do (digraph:insert-edge *d* from to))

    (digraph.dot:draw *d* :filename "digraph.png" :format :png)

![Digraph PNG](http://i.imgur.com/TtyGQfM.png)

[Graphviz]: http://www.graphviz.org/
[cl-dot]: https://github.com/michaelw/cl-dot
