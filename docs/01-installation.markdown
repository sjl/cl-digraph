Installation
============

cl-digraph can be installed with [Quicklisp][]: `(ql:quickload :cl-digraph)`

The `cl-digraph` system contains the core API and has no dependencies.

The `cl-digraph.dot` system contains support for drawing digraphs with Graphviz
using [cl-dot][].

The `cl-digraph.test` system contains the test suite, which uses [1am][].

[quicklisp]: https://quicklisp.org/
[1am]: https://github.com/lmj/1am
[cl-dot]: https://github.com/michaelw/cl-dot
