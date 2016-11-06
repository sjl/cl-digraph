Installation
============

cl-digraph is compatible with Quicklisp, but not *in* Quicklisp (yet?).  You can
clone the repository into your [Quicklisp local-projects][local] directory for
now.

The `cl-digraph` system contains the core API and has no dependencies.

The `cl-digraph.dot` system contains support for drawing digraphs with Graphviz
using [cl-dot][].

The `cl-digraph.test` system contains the test suite, which uses [1am][].

[local]: https://www.quicklisp.org/beta/faq.html#local-project
[1am]: https://github.com/lmj/1am
[cl-dot]: https://github.com/michaelw/cl-dot
