cl-digraph is an implementation of a mutable [directed graph][] data structure
for Common Lisp.

[directed graph]: https://en.wikipedia.org/wiki/Directed_graph

* **License:** MIT
* **Documentation:** <https://docs.stevelosh.com/cl-digraph/>
* **Mercurial:** <https://hg.stevelosh.com/cl-digraph/>
* **Git:** <https://github.com/sjl/cl-digraph/>

cl-digraph focuses on simplicity, correctness, and usability.  Performance is
not *terrible*, but is not a high priority.

It is currently not thread-safe, but this may happen in the future.

The test suite currently passes in SBCL, CCL, ECL, and ABCL on OS X and Debian.
Further testing is welcome.
