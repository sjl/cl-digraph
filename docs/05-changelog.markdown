Changelog
=========

Here's the list of changes in each released version.

[TOC]

v1.4.0
------

Added an explicit [condition hierarchy](../usage#conditions).

v1.3.2
------

[Fixed a bug](https://github.com/sjl/cl-digraph/issues/4) where certain kinds of
cycles were not correctly detected during topological sorting.

v1.3.1
------

[Fixed a bug](https://github.com/sjl/cl-digraph/pull/3) for recent SBCL versions
when creating a digraph without a custom hash function.

v1.3.0
------

Added the `arbitrary-vertex` function to return an arbitrary vertex of
a digraph.

v1.2.1
------

Fixed a bug in `copy-digraph`.

v1.2.0
------

Added `rootp` and `leafp` predicates to check whether a vertex is a root/leaf in
a digraph.

v1.1.0
------

Minor internal cleanup.

If you pass an invalid `strategy` argument to `reachablep` there will now be
a restart available to supply a new value, instead of just crashing and burning.

v1.0.0
------

Initial version.
