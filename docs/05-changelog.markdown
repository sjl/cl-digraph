Changelog
=========

Here's the list of changes in each released version.

[TOC]

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
