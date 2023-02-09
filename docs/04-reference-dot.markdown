# cl-dot Support

cl-digraph includes support for drawing digraphs with Graphviz using `cl-dot`
  in the `cl-digraphs.dot` system.

  [TOC]

## Package `DIGRAPH.DOT`

### `DRAW` (function)

    (DRAW DIGRAPH &KEY (FILENAME digraph.png) (FORMAT :PNG) (SHAPE :CIRCLE))

Draw `digraph` with cl-dot.

