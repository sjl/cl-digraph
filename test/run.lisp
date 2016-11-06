#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload 'cl-digraph)
(time (asdf:test-system 'cl-digraph))
(quit)
