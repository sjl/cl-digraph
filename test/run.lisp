#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload 'cl-digraph.test :silent t)
(time (asdf:test-system 'cl-digraph))
(quit)
