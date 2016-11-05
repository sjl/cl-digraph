(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :dohash
               :ensure-boolean
               :ensure-gethash
               :ensure-list
               :hash-table-keys
               :maphash-keys
               :mkstr
               :once-only
               :rcurry
               :removef
               :symb
               :with-gensyms

               )
  :package "DIRECTED-GRAPH.QUICKUTILS")
