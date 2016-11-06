(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :appendf
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
  :package "DIGRAPH.QUICKUTILS")
