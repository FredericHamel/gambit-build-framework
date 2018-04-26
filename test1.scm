
(define-macro (dummy)
  (table-set! (##compilation-scope) 'module-name "github.com/FredericHamel/test1")
  #f)
(println "Loading test1.scm")
(dummy)
(pp (this-source-file))

