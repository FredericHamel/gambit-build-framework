#!/usr/bin/env gsc-script -i
(load "build/_build")

(set! build#verbosity-level 0)

(build#setup)
(build#compile "test1.scm") ;; TODO: use a list instead
(build#compile "test.scm")
(build#link-dynamic)
(println "intermediate-files: " 
         (object->string build#intermediate-files))

