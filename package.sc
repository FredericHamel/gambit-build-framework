; #!/usr/bin/gsc-script -i _new-build-example.scm
; (##require-module _build)
;(macro-load-build)

(build#setup)

(build#link
  (build#make-project!
    (build#add-sources preload: #f "_build.scm"
                       (path-expand "~~lib/_gambitgsc.c"))
    options: '((ld-options "~~lib/libgambitgsc.a")
               (l "~~lib/_gambitgsc.c))
  ;link-options: "-lX11"
  )

