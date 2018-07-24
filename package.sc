; #!/usr/bin/gsc-script -i _new-build-example.scm
; (##require-module _build)
;(macro-load-build)

(include "_build#.scm")

(setup)

(link
  (make-project!
    (add-sources "_build.scm")
    link-base: "~~lib/libgambitgsc.a"
    options: '((l "~~lib/_gambitgsc.c")))
  ;link-options: "-lX11"
  )

