
(setup)

(link
  (make-project
    (add-sources "_build.scm")

    link-base: "~~lib/libgambitgsc.a"
    options: '((l "~~lib/_gambitgsc.c")))
  )

