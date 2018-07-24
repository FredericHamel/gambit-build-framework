#!/usr/bin/env gsc-script -i $0
(macro-load-build)

(build#setup)
(build#compile preload: #f "_build.scm")
(build#link-dynamic)
