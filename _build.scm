
(namespace ("build#"))

(##include "~~lib/gambit#.scm")
(##include "~~lib/_gambit#.scm")

;; Define default options
(define-macro (macro-default-script-filename) "package.sc")

;; Options pass on the command line.
(define-type buildref
  file type
  target
  opts
  compile-opts
  linker-opts)

;(define (make-project prefix source-files intermediate-files intermediate-obj-files linker-options)
;  (vector prefix source-files intermediate-files intermediate-obj-files linker-options))

(define-type project
  constructor: make-raw-project
  prefix
  source-files ;; With options
  intermediate-files
  intermediate-obj-files
  link-base ;; Used in link-incremental
  options)

(define current-build-directory (make-parameter #f))

;; Some global state
(define project-global-object (make-parameter #f))
(define project-global-prefix (make-parameter #f))

(define dependency-list (make-parameter '()))

;; Last dependancy project object
(define dependency-object (make-parameter #f))

(define (make-project source-files #!key (link-base #f) (options '()))
  (let ((p (make-raw-project (or (project-global-prefix) "src")
                             source-files #f #f link-base options)))
    ;; Dependancy if not main project.
    (if (project-global-object)
      (dependency-object p)
      (project-global-object p))
    p))

;; Backward compatibility.
(define make-project! make-project)

;; name: fullname of the library (i.e. github.com/feeley/crypto)
;;  - the name contain the path that contain the package.sc
(define (library name)
  (if (project-global-object)
    (and
      (not (member name (dependency-list)))
      (let ((fullpath (path-expand
                        ;; FIXME: Should be a buildref attribute
                        (path-expand "package.sc" name)
                        (or (##os-path-gambitdir-map-lookup "userlib")
                            "~/.gambit_userlib"))))

        (if (file-exists? fullpath)
          ;; Resolve lib
          (parameterize
            ;; Library fetch ctx
            ((project-global-prefix name)
             (dependency-object #f))
            (and (not (member name (dependency-list)))
                 (begin
                   (load fullpath)
                   (dependency-list (cons name (dependency-list)))))
            (dependency-object))

          (error "FileNotFound: " fullpath))))
    (error "Main project should be declare")))

(define verbosity-level 0)

(define (println-log #!rest args #!key (verbosity 0))
  (if (< verbosity verbosity-level)
    (apply println args)))

;; Create directory if it does not exist.
(define (mkdir dir)
  (if (not (file-exists? dir))
    (create-directory dir)))

(define (create-directory-tree dir destdir)
  (let ((fulldir (path-expand dir destdir)))
    (let ((fulldir-len (string-length fulldir)))
      (let loop ((i (string-length destdir)))
        (if (< i fulldir-len)
          (case (string-ref fulldir i)
            ((#\/)
             (mkdir (substring fulldir 0 i))
             (loop (+ i 1)))
            (else
              (loop (+ i 1)))))))))

(define (delete-directory-tree dir)
  (for-each
    (lambda (subdir)
      (let ((absolute-subdir (path-expand subdir dir)))
        (delete-directory-tree absolute-subdir)))
    (directory-files dir))
  (delete-directory dir))

(define (options->string opts)
  (if (pair? opts)
    (let loop ((opts (cdr opts)) (result (car opts)))
      (if (pair? opts)
        (loop (cdr opts)
              (string-append result " " (car opts)))
        result))
    ""))


;(create-directory-tree "a/b/c/" (current-directory))

(define (parse-command-line)
  (let loop ((cl (cdr (command-line)))
             (file #f) (type #f) (target #f)
             (options '())
             (compile-options '())
             (link-options '()))
    (if (pair? cl)
      (let ((arg (car cl))
            (rest (cdr cl)))
        (cond
          ((string=? arg "-f")
           (if (pair? rest)
             (if file
               (error "Parameter -f already specified")
               (loop (cdr rest) (car rest) type target options compile-options link-options))
             (error "Missing argument to -f")))

          ((string=? arg "-debug")
           (loop rest file type target (cons '(debug) options) compile-options link-options))

          ((string=? arg "-verbose")
           (loop rest file type target (cons '(verbose) options) compile-options link-options))

          ((string=? arg "-dynamic")
           (if type
             (error "Build type is already defined")
             (loop rest file 'dyn target options (cons "-D___DYNAMIC" compile-options link-options))))

          ((string=? arg "-exe")
           (if type
             (error "Build type is already defined")
             (loop rest file 'exe target options compile-options link-options)))

          ((string=? arg "-load")
           (if (pair? rest)
             (begin
               (load (car rest))
               (loop (cdr rest) file type target options compile-options link-options))
             (error "Missing argument to -load")))

          ((string=? arg "-target")
           (if (pair? rest)
             (if target
               (error "Build target is already defined")
               (let ((new-target (string->symbol (car rest))))
                 (loop (cdr rest) file type new-target options compile-options link-options)))
             (error "Missing argument to -target")))

          ;; TODO: add some link-options

          (else
            (error (string-append "Not a supported argument '" arg "'")))))

      (make-buildref
        (or file (macro-default-script-filename))
        (or type 'dyn)
        (or target (c#default-target))
        options
        (if type ;; Fallback to default
          compile-options
          ;; Need this to build dynamic library.
          (cons "-D___DYNAMIC" compile-options))
        link-options))))




(define (setup)
  (let* ((target (buildref-target info))
         (cwd (current-directory))
         (build-subdirectory-name (##build-subdir-name target)))
    (mkdir build-subdirectory-name)

    (current-build-directory
      (path-expand build-subdirectory-name cwd))))


(define (add-sources arg #!rest args)
  (let loop ((rev-files '())
             (file-opts '())
             (rest (cons arg args)))
    (if (pair? rest)
      (let ((opt (car rest))
            (opts-rest (cdr rest)))
        (cond
          ((keyword? opt)
           (case opt
             ((preload:)
              (if (pair? opts-rest)
                (let ((bool (car opts-rest)))
                  (if (boolean? bool)
                    (loop rev-files
                          (cons
                            (cons 'preload bool)
                            file-opts)
                          (cdr opts-rest))
                    (error "Expected boolean")))
                (error "Missing argument to preload")))

             ((linker-name:)
              (if (pair? opts-rest)
                (let ((libname (car opts-rest)))
                  (if (string? libname)
                    (loop rev-files
                          (cons
                            (cons 'linker-name libname)
                            file-opts)
                          (cdr opts-rest))
                    (error "Expected string")))
                (error "Missing argument to linker-name")))

             ((ld-options:)
              ; macro-check-string arg n (add-source arg . args) ...
              (if (pair? opts-rest)
                (let ((arg (car opts-rest)))
                  (if (string? arg)
                    (loop rev-files
                          (cons
                            (cons (string->symbol (keyword->string opt)) arg)
                            file-opts)
                          (cdr opts-rest))
                    (error "Expected string")))
                (error "Missing argument to preload")))

             (else
               (error "Unknown keyword " opt))))
          ((string? opt)
           (loop (cons (cons opt file-opts) rev-files)
                 '()
                 (cdr rest)))
          (else
            (error "Not supported yet " opt))))
      (reverse rev-files))))

(define (compile-project! project)
  (println-log "project: " project)
  (if (project? project)
       (let ((target (buildref-target info))
             (cc-options (options->string (buildref-compile-opts info))))
         (project-intermediate-files-set! project
           (map (lambda (file-and-opt)
                  (let* ((file (car file-and-opt))
                         (file-ext (path-extension file))
                         (prefix (project-prefix project))
                         (opt-linker-name
                           (cond
                             ((assoc 'linker-name (cdr file-and-opt))
                              => cdr)
                             (else (macro-absent-obj)))))

                    (create-directory-tree (path-expand file prefix) (current-build-directory))
                    (let ((targ-file (compile-file-to-target
                                       file
                                       options: `((target ,target))
                                       output: (path-expand
                                                 (path-directory
                                                   file)
                                                 (path-expand
                                                   prefix ;; change dir
                                                   (current-build-directory)))
                                       module-name: opt-linker-name
                                       linker-name: opt-linker-name)))
                      (if targ-file
                        (cons targ-file (cdr file-and-opt))
                        (exit 1)))))

                (project-source-files project)))
         (project-intermediate-obj-files-set! project
           (map (lambda (file-and-opt)
                  (let* ((file (car file-and-opt))
                         (file-ext (path-extension file))
                         #;(prefix (if (eq? project project-object) "src" (project-name project))))
                    (let ((obj-file (compile-file
                                      file
                                      options: `((obj))
                                      output: (path-expand
                                                (path-directory file)
                                                (current-build-directory)
                                                #;(path-expand prefix (current-build-directory)))
                                      cc-options: cc-options)))
                      obj-file)))

                (project-intermediate-files project))))
           (error "Expected a project object.")))


(define (libraries #!rest lst)
  (if (pair? lst)
    (let ((lib (library (car lst))))
      (if lib
        (cons lib (libraries (cdr lst)))
        (libraries (cdr lst))))
    lst))

(define (link project #!key (dependencies #f) (link-with #f) (link-options #f))
  (compile-project! project)
  #;(if (pair? dependencies)
    (for-each
      (lambda (dep)
        (compile-project! dep))
      dependencies))

  ;; Only if main-project
  (if (eq? project (project-global-object))
    (let* ((target (buildref-target info))
           (global-options (buildref-opts info))
           (type (buildref-type info))
           (cc-options (options->string (buildref-compile-opts info)))
           (link-options (if link-with
                           (options->string (cons link-options
                                                  (map (lambda (lib)
                                                         ;; Fetch the link arguments
                                                         (string-append "-l" lib))
                                                       link-with)))
                           link-options)))
      ;; Link should be there
      (case type
        ((dyn)
         #;(error "Build type 'dyn not implemented yet")
         (let* ((intermediate-files (project-intermediate-files project))
                (last-file-opt
                  (list-ref intermediate-files
                            (- (length intermediate-files) 1)))
                (last-file
                  (path-strip-directory
                    (if (pair? last-file-opt)
                      (car last-file-opt)
                      last-file-opt)))
                (link-file
                  (link-flat (project-intermediate-files project)
                             output: (path-expand
                                       (string-append
                                         (path-strip-extension
                                           last-file)
                                         ".o1"
                                         (path-extension last-file))
                                       (current-build-directory))
                             warnings?: #f))
                (link-file-obj
                  (compile-file
                    link-file
                    options: (cons `(target ,target) '((obj)))
                    cc-options: cc-options)))
         (##gambcomp target type (current-build-directory)
          (##append
            (project-intermediate-obj-files project)
            (##list link-file-obj))

          (path-expand
            (path-strip-directory
              (string-append (path-strip-extension last-file) ".o1"))
            (current-build-directory))
          #f (if link-options (##list (##cons "LD_OPTIONS" link-options)) '()))
         (delete-file link-file-obj)
         (delete-file link-file)))

        ((exe)
         (let* ((intermediate-files (project-intermediate-files project))
                (last-file-opt
                  (list-ref intermediate-files
                            (- (length intermediate-files) 1)))
                (last-file
                  (path-strip-directory
                    (if (pair? last-file-opt)
                      (car last-file-opt)
                      last-file-opt)))
                (base-link-options (path-expand
                                     (or
                                       (project-link-base project)
                                       "~~lib/libgambit.a")))
                (link-file
                  (link-incremental (project-intermediate-files project)
                                    output: (current-build-directory)
                                    base: (path-expand (cond
                                                         ((assq 'l (project-options project)) => cadr)
                                                         (else "~~lib/_gambit.c")))
                                    warnings?: #f))
                (link-file-obj
                  (compile-file
                    link-file
                    options: (cons `(target ,target) '((obj)))
                    cc-options: cc-options)))
         (##build-executable
          (##append
            (project-intermediate-obj-files project)
            (##list link-file-obj)) ;; FILES

          `((target ,target)) ;; OPTIONS

          (path-expand
            (path-strip-directory
              (path-strip-extension last-file))
            (current-build-directory)) ;; OUTPUT

          "" ;; Empty CC_OPTIONS

          "" ;; LD_OPTIONS_PRELUDE

          (if link-options
            (string-append base-link-options " " link-options)
            base-link-options)) ;; LD_OPTIONS
         (delete-file link-file-obj)
         (delete-file link-file))
         #;(error "Build type 'exe build not implemented yet"))

        (else
          (error "Build type not supported")))

      ;; Cleanup
      #;(and dependencies
           (for-each
             (lambda (dep)
               (for-each
                 (lambda (file)
                   (delete-file file))
                 (project-intermediate-files dep)))
             dependencies))

      (for-each
        (lambda (file-opts)
          (delete-file (car file-opts)))
        (project-intermediate-files project))

      (for-each
        (lambda (file)
          (delete-file file))
        (project-intermediate-obj-files project))
      (delete-directory (path-expand "src" (current-build-directory))))
    #;(add-linker-options project-object link-options)))

(define intermediate-files '())

;; Use a list.
(define (compile arg #!rest args)
  (println "[deprecated] `compile` will be removed in future version")
  (let ((target (buildref-target info)))
    (let loop ((rev-files '())
               (file-opts '())
               (rest (cons arg args)))
      (if (pair? rest)
        (let ((opt (car rest))
              (opts-rest (cdr rest)))
          (cond
            ((keyword? opt)
             (case opt
               ((preload:)
                (if (pair? opts-rest)
                  (let ((bool (car opts-rest)))
                    (if (boolean? bool)
                      (loop rev-files
                            (cons
                              (cons 'preload bool)
                              file-opts)
                            (cdr opts-rest))
                      (error "Expected boolean")))
                  (error "Missing argument to preload")))
               ((ld-options: cc-options:)
                (if (pair? opts-rest)
                  (let ((arg (car opts-rest)))
                    (if (string? arg)
                      (loop rev-files
                            (cons
                              (cons (string->symbol (keyword->string opt)) arg)
                              file-opts)
                            (cdr opts-rest))
                      (error "Expected string")))
                  (error "Missing argument to preload")))
               (else
                 (error "Unknown keyword " opt))))
            ((string? opt)
             (loop (cons (cons opt file-opts) rev-files)
                   '()
                   (cdr rest)))
            (else
              (error "Not supported yet " opt))))
        (let ((files-lst (reverse rev-files)))
          (set! intermediate-files
            (map (lambda (file-and-opt)
                   (let* ((file (car file-and-opt))
                          (file-ext (path-extension file))
                          ;; Should not be there.
                          (prefix "src"))
                     (create-directory-tree (path-expand file prefix) (current-build-directory))
                     (let ((targ-file (compile-file-to-target
                                        file
                                        options: `((target ,target) (debug))
                                        output: (path-expand
                                                  (path-directory
                                                    file)
                                                  (path-expand
                                                    prefix ;; change dir
                                                    (current-build-directory))))))
                         (cons targ-file (cdr file-and-opt)))))
                 files-lst)))))))

;; Only for dynamic
(define (link-dynamic)
  (println "[deprecated] `link-dynamic` will be removed in future version")
  (let* ((last-file-opt
          (list-ref intermediate-files
                    (- (length intermediate-files) 1)))
         (last-file (path-strip-directory
                      (if (pair? last-file-opt) (car last-file-opt) last-file-opt)))
         (target (buildref-target info))
         (options (cons `(target ,target)
                        (cons '(obj) (buildref-options info)))))
    (let ((link-intermediate
            (link-flat intermediate-files
                       output: (path-expand
                                 (string-append
                                   (path-strip-extension last-file)
                                   ".o1"
                                   (path-extension last-file))
                                 (current-build-directory))
                       warnings?: #f)))
      (##gambcomp target 'dyn #f
       (append
         (map (lambda (file-opt)
                (compile-file (path-expand (if (pair? file-opt) (car file-opt) file-opt))
                              options: options
                              cc-options: "-D___DYNAMIC"))
              intermediate-files)
         (list
           (compile-file
             link-intermediate
             options: options
             cc-options: "-D___DYNAMIC")))
       (path-expand
         (path-strip-directory
           (string-append (path-strip-extension last-file) ".o1"))
         (current-build-directory))
       #f '()))))


;; Main
(define info
  (parse-command-line))

(load (buildref-file info))
