
(namespace ("build#"))

(##include "~~lib/gambit#.scm")
(##include "~~lib/_gambit#.scm")

;; Maybe at more meta-info
(define-type build-info
  type
  target
  options
  compile-options
  linker-options
  package-file)

;(define (make-project prefix source-files intermediate-files intermediate-obj-files linker-options)
;  (vector prefix source-files intermediate-files intermediate-obj-files linker-options))

(define-type project
  constructor: make-raw-project
  prefix
  source-files ;; With options
  intermediate-files
  intermediate-obj-files
  linker-options)

;; Some global state
(define project-global-object (make-parameter #f))
(define project-global-prefix (make-parameter #f))

(define dependency-list (make-parameter '()))
;; Last dependancy project object
(define dependency-object (make-parameter #f))

(define (make-project source-files #!optional (linker-options #f))
  (let ((p (make-raw-project (or (project-global-prefix) "src")
                             source-files #f #f linker-options)))
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
                        ;; FIXME: Should be a build-info attribute
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
             (type #f)
             (target #f)
             (options '())
             (compile-options '())
             (package-file #f))
    (if (pair? cl)
      (let ((arg (car cl))
            (rest (cdr cl)))
        (cond
          ((string=? arg "-f")
           (if (pair? rest)
             (if package-file
               (error "Parameter -f already specified")
               (loop (cdr rest) type target options compile-options (car rest)))
             (error "Missing argument to -f")))

          ((string=? arg "-debug")
           (loop rest type target (cons '(debug) options) compile-options package-file))

          ((string=? arg "-dynamic")
           (if type
             (error "Build type is already defined")
             (loop rest 'dyn target options (cons "-D___DYNAMIC" compile-options package-file))))

          ((string=? arg "-exe")
           (if type
             (error "Build type is already defined")
             (loop rest 'exe target options compile-options package-file)))

          ((string=? arg "-target")
           (if (pair? rest)
             (if target
               (error "Build target is already defined")
               (let ((new-target (string->symbol (car rest))))
                 (loop (cdr rest) type new-target options compile-options package-file)))
             (error "Missing argument to -target")))

          (else
            (error (string-append "Not a supported argument '" arg "'")))))

      (make-build-info (or type 'dyn)
                       (or target (c#default-target))
                       options
                       (if type ;; Fallback to default
                         compile-options
                         (cons "-D___DYNAMIC" compile-options))
                       #f
                       (or package-file "package.sc")))))

(define build-dir ".builds/") ; this is relative to current project directory

(define info
  (parse-command-line))

(define (set-build-dir! new-build)
  (set! build-dir new-build))

(define (setup)
  (let* ((target (build-info-target info))
         (target-build-folder-name
           (string-append (##system-version-string) "@" (symbol->string target)))
         (cwd (current-directory))
         (relative-output-directory (path-expand
                                      target-build-folder-name build-dir)))
    (set-build-dir! (path-expand relative-output-directory cwd))
    (create-directory-tree relative-output-directory cwd)))


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
       (let ((target (build-info-target info))
             (cc-options (options->string (build-info-compile-options info))))
         (project-intermediate-files-set! project
           (map (lambda (file-and-opt)
                  (let* ((file (car file-and-opt))
                         (file-ext (path-extension file))
                         (prefix (project-prefix project)))

                    (create-directory-tree (path-expand file prefix) build-dir)
                    (let ((targ-file (compile-file-to-target
                                       file
                                       options: `((target ,target))
                                       output: (path-expand
                                                 (path-directory
                                                   file)
                                                 (path-expand
                                                   prefix ;; change dir
                                                   build-dir)))))
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
                                                build-dir
                                                #;(path-expand prefix build-dir))
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
    (let* ((target (build-info-target info))
           (global-options (build-info-options info))
           (type (build-info-type info))
           (cc-options (options->string (build-info-compile-options info)))
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
                                       build-dir)
                             warnings?: #f))
                (link-file-obj
                  (compile-file
                    link-file
                    options: (cons `(target ,target) '((obj)))
                    cc-options: cc-options)))
         (##gambcomp target type build-dir
          (##append
            (project-intermediate-obj-files project)
            (##list link-file-obj))

          (path-expand
            (path-strip-directory
              (string-append (path-strip-extension last-file) ".o1"))
            build-dir)
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
                (base-link-options (string-append "-L" (path-expand "~~lib/libgambit.a")))
                (link-file
                  (link-incremental (project-intermediate-files project)
                             output: (path-expand
                                       (string-append
                                         (path-strip-extension
                                           last-file)
                                         ".exe"
                                         (path-extension last-file))
                                       build-dir)
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
              (string-append (path-strip-extension last-file) ".exe"))
            build-dir) ;; OUTPUT

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
      (delete-directory (path-expand "src" build-dir)))
    #;(add-linker-options project-object link-options)))

(define intermediate-files '())

;; Use a list.
(define (compile arg #!rest args)
  (println "[deprecated] `compile` will be removed in future version")
  (let ((target (build-info-target info)))
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
                     (create-directory-tree (path-expand file prefix) build-dir)
                     (let ((targ-file (compile-file-to-target
                                        file
                                        options: `((target ,target) (debug))
                                        output: (path-expand
                                                  (path-directory
                                                    file)
                                                  (path-expand
                                                    prefix ;; change dir
                                                    build-dir)))))
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
         (target (build-info-target info))
         (options (cons `(target ,target)
                        (cons '(obj) (build-info-options info)))))
    (let ((link-intermediate
            (link-flat intermediate-files
                       output: (path-expand
                                 (string-append
                                   (path-strip-extension last-file)
                                   ".o1"
                                   (path-extension last-file))
                                 build-dir)
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
         build-dir)
       #f '()))))

(load (build-info-package-file info))
