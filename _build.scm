
(namespace ("build#"))

(##include "~~lib/gambit#.scm")
(##include "~~lib/_gambit#.scm")

;; Maybe at more meta-info
(define-type build-info
  type
  target
  options)

(define-type project
  constructor: make-raw-project
  prefix
  source-files ;; With options
  intermediate-files
  intermediate-obj-files
  linker-options)

;; Some global state
(define project-object (make-parameter #f))
(define dependency-list (make-parameter '()))
;; Last dependancy project object
(define dependency-object (make-parameter #f))

(define (make-project! name source-files #!optional (linker-options #f))
  (let ((p (make-raw-project name source-files #f #f linker-options)))
    ;; Dependancy if not main project.
    (if (project-object)
      (dependency-object p)
      (project-object p))
    p))

;; name: fullname of the library (i.e. github.com/feeley/crypto)
(define (library name)
  (if (project-object)
    (and
      (not (member name (dependency-list)))
      (let ((fullpath (path-expand
                        ;; Should allow the build script to be rename
                        ;; from the command line.
                        (path-expand "package.sc" name)
                        (or (getenv "R7RS_LIBRARY_PATH" #f)
                            (##os-path-gambitdir-map-lookup "R7RS")
                            "~/.r7rs"))))
        (if (file-exists? fullpath)
          ;; Resolve lib
          (parameterize
            ;; Clone current build ctx
            (#;(project-object (project-object))
             (dependency-object #f))
            (and (not (member name (dependency-list)))
                 (begin
                   (load fullpath)
                   (dependency-list (cons name (dependency-list)))))
            (project-prefix-set! (dependency-object) name)
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

;(create-directory-tree "a/b/c/" (current-directory))

(define (parse-command-line)
  (let loop ((cl (cdr (command-line)))
             (type #f)
             (target #f)
             (options '()))
    (if (pair? cl)
      (let ((arg (car cl))
            (rest (cdr cl)))
        (cond
          ((string=? arg "-debug")
           (loop rest type target (cons '(debug) options)))
          ((string=? arg "-dynamic")
           (if type
             (error "Build type is already defined")
             (loop rest 'dyn target options)))

          ((string=? arg "-exe")
           (if type
             (error "Build type is already defined")
             (loop rest 'exe target options)))

          ((string=? arg "-target")
           (if (pair? rest)
             (if target
               (error "Build target is already defined")
               (let ((new-target (string->symbol (car rest))))
                 (loop (cdr rest) type new-target options)))
             (error "Missing argument to -target")))
          (else
            (error (string-append "Not a supported argument '" arg "'")))))
      (make-build-info (or type 'dyn)
                       (or target (c#default-target)) options))))

(define build-dir ".builds/")

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
  (println "project: " project)
  (and (project? project)
       (let ((target (build-info-target info)))
         (project-intermediate-files-set! project
           (map (lambda (file-and-opt)
                  (let* ((file (car file-and-opt))
                         (file-ext (path-extension file))
                         ;; Should not be there.
                         (prefix (if (eq? project (project-object))
                                   "src"
                                   ;; FIXME: project-name is not a good thing
                                   (project-prefix project))))

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
                      (cons targ-file (cdr file-and-opt)))))

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
                                      cc-options: "-D___DYNAMIC")))
                      obj-file)))

                (project-intermediate-files project))))))


(define (libraries #!rest lst)
  (if (pair? lst)
    (let ((lib (library (car lst))))
      (if lib
        (cons lib (libraries (cdr lst)))
        (libraries (cdr lst))))
    lst))

(define (link project #!key (dependencies #f) (link-options #f))
  (compile-project! project)
  #;(if (pair? dependencies)
    (for-each
      (lambda (dep)
        (compile-project! dep))
      dependencies))

  ;; Only if main-project
  (if (eq? project (project-object))
    (let* ((target (build-info-target info))
           (global-options (build-info-options info)))
      ;; Link should be there
      (case (or (build-info-type info) 'dyn)
        ((dyn)
         (println "[Notices] Build type 'dyn not completed yet")
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
                    options: (cons '(target C) '((obj)))
                    cc-options: "-D___DYNAMIC")))
         ; FIXME: change 'C with build-target
         (println project)
         (##gambcomp 'C 'dyn build-dir
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
         (error "Build type 'exe build not implemented yet"))

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
  (let ((target (or (build-info-target info) (c#default-target))))
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
                   (let* (#;(has-opt? (pair? file-and-opt))
                          (file (car file-and-opt)))
                     (create-directory-tree file build-dir)
                     (let ((targ-file (compile-file-to-target
                                        file
                                        options: `((target ,target))
                                        output: (path-expand
                                                  (path-directory
                                                    file)
                                                  build-dir))))
                         (cons targ-file (cdr file-and-opt)))))
                 files-lst)))))))

;; Only for dynamic
(define (link-dynamic)
  (let* ((last-file-opt
          (list-ref intermediate-files
                    (- (length intermediate-files) 1)))
         (last-file (if (pair? last-file-opt) (car last-file-opt) last-file-opt))
         (target (or (build-info-target info) (c#default-target))))
    (let ((link-intermediate
            (link-flat intermediate-files
                       output: (path-expand
                                 (string-append
                                   (path-strip-extension last-file)
                                   ".o1"
                                   (path-extension last-file)))
                       warnings?: #f)))
      (##gambcomp target 'dyn #f
       (append
         (map (lambda (file-opt)
                (compile-file (path-expand (if (pair? file-opt) (car file-opt) file-opt))
                              options: (cons `(target ,target) '((obj)))
                              cc-options: "-D___DYNAMIC"))
              intermediate-files)
         (list
           (compile-file
             link-intermediate
             options: (cons `(target ,target) '((obj)))
             cc-options: "-D___DYNAMIC")))
       (string-append (path-strip-extension last-file) ".o1")
       #f '()))))
