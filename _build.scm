
(namespace ("build#"))

(##include "~~lib/gambit#.scm")
(##include "~~lib/_gambit#.scm")

;; Maybe at more meta-info
(define-type build-info
  type
  target
  other)

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

;(create-directory-tree "a/b/c/" (current-directory))

(define (parse-command-line)
  (let loop ((cl (cdr (command-line)))
             (type #f)
             (target #f)
             (other '()))
    (if (pair? cl)
      (let ((arg (car cl))
            (rest (cdr cl)))
        (cond
          ((string=? arg "-dynamic")
           (loop rest 'dyn target other))

          ((string=? arg "-exe")
           (loop rest 'exe target other))

          ((string=? arg "-target")
           (if (pair? rest)
             (let ((new-target (string->symbol (car rest))))
               (loop (cdr rest) type new-target other))
             (error "Missing argument to -target")))
          (else
            (error "Not implemented"))))
      (make-build-info type target other))))

(define build-dir ".builds/")

(define info
  (parse-command-line))

(define (set-build-dir! new-build)
  (set! build-dir new-build))

(define (setup)
  (let* ((target (or (build-info-target info) (c#default-target)))
         (target-build-folder-name
           (string-append (##system-version-string) "@" (symbol->string target)))
         (cwd (current-directory))
         (relative-output-directory (path-expand
                                      target-build-folder-name build-dir)))
    (set! build-dir (path-expand relative-output-directory cwd))
    (create-directory-tree relative-output-directory cwd)))

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
