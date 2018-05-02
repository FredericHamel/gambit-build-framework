
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
  (let* ((target (or (build-info-target info) 'C))
         (target-build-folder-name
           (string-append (##system-version-string) "@" (symbol->string target)))
         (cwd (current-directory))
         (relative-output-directory (path-expand
                                      target-build-folder-name build-dir)))
    (set! build-dir (path-expand relative-output-directory cwd))
    (create-directory-tree relative-output-directory cwd)))

(define intermediate-files '())

;; Use a list.
(define (compile file)
  (let ((target (or (build-info-target info) 'C)))
    (create-directory-tree file build-dir)
    (set! intermediate-files
      (cons (compile-file-to-target file options: (list (list 'target target))
                            output: (path-expand
                                      (path-directory
                                        file)
                                      build-dir))
            intermediate-files))

    #;(compile-file file
        options: opts
        output: outdir)))

;; Only fo dynamic 
(define (link-dynamic)
  (let ((last-file
          (list-ref intermediate-files
                    (- (length intermediate-files) 1)))
        (target (or (build-info-target info) 'C)))
    (let ((link-intermediate
            (link-flat (map (lambda (x) (cons x '((preload . #f))))
                            intermediate-files)
                       output: (path-expand
                                 (string-append
                                   (path-strip-extension last-file)
                                   ".o1"
                                   (path-extension last-file))))))
    (##gambcomp target 'dyn #f
        (append
          (map (lambda (file)
                 (println file)
                 (compile-file (path-expand file)
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
