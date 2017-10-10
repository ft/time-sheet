;; Copyright (c) 2015-2017 time-sheet workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet utils file)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 optargs)
  #:export (is-csv?
            is-file-type?
            is-json?
            match-files
            read-file
            use-file-or-port))

(define (read-file file)
  (with-input-from-file file
    (lambda () (read (current-input-port)))))

(define (scandir* dir rx?)
  (or (scandir dir rx? (lambda (x y) #t))
      '()))

(define (with-dir d f)
  (string-concatenate (list d file-name-separator-string f)))

(define* (match-files #:key
                      (directory ".")
                      (directories '())
                      (regexp "."))
  (define rx (make-regexp regexp regexp/extended))
  (define (regex-matches? x) (regexp-exec rx x))
  (let loop ((rest (if (null? directories)
                       (list directory)
                       directories))
             (acc '()))
    (cond ((null? rest) acc)
          (else (loop (cdr rest)
                      (let ((dir (car rest)))
                        (append acc (map (lambda (x)
                                           (with-dir dir x))
                                         (scandir* dir regex-matches?)))))))))

(define (use-file-or-port file-or-port thunk)
  ((cond ((string? file-or-port) with-input-from-file)
         ((port? file-or-port) with-input-from-port)
         (else (throw 'unknown-argument-type file-or-port)))
   file-or-port
   thunk))

(define (is-file-type? file-name ext)
  (let ((strlen (string-length file-name))
        (extlen (string-length ext)))
    (and (> strlen extlen)
         (string-ci=? ext (substring file-name (- strlen extlen))))))

(define (is-json? file)
  (is-file-type? file ".json"))

(define (is-csv? file)
  (is-file-type? file ".csv"))
