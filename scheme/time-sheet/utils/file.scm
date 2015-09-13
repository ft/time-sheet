;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet utils file)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 optargs)
  #:export (match-files
            read-file))

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
