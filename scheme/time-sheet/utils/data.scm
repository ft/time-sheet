;; Copyright (c) 2016 time-sheet workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet utils data)
  #:export (hash-map->alist))

(define (maybe-hm-recurse x)
  (cond ((hash-table? x) (hash-map->alist x))
        ((list? x) (map hash-map->alist x))
        ((pair? x) (cons (hash-map->alist (car x))
                         (hash-map->alist (cdr x))))
        (else x)))

(define (hash-map->alist x)
  (if (not (hash-table? x))
      x
      (hash-map->list (lambda (a b)
                        (cons a (maybe-hm-recurse b)))
                      x)))
