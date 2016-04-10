;; Copyright (c) 2016 time-sheet workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet utils data)
  #:export (hash-map->alist
            id<
            sort-by-id
            string->date))

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
                        (cons (string->symbol a) (maybe-hm-recurse b)))
                      x)))

(define (id< a b)
  "With two alists A and B, determine whether or not the `id' entry of A is
numerically less than the one of B."
  (< (assq-ref a 'id) (assq-ref b 'id)))

(define (sort-by-id lst)
  "Take a list of alists LST and sort it in terms of the `id' entries of those
alists."
  (sort lst id<))

(define (string->date str)
  "Split date string into an annotated (time-sheet utils date) expression."
  (let ((data (string-split str #\-)))
    (list (cons 'year (string->number (car data)))
          (cons 'month (string->number (cadr data)))
          (cons 'day (string->number (caddr data))))))
