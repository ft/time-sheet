;; Copyright (c) 2015-2017 time-sheet workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet vacation)
  #:use-module (time-sheet utils date)
  #:use-module (time-sheet holidays)
  #:export (make-vacation-predicate
            vacation))

(define (make-vacation-predicate lst)
  (lambda (day) (not (not (member day lst)))))

(define (filter-free-time lst)
  (filter (lambda (x)
            (not (or (is-holiday? x)
                     (is-week-end? x))))
          lst))

;; Example:
;;
;; (vacation '(2015 03 02)
;;           #:span '(2015 03 12) '(2015 04 27))
;;
;; This will return a list of days, that are neither holidays nor weekends that
;; lie within the specified days and spans of days.

(define (vacation . lst)
  (filter-free-time
   (let loop ((rest lst) (acc '()))
     (cond ((null? rest) (reverse acc))
           ((list? (car rest))
            (loop (cdr rest) (cons (car rest) acc)))
           ((eq? (car rest) #:span)
            (cond ((< (length rest) 3)
                   (throw 'vacation-span-not-enough-arguments lst rest))
                  ((not (and (list? (cadr rest))
                             (list? (caddr rest))))
                   (throw 'vacation-span-broken-arguments lst rest))
                  (else (loop (cdddr rest)
                              (append (reverse (date-span (cadr rest)
                                                          (caddr rest)))
                                      acc)))))
           (else (throw 'vacation-span-broken-arguments lst rest))))))
