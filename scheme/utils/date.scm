;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.

;; This library deals with questions one might have about dates. A date in this
;; module's sense is a list of three items: (year month day)
;;
;; Questions this module answers:
;;
;;   - Is a given year a leap-year?
;;   - Does this date fall on a week-day or rather the week end?
;;   - When is easter sunday in a given year?
;;   - If I add N days to a date, on which date do I end up on?
;;   - If I subtract N days from a date, on which date do I end up on?
;;
;; The module supports conversion to and from an annotated date representation:
;;
;;  ((year . 1234) (month . 12) (day . 3))
;;
;; The functions working with a date object do require the non-annotated
;; version, though.

(define-module (utils date)
  #:use-module (srfi srfi-1)
  #:export (is-leap-year?
            is-week-day?
            is-week-end?
            alist->date
            date->alist
            date->day
            date->week
            date+
            date-
            date-span
            day-of-year
            date+woy->year
            easter-date))

(define (date->alist d)
  "Convert a non-annotated date to an annotated one.

Example: (date->alist '(2015 3 27))
         => ((year . 2015) (month . 3) (day 27))"
  (list (cons 'year (car d))
        (cons 'month (cadr d))
        (cons 'day (caddr d))))

(define (alist->date alist)
  "Convert an annotated date to a non-annotated one.

Example: (alist-date '((year . 2015) (month . 3) (day 27)))
         => (2015 3 27)"
  (let ((year (assq-ref alist 'year))
        (month (assq-ref alist 'month))
        (day (assq-ref alist 'day)))
    (list year month day)))

(define-inlinable (is-divisable-by? what by)
  (zero? (modulo what by)))

(define (is-leap-year? year)
  "Check if YEAR is a leap-year."
  (or (is-divisable-by? year 400)
      (and (is-divisable-by? year 4)
           (not (is-divisable-by? year 100)))))

(define (easter-date year)
  "Return the day of easter sunday for a given YEAR in the gregorian calendar."
  (let* ((a (modulo year 19))
         (b (quotient year 100))
         (c (modulo (+ (- b
                          (quotient b 4)
                          (quotient (* b 8)
                                    25))
                       (* 19 a)
                       15)
                    30))
         (d (- c (* (quotient c 28)
                    (- 1 (* (quotient c 28)
                            (quotient 29
                                      (+ c 1))
                            (quotient (- 21 a)
                                      11))))))
         (e (modulo (- (+ year
                          (quotient year 4)
                          d
                          2
                          (quotient b 4))
                       b)
                    7))
         (f (- d e))
         (month (+ 3 (quotient (+ f 40)
                               44)))
         (day (- (+ f 28)
                 (* 31 (quotient month 4)))))
    (list year month day)))

(define *week-days*
  #(sunday monday tuesday wednesday thursday friday saturday))

(define (date->day date)
  (vector-ref *week-days* (week-day date)))

(define (week-day date)
  (let* ((year (car date))
         (month (cadr date))
         (day (caddr date))
         (a (quotient (- 14 month) 12))
         (y (- year a))
         (m (+ month (* 12 a) -2)))
    (modulo (+ day
               y
               (quotient y 4)
               (- (quotient y 100))
               (quotient y 400)
               (quotient (* 31 m)
                         12))
            7)))

(define (week-day* date)
  (let ((dow (week-day date)))
    (if (zero? dow) 7 dow)))

(define (is-week-end? d)
  "Determine whether D is either a saturday or a sunday."
  (let ((dow (week-day d)))
    (or (= dow 0)
        (= dow 6))))

(define (is-week-day? d)
  "Determine whether D is neither a saturday nor a sunday."
  (not (is-week-end? d)))

(define (days-per-month year month)
  (define dpm #(31 0 31 30 31 30 31 31 30 31 30 31))
  (cond ((= month 2) (if (is-leap-year? year) 29 28))
        ((or (< month 1) (> month 12))
         (throw 'invalid-month month))
        (else (vector-ref dpm (- month 1)))))

(define (day-of-year date)
  "Return the index of DATE within its year.

Indexing starts at 1. Example:

  (day-of-year '(2015 02 01)) => 32"
  (let loop ((cur (- (cadr date) 1)) (acc 0))
    (cond ((< cur 0) (throw 'month-is-zero-or-negative date))
          ((zero? cur) (+ acc (caddr date)))
          (else (loop (- cur 1) (+ acc (days-per-month (car date) cur)))))))

(define (date->week date)
  "Returns the ISO week that DATE falls into.

ISO describes the first week of the year as the first week that has at least
four days in the new year. That means, if the first day of a year is
Thursday or earlier, that day is part of the first week of the year.

If the first day of the year is a Friday or later, those first few days are
part of the last week of the last week of the past year.

Examples:

  (date->week '(2008 12 30)) => 1
  (date->week '(2007 01 01)) => 1
  (date->week '(2006 01 01)) => 52
  (date->week '(2005 01 01)) => 53"
  (let* ((doy (day-of-year date))
         (first (week-day* (list (car date) 1 1)))
         (week-division (lambda (o) (+ o (int/ (+ (- first 9) doy) 7))))
         (week (cond ((< first 5)
                      (week-division 2))
                     ((<= doy (- 8 first))
                      (date->week (list (- (car date) 1) 12 31)))
                     (else (week-division 1)))))
    (if (not (and (= (cadr date) 12)
                  (> (caddr date) 28)))
        week
        (let ((days-in-last-week (week-day* (list (car date) 12 31))))
          (if (and (< days-in-last-week 4)
                   (< (- 31 (caddr date)) days-in-last-week))
              1
              week)))))

(define (date+woy->year date woy)
  "Considering DATE and ISO WEEK-OF-YEAR (WOY), produre a year.

Such that the year matches the WEEK-OF-YEAR."
  (cond ((and (= 1 (cadr date))
              (> woy 50))
         (- (car date) 1))
        ((and (= 12 (cadr date))
              (= 1 woy))
         (+ (car date) 1))
        (else (car date))))

(define (days-left-in-month date)
  (let ((dpm (days-per-month (car date) (cadr date))))
    (- dpm (caddr date))))

(define (int/ a b)
  (inexact->exact (floor (/ a b))))

(define (date+ date n)
  "Add N days to DATE."
  (if (< n 0)
      (date- date n)
      (let loop ((n n)
                 (year (car date))
                 (month (cadr date))
                 (day (caddr date))
                 (days-left (days-left-in-month date)))
        (cond ((= n 0) (list year month day))
              ((<= n days-left) (list year month (+ day n)))
              (else (let ((new-month (if (= month 12) 1 (+ month 1)))
                          (new-year (if (= month 12) (+ year 1) year)))
                      (loop (- n days-left)
                            new-year
                            new-month
                            0
                            (days-per-month new-year new-month))))))))

(define (date- date n)
  "Substract N days from DATE."
  (if (< n 0)
      (date+ date n)
      (let loop ((n n)
                 (year (car date))
                 (month (cadr date))
                 (day (caddr date)))
        (cond ((= n 0) (list year month day))
              ((< n day) (list year month (- day n)))
              (else (let* ((new-month (if (= month 1) 12 (- month 1)))
                           (new-year (if (= month 1) (- year 1) year))
                           (new-day (days-per-month new-year new-month)))
                      (loop (- n day) new-year new-month new-day)))))))

(define (date-span start stop)
  (let loop ((cur stop) (acc '()))
    (cond ((equal? cur start)
           (cons start acc))
          (else (loop (date- cur 1) (cons (list-copy cur) acc))))))
