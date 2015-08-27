;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet calendar)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (time-sheet utils date)
  #:use-module (time-sheet holidays)
  #:use-module (time-sheet vacation)
  #:export (generate-calendar))

(define (span->weeks span)
  "Take a span of days and produce a list of weeks.

Currently, SPAN is expected to be sorted in chronological order."
  (if (null? span)
      '()
      (let loop ((rest span)
                 (weeks '())
                 (year* (caar span))
                 (woy* (date->week (car span)))
                 (this-week '()))
        (if (null? rest)
            (if (null? this-week)
                (reverse weeks)
                (reverse (cons (list year* woy* (reverse this-week))
                               weeks)))
            (let ((year (caar rest))
                  (woy (date->week (car rest))))
              (if (not (and (= year* year)
                            (= woy* woy)))
                  (loop (cdr rest)
                        (cons (list year* woy* (reverse this-week))
                              weeks)
                        year woy
                        (list (car rest)))
                  (loop (cdr rest)
                        weeks year woy
                        (cons (car rest) this-week))))))))

(define (hours-of-data data)
  (apply + (map (lambda (x) (assq-ref x 'time)) data)))

(define (filter-day day data)
  (filter (lambda (x)
            (equal? (alist->date (assq-ref x 'date))
                    day))
          data))

(define-syntax-rule (increment-if-type (tv type) var)
  (if (eq? tv type)
      (+ 1 var)
      var))

(define (fill-week week ts hpd
                   is-vacation?
                   is-compensatory?
                   is-extra-leave?)
  (let loop ((rest (caddr week))
             (acc '())
             (workdays 0)
             (holidays 0)
             (vacation-days 0)
             (compensatory-days 0)
             (extra-leave-days 0)
             (weekend-days 0)
             (hours 0))
    (if (null? rest)
        (let ((required (* hpd (+ compensatory-days workdays))))
          (list (cons 'data (reverse acc))
                (cons 'year (car week))
                (cons 'week (cadr week))
                (cons 'hours hours)
                (cons 'required required)
                (cons 'balance (- hours required))
                (cons 'workdays workdays)
                (cons 'vacation-days vacation-days)
                (cons 'compensatory-days compensatory-days)
                (cons 'extra-leave-days extra-leave-days)
                (cons 'holidays holidays)
                (cons 'weekend-days weekend-days)))
        (let* ((today (car rest))
               (type (cond ((is-vacation? today) 'vacation)
                           ((is-holiday? today) 'holiday)
                           ((is-compensatory? today) 'compensatory)
                           ((is-extra-leave? today) 'extra-leave)
                           ((is-week-end? today) 'weekend)
                           (else 'workday)))
               (data (filter-day today ts)))
          (let ((hours-of-day (hours-of-data data)))
            (loop (cdr rest)
                  (cons (list today
                              (list (date->day today) type hours-of-day)
                              data)
                        acc)
                  (increment-if-type (type 'workday) workdays)
                  (increment-if-type (type 'holiday) holidays)
                  (increment-if-type (type 'vacation) vacation-days)
                  (increment-if-type (type 'compensatory) compensatory-days)
                  (increment-if-type (type 'extra-leave) extra-leave-days)
                  (increment-if-type (type 'weekend) weekend-days)
                  (+ hours hours-of-day)))))))

(define (add-calendar-stats calendar)
  calendar)

(define (fill-calendar weeks ts hpd
                       is-vacation?
                       is-compensatory?
                       is-extra-leave?)
  (let* ((fill-week* (lambda (x) (fill-week x ts hpd
                                            is-vacation?
                                            is-compensatory?
                                            is-extra-leave?)))
         (filled (map fill-week* weeks)))
    (add-calendar-stats filled)))

(define* (generate-calendar #:key
                            (time-sheet '())
                            (vacation '())
                            (compensatory '())
                            (extra-leave '())
                            (holidays '())
                            (span '())
                            (hours-per-day 8))
  (define is-vacation?
    (if (null? vacation)
        (lambda (x) #f)
        (make-vacation-predicate vacation)))
  (define is-compensatory?
    (if (null? compensatory)
        (lambda (x) #f)
        (make-vacation-predicate compensatory)))
  (define is-extra-leave?
    (if (null? extra-leave)
        (lambda (x) #f)
        (make-vacation-predicate extra-leave)))
  ;; span->weeks produces something that works nicely as a calendar. The job of
  ;; fill-calendar is to pull out data for each day from time-sheet and work
  ;; out hours per day and all that fun stuff. At the end of a week, work out
  ;; the number of work-days the week had and the difference between required
  ;; and actual numbers.
  (fill-calendar (span->weeks span)
                 time-sheet
                 hours-per-day
                 is-vacation?
                 is-compensatory?
                 is-extra-leave?))
