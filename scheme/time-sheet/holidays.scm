;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet holidays)
  #:use-module (srfi srfi-1)
  #:use-module (time-sheet utils date)
  #:export (local-holidays-for
            is-holiday?
            only-weekdays))

(define cache '())
(define cached-years '())

(define (is-holiday? day)
  (let ((year (car day)))
    (unless (member year cached-years)
      (set! cache
        (append cache
                (local-holidays-for year)))
      (set! cached-years
        (cons year cached-years)))
    (let loop ((rest cache))
      (cond ((null? rest) #f)
            ((equal? day (cdar rest)) (car rest))
            (else (loop (cdr rest)))))))

(define (local-holidays-for year)
  (let ((easter-day (easter-date year)))
    `(("Neujahr" ,year 1 1)
      ("Karfreitag" . ,(date- easter-day 2))
      ("Ostersonntag" . ,easter-day)
      ("Ostermontag" . ,(date+ easter-day 1))
      ("Tag der Arbeit" ,year 5 1)
      ("Christi Himmelfahrt" . ,(date+ easter-day 39))
      ("Pfingstsonntag" . ,(date+ easter-day 49))
      ("Pfingstmontag" . ,(date+ easter-day 50))
      ("Fronleichnam" . ,(date+ easter-day 60))
      ("Tag der deutschen Einheit" ,year 10 3)
      ("Allerheiligen" ,year 11 1)
      ("1.Weihnachtstag" ,year 12 25)
      ("2.Weihnachtstag" ,year 12 26))))

(define (only-weekdays lst)
  (filter (lambda (x) (is-week-day? (cdr x)))
          lst))
