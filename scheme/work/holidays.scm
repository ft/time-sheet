;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.

(define-module (work holidays)
  #:use-module (srfi srfi-1)
  #:use-module (utils date)
  #:export (local-holidays-for
            make-holiday-predicate
            only-weekdays))

(define (make-holiday-predicate lst)
  (let ((day-lst (map cdr lst)))
    (lambda (day) (not (not (member day day-lst))))))

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
