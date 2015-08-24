(define-module (work calendar)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (utils date)
  #:use-module (work holidays)
  #:use-module (work vacation)
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

(define (fill-week week ts hpd is-holiday? is-vacation?)
  (let loop ((rest (caddr week))
             (acc '())
             (workdays 0)
             (holidays 0)
             (vacation-days 0)
             (hours 0))
    (if (null? rest)
        (let ((required (* hpd workdays)))
          (list (cons 'data (reverse acc))
                (cons 'year (car week))
                (cons 'week (cadr week))
                (cons 'hours hours)
                (cons 'required required)
                (cons 'balance (- hours required))
                (cons 'workdays workdays)
                (cons 'vacation-days vacation-days)
                (cons 'holidays holidays)))
        (let* ((today (car rest))
               (type (cond ((is-vacation? today) 'vacation)
                           ((is-holiday? today) 'holiday)
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
                  (+ hours hours-of-day)))))))

(define (add-calendar-stats calendar)
  calendar)

(define (fill-calendar weeks ts hpd is-holiday? is-vacation?)
  (let* ((fill-week* (lambda (x) (fill-week x ts hpd is-holiday? is-vacation?)))
         (filled (map fill-week* weeks)))
    (add-calendar-stats filled)))

(define* (generate-calendar #:key
                            (time-sheet '())
                            (vacation '())
                            (holidays '())
                            (span '())
                            (hours-per-day 8))
  (define is-holiday?
    (if (null? holidays)
        (lambda (x) #f)
        (make-holiday-predicate holidays)))
  (define is-vacation?
    (if (null? vacation)
        (lambda (x) #f)
        (make-vacation-predicate vacation)))
  ;; span->weeks produces something that works nicely as a calendar. The job of
  ;; fill-calendar is to pull out data for each day from time-sheet and work
  ;; out hours per day and all that fun stuff. At the end of a week, work out
  ;; the number of work-days the week had and the difference between required
  ;; and actual numbers.
  (fill-calendar (span->weeks span)
                 time-sheet
                 hours-per-day
                 is-holiday?
                 is-vacation?))
