(use-modules (srfi srfi-1)
             (ice-9 pretty-print)
             (time-sheet holidays)
             (time-sheet utils date))

(define *args* (cdr (command-line)))
(define *argn* (length *args*))

(define *year* 0)
(define *start* '(12 24))
(define *end* '(12 31))

(define (set-year! y)
  (set! *year* y)
  (set! *start* (cons y *start*))
  (set! *end*   (cons y *end*)))

(define (date-ish? str)
  (and (string? str)
       (let ((lst (string-split str #\-)))
         (and (= 3 (length lst))
              (every string->number lst)))))

(define (date->string date)
  (format #f "~a-~a-~a" (first date) (second date) (third date)))

(define (string->date str)
  (map string->number (string-split str #\-)))

(cond ((= *argn* 0) (set-year! (car (today))))
      ((= *argn* 1) (set-year! (or (string->number (car *args*))
                                   (car (today)))))
      ((and (= *argn* 2)
            (every date-ish? *args*))
       (begin (set! *start* (string->date (first *args*)))
              (set! *end* (string->date (second *args*)))))
      (else (begin (format #t "Unknown argument(s): ~a~%" *args*)
                   (quit 1))))

(define (bool->int b)
  (if b 1 0))

(define (date? obj)
  (and (list? obj)
       (= 3 (length obj))
       (every integer? obj)))

(define (do-i-need-vacation date)
  (let ((holiday (is-holiday? date)))
    (if holiday
        (cons 'holiday holiday)
        (let ((weekend (is-week-end? date)))
          (if weekend
              (cons* 'weekend (date->day date) date)
              date)))))

(define (decorate obj)
  (if (date? obj)
      (cons (date->day obj) obj)
      obj))

(let* ((span (date-span *start* *end*))
       (maybe-vacation (map do-i-need-vacation span))
       (days (fold + 0 (map (compose bool->int date?) maybe-vacation))))
  (format #t "Time-Table:~%~%")
  (pretty-print (map decorate maybe-vacation) #:per-line-prefix "  ")
  (format #t "~%You are going to need ~a vacation days for ~a to ~a.~%"
          days (date->string *start*) (date->string *end*)))
