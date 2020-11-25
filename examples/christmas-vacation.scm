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

(cond ((= *argn* 0) (set-year! (car (today))))
      ((= *argn* 1) (set-year! (or (string->number (car *args*))
                                   (car (today)))))
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

(let* ((span (date-span *start* *end*))
       (maybe-vacation (map do-i-need-vacation span))
       (days (fold + 0 (map (compose bool->int date?) maybe-vacation))))
  (format #t "Time-Table:~%~%")
  (pretty-print maybe-vacation #:per-line-prefix "  ")
  (format #t "~%You are going to need ~a days of christmas vacation for ~a.~%"
          days *year*))
