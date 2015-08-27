;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.

(define-module (work csv-import)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 control)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  #:use-module (utils date)
  #:export (csv->scm
            csv->s-exp
            read-timesheet
            timesheet->alist
            timesheet->sorted-alist))

;; CSV reader code taken from guile-csv
;; Copyright (C) 2008, 2012, 2013
;; Andy Wingo <wingo at pobox dot com>
;; Nala Ginrut <nalaginrut@gmail.com>
;; LGPL3+

(define (csv-read-row port delimiter have-cell init-seed)
  (define (!)
    (let ((c (read-char port)))
      c))
  (define (finish-cell b seed)
    (have-cell (list->string (reverse b)) seed))
  (define (next-cell b seed)
    (state-init (!) (finish-cell b seed)))
  (define (state-init c seed)
    (cond ((eqv? c delimiter) (state-init (!) (have-cell "" seed)))
          ((eqv? c #\") (state-string (!) '() seed))
          ((eqv? c #\newline) seed)
          ((eof-object? c) seed)
          (else (state-any c '() seed))))
  (define (state-string c b seed)
    (cond ((eqv? c #\") (state-string-quote (!) b seed))
          ((eof-object? c) (error "Open double-quoted string"
                                  (list->string (reverse b))))
          (else (state-string (!) (cons c b) seed))))
  (define (state-string-quote c b seed)
    (cond ((eqv? c #\") (state-string (!) (cons c b) seed))
          ((eqv? c delimiter) (next-cell b seed))
          ((eqv? c #\newline) (finish-cell b seed))
          ((eof-object? c) (finish-cell b seed))
          (else (error "Single double quote at unexpected place." c b))))
  (define (state-any c b seed)
    (cond ((eqv? c delimiter) (next-cell b seed))
          ((eqv? c #\newline) (finish-cell b seed))
          ((eof-object? c) (finish-cell b seed))
          (else (state-any (!) (cons c b) seed))))
  (state-init (!) init-seed))

(define (csv-read port delimiter new-row have-cell have-row init-seed)
  (let lp ((seed init-seed))
    (cond
     ((eof-object? (peek-char port)) seed)
     (else (lp (have-row (csv-read-row port delimiter have-cell (new-row seed))
                         seed))))))

(define* (make-csv-reader delimiter #:key
                          (new-row (lambda (rows) '()))
                          (have-cell (lambda (cell row)
                                       (cons cell row)))
                          (have-row (lambda (row rows)
                                      (cons (list->vector (reverse row)) rows)))
                          (init-seed '()))
  (lambda (port)
    (reverse
     (csv-read port delimiter new-row have-cell have-row init-seed))))

;; “zip2” is from the chip-remote project.
(define (zip2 la lb)
  "This is like `zip' from (srfi srfi-1), except that it returns a list of
pairs instead of a list of lists with two elements in them.

  (zip2 '(a c e) '(b d f)) => ((a . b) (c . d) (e . f))"
  (let next ((a la)
             (b lb)
             (acc '()))
    (cond ((any null? (list a b))
           (reverse acc))
          (else
           (next (cdr a) (cdr b)
                 (cons (cons (car a) (car b))
                       acc))))))

(define read-timesheet
  (make-csv-reader #\, #:have-row (lambda (r rs) (cons (reverse r) rs))))

(define keys
  '(("#" . id)
    ("Datum" . date)
    ("Mitglied" . person)
    ("Aktivität" . activity)
    ("Projekt" . project)
    ("Ticket" . ticket-id)
    ("Ticket Thema" . ticket-subject)
    ("Kommentar" . comment)
    ("Stunden" . time)))

(define (translate-key k)
  (call/ec (lambda (return)
             (fold (lambda (e default)
                     (if (string-ci= k (car e))
                         (return (cdr e))
                         default))
                   k
                   keys))))

(define (string->time str)
  (inexact->exact (string->number str)))

(define (string->date str)
  (let ((data (string-split str #\-)))
    (list (cons 'year (string->number (car data)))
          (cons 'month (string->number (cadr data)))
          (cons 'day (string->number (caddr data))))))

(define (string->ticket-id str)
  (let ((lst (string-split str #\space)))
    (if (= (length lst) 2)
        (string->number (substring (cadr lst) 1))
        #f)))

(define filters
  `((time . ,string->time)
    (date . ,string->date)
    (id . ,string->number)
    (ticket-id . ,string->ticket-id)))

(define (maybe-filter what)
  (let* ((key (car what))
         (value (cdr what))
         (f (assq key filters)))
    (if f
        (cons key ((cdr f) value))
        what)))

(define (id< a b)
  (< (assq-ref a 'id) (assq-ref b 'id)))

(define (sort-by-id lst)
  (sort lst id<))

(define (make-zipper data)
  (let ((names (map translate-key data)))
    (lambda (e) (map maybe-filter (zip2 names e)))))

(define (timesheet->alist data)
  (map (make-zipper (car data)) (cdr data)))

(define (timesheet->sorted-alist data)
  (sort-by-id (timesheet->alist data)))

(define* (csv->scm file-or-port #:key (sort? #t))
  ((if sort? timesheet->sorted-alist timesheet->alist)
   ((cond ((string? file-or-port) with-input-from-file)
          ((port? file-or-port) with-input-from-port)
          (else (throw 'unknown-argument-type file-or-port)))
    file-or-port
    (lambda () (read-timesheet (current-input-port))))))

(define* (csv->s-exp file-or-port #:key (sort? #t))
  (pretty-print (csv->scm file-or-port #:sort? sort?)))
