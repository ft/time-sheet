;; Copyright (c) 2015 time-sheet workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

;; latex.scm: LaTeX rendering module for time-sheets.
;;
;; The core of this module are two functions:
;;
;;  - latex-document
;;  - table-from
;;
;; ‘latex-document’ generates a LaTeX skeleton document. To fill the document
;; with data, you need to supply a #:content argument. This is where the second
;; function ‘table-from’ comes into play.
;;
;; An example call of a combination of the two may look like this:
;;
;;    (latex-document #:content
;;                    (lambda ()
;;                      (table-from (generate-calendar ...))))
;;
;; The rest of the code is made of by utilities and configuration helpers.

(define-module (work render latex)
  #:use-module (utils date)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:export (pretty-alignment
            pretty-days
            pretty-items
            pretty-months
            latex-document
            table-from))

(eval-when (expand load eval)
  (define (maybe-convert string-or-symbol)
    (cond ((string? string-or-symbol) string-or-symbol)
          ((not string-or-symbol) "*sym-is-false*")
          (else (symbol->string string-or-symbol))))

  (define (strcat . lst)
    (string-concatenate lst)))

(define-syntax options
  (lambda (x)
    (define (produce-options kw lst)
      (datum->syntax
       kw
       (map (lambda (x)
              (cond ((symbol? x) (symbol->string x))
                    ((and (list? x)
                          (= (length x) 2))
                     (strcat (maybe-convert (car x))
                             "="
                             (maybe-convert (cadr x))))
                    (else (maybe-convert (car x)))))
            (syntax->datum lst))))
    (syntax-case x ()
      ((kw opts ...)
       (with-syntax (((str ...) (produce-options #'kw #'(opts ...))))
         #'(list str ...))))))

(define (display-to port . lst)
  (for-each (lambda (x) (display x port)) lst))

(define* (combine-options opts #:key (delimiter ","))
  (string-join opts delimiter 'infix))

(define (latex-macro-with-options macro opts argument)
  (format #f "\\~a~a{~a}~%"
          macro
          (if (null? opts)
              ""
              (format #f "[~a]" (combine-options (apply append opts))))
          (if (list? argument)
              (string-join (map maybe-convert argument) "}{" 'infix)
              argument)))

(define (document-class class . opts)
  (latex-macro-with-options 'documentclass opts class))

(define (use-package pkg . opts)
  (latex-macro-with-options 'usepackage opts pkg))

(define (begin-environment env)
  (latex-macro-with-options 'begin '() env))

(define (end-environment env)
  (latex-macro-with-options 'end '() env))

(define-syntax-rule (define-render-alist name (key val) ...)
  (define* (name #:key (key val) ...)
    (list (cons (quote key) key) ...)))

(define-render-alist pretty-days
  (monday 'Monday)
  (tuesday 'Tuesday)
  (wednesday 'Wednesday)
  (thursday 'Thursday)
  (friday 'Friday)
  (saturday 'Saturday)
  (sunday 'Sunday))

(define-render-alist pretty-items
  (calendar-sum "Calendar Sum")
  (daily-sum "Daily Sum")
  (day 'Day)
  (holiday 'Holiday)
  (holidays 'Holidays)
  (hours 'Hours)
  (iso-week 'Week)
  (balance 'Balance)
  (required 'Required)
  (type 'Type)
  (vacation 'Vacation)
  (vacation-days 'Vacation)
  (weekend 'Weekend)
  (weekly-sum "Weekly Sum")
  (workday 'Workday)
  (workdays 'Workdays)
  (year 'Year))

(define-render-alist pretty-alignment
  (date 'left)
  (day 'center)
  (type 'center)
  (daily-sum 'right)
  (weekly-sum 'right))

(define* (pretty-months #:key
                        (january 'January)
                        (febuary 'Febuary)
                        (march 'March)
                        (april 'April)
                        (may 'May)
                        (june 'June)
                        (july 'July)
                        (august 'August)
                        (september 'September)
                        (october 'October)
                        (november 'November)
                        (december 'December))
  (vector january febuary march april may june july
          august september october november december))

(define* (table-from calendar
                     #:key
                     (port (current-output-port))
                     (date-style 'english)
                     (alternating-shade #f)
                     (weekly-structure '(header days summary))
                     (days (pretty-days))
                     (months (pretty-months))
                     (pretty (pretty-items))
                     (alignment (pretty-alignment)))
  (define* (hline #:key (times 1))
    (let loop ((iter times))
      (if (zero? iter)
          (format port "~%")
          (begin (format port "\\hline")
                 (loop (- iter 1))))))

  (define (table-line x)
    (format port "~a \\\\~%" x))

  (define (month->name idx)
    (maybe-convert (vector-ref months (- idx 1))))

  (define (day->name d)
    (maybe-convert (assq-ref days d)))

  (define (entry->alignment e)
    (assq-ref alignment e))

  (define (entry->string e)
    (maybe-convert (assq-ref pretty e)))

  (define (render-date date)
    (let ((day (caddr date))
          (month (vector-ref months (- (cadr date) 1))))
      (case date-style
        ((english) (format #f "~a, ~a~a" month day (case day
                                                     ((1) "st")
                                                     ((2) "nd")
                                                     ((3) "rd")
                                                     (else "th"))))
        ((day-month) (format #f "~a. ~a" day month))
        (else (throw 'unknown-date-style date-style)))))

  (define (render-hours h)
    (number->string (exact->inexact h)))

  (define (render-task task)
    (format port "~a~%" (assq-ref task 'comment)))

  (define (table-columns . lst)
    (string-join lst " & " 'infix))

  (define (common-line date sym type dsum wsum)
    (table-line (table-columns (render-date date)
                               (day->name sym)
                               (entry->string type)
                               (render-hours dsum)
                               (render-hours wsum))))

  (define (render-day day weekly-sum cal-sum meta)
    (match day
      ((date (day-symbol day-type daily-sum) tasks)
       (when (memq 'days weekly-structure)
         (common-line date day-symbol day-type
                      daily-sum
                      (+ daily-sum weekly-sum))
         (when (memq 'tasks weekly-structure)
           (for-each render-task tasks)))
       daily-sum)
      (_ (throw 'broken-day-data day))))

  (define* (multicolumn text #:key (width 1) (alignment "|c|"))
    (format #f "\\multicolumn{~a}{~a}{~a}"
            width alignment text))

  (define (key-value key value)
    (format #f "~a: ~a" (entry->string key) value))

  (define (multi-key-value . lst)
    (string-join (map (lambda (x) (apply key-value x)) lst)
                 ", " 'infix))

  (define (week-header week cal-sum)
    (hline)
    (let ((y (car (caadar week)))
          (w (assq-ref week 'week)))
      (table-line
       (table-columns
        (multicolumn (key-value 'year y) #:width 2 #:alignment "|l")
        (multicolumn (key-value 'iso-week w) #:width 3 #:alignment "l|"))))
    (when (memq 'days weekly-structure) (hline)))

  (define (weekly-summary meta weekly-sum cal-sum cal-balance)
    (define (meta-kv key) (list key (assq-ref meta key)))
    (hline)
    (table-line
     (table-columns
      (multicolumn (multi-key-value (meta-kv 'workdays)
                                    (meta-kv 'vacation-days)
                                    (meta-kv 'holidays))
                   #:width 3
                   #:alignment "|l")
      (multicolumn (strcat (entry->string 'calendar-sum) ":") #:alignment "|r")
      (multicolumn (render-hours cal-sum) #:alignment "r|")))
    (let ((b (assq-ref meta 'balance)))
      (table-line
       (table-columns
        (multicolumn
         (multi-key-value (meta-kv 'required)
                          (list 'hours (render-hours weekly-sum))
                          (list 'balance
                                (strcat (if (> b 0) "+" "")
                                        (render-hours cal-balance))))
         #:width 3
         #:alignment "|l")
        (multicolumn (strcat (entry->string 'balance) ":") #:alignment "|r")
        (multicolumn (render-hours cal-balance) #:alignment "r|")))))

  (define (render-week week cal-sum cal-balance)
    (match week
      ((days meta ...)
       (when (memq 'header weekly-structure)
         (week-header week cal-sum))
       (let loop ((rest (cdr days)) (weekly-sum 0))
         (if (null? rest)
             (let ((balance (assq-ref meta 'balance)))
               (when (memq 'summary weekly-structure)
                 (weekly-summary meta weekly-sum
                                 (+ weekly-sum cal-sum)
                                 (+ balance cal-balance)))
               (unless (= weekly-sum (assq-ref week 'hours))
                 ;; If these are not the same, either this code is wrong, or
                 ;; the calendar generation is wrong, or both are wrong.
                 (throw 'inconsistent-weekly-sum
                        weekly-sum (assq-ref week 'hours)))
               (hline)
               (list weekly-sum balance))
             (begin
               (loop (cdr rest) (+ weekly-sum
                                   (render-day (car rest)
                                               weekly-sum
                                               cal-sum
                                               meta)))))))
      (_ (throw 'broken-week-day week))))

  (define (gen-alignment)
    (let ((common-data '(date day type daily-sum weekly-sum)))
      (apply strcat
             (cons "|" (map (lambda (x)
                              (let ((a (entry->alignment x)))
                                (case a
                                  ((left) "l|")
                                  ((center) "c|")
                                  ((right) "r|")
                                  (else (throw 'unknown-alignment x a)))))
                            common-data)))))

  (display-to port (begin-environment `(longtable ,(gen-alignment))))
  (let loop ((rest calendar) (cal-sum 0) (cal-balance 0))
    (if (null? rest)
        cal-sum
        (let ((cal-meta (render-week (car rest) cal-sum cal-balance)))
          (loop (cdr rest)
                (+ cal-sum (car cal-meta))
                (+ cal-balance (cadr cal-meta))))))
  (display-to port (end-environment 'longtable)))

(define* (latex-document #:key
                         (port (current-output-port))
                         (class 'scrreprt)
                         (content (lambda () #f))
                         (document-options (options a4paper
                                                    12pt
                                                    (listof totoc)))
                         (geometry (options a4paper
                                            includehead
                                            (top 2.5cm)
                                            (headsep 1cm)
                                            (bottom 2.5cm)
                                            (left 3.0cm)
                                            (right 2.5cm))))
  (display-to port
              (document-class class document-options)
              (use-package 'geometry geometry)
              (use-package 'luainputenc (options utf8))
              (use-package 'longtable)
              (begin-environment 'document))
  (content)
  (display-to port (end-environment 'document)))