;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

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

(define-module (time-sheet render latex)
  #:use-module (time-sheet utils date)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:export (pretty-alignment
            pretty-days
            pretty-items
            pretty-months
            pretty-styles
            pretty-summary
            latex-document
            line-style
            summary-for
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

(define* (line-style #:key
                     (colour #f)
                     (hue #f)
                     (intensity 1)
                     (text-colour #f)
                     (text-hue #f)
                     (text-intensity 1)
                     (size #f)
                     (size-unit 'pt)
                     (line-space #f)
                     (line-space-unit 'em))
  "Return a text-property setter.

The return value is a pair of functions of one argument. That argument is used
as a string that the functions will wrap into LaTeX code that applies a number
of properties, which define the way the text is rendered.

The first function of the pair applies properties to an entire line in a table,
the second function applies properties to a piece of text in a column of a line
in a table.

line-style uses keyword arguments to determine the type of function it returns,
the default values of these keywords will cause the function to return the
identity function: (lambda (x) x).

  #:colour => this has to be either `gray', `rgb', `cmyk', `named' or any
              colour recognised by the `color' LaTeX package.

  #:hue => In case `#:colour' is `named' this defines the colour to be used by
           name. If it is `rgb' this has to be a list of three values between
           0 and 1; in case of `cmyk' a list of four values between 0 and 1. In
           case of `gray' this value is ignored.

  #:intensity => With `#:colour' set to `named' of `gray' this defines the
                 intensity of the colour.

  #:text-colour => Like #:colour but for text.

  #:text-hue => Like #:hue but for text.

  #:text-intensity => Like #:intensity but for text.

  #:size => Defines the font size for the text. This may be an integer or a
            symbol that specifies one of LaTeX's default font sizes (like
            Huge, tiny or scriptsize).

  #:size-unit => The unit used for #:size (default: pt)

  #:line-space => Defines the line-space for the text.

  #:line-space-unit => The unit used for #:line-space (default: em)"
  (cons (lambda (x)
          (with-row-colour colour hue intensity x))
        (lambda (x)
          (with-text-colour text-colour text-hue text-intensity
                            (with-size size size-unit
                                       line-space line-space-unit x)))))

(define (with-size s su ls lsu text)
  (define (size-fallback size fallback)
    (if size size fallback))
  (define (unit-fallback size unit fallback)
    (if size unit fallback))
  (cond ((not (or s ls)) text)
        ((symbol? s) (format #f "{\\~a ~a}" s text))
        (else (format #f "{\\fontsize{~a~a}{~a~a}\\selectfont ~a}"
                      (size-fallback s ls)
                      (unit-fallback s su lsu)
                      (size-fallback ls s)
                      (unit-fallback ls lsu su)
                      text))))

(define (with-colour* macro c h i text pp)
  (define (join-colour value len excpt)
    (if (= (length h) 3)
        (string-join (map (lambda (x)
                            (number->string
                             (exact->inexact x)))
                          h)
                     "," 'infix)
        (throw excpt h)))
  (cond ((not c) text)
        (else (let ((model (case c
                             ((gray rgb cmyk named) c)
                             (else 'named)))
                    (value (case c
                             ((gray) (- 1 i))
                             ((named) i)
                             ((rgb) (join-colour h 3 'invalid-rgb-set))
                             ((cmyk) (join-colour h 4 'invalid-cmyk-set))
                             (else c))))
                (pp (format #f "\\~a[~a]{~a} ~a" macro model value text))))))

(define (with-row-colour c h i text)
  (with-colour* 'rowcolor c h i text (lambda (x) x)))

(define (with-text-colour c h i text)
  (with-colour* 'color c h i text
                (lambda (x) (strcat "{" x "}"))))

(define (latex-escape data)
  (regexp-substitute/global #f "([_&#])" data 'pre "\\" 1 'post))

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
  (balance 'Balance)
  (calendar-sum "Calendar Sum")
  (compensatory 'Compensatory)
  (compensatory-days "Compensatory Days")
  (daily-sum "Daily Sum")
  (day 'Day)
  (days 'Days)
  (extra-leave 'Extra-Leave)
  (extra-leave-days "Extra Leave Days")
  (holiday 'Holiday)
  (holidays 'Holidays)
  (hours 'Hours)
  (iso-week 'Week)
  (required 'Required)
  (required-hours "Required Hours")
  (summary 'Summary)
  (type 'Type)
  (vacation 'Vacation)
  (vacation-days 'Vacation)
  (weekend-days "Weekend Days")
  (weekend 'Weekend)
  (weekly-sum "Weekly Sum")
  (weeks 'Weeks)
  (workday 'Workday)
  (workdays 'Workdays)
  (work-hours "Work Hours")
  (year 'Year))

(define-render-alist pretty-alignment
  (date 'left)
  (day 'center)
  (type 'center)
  (daily-sum 'right)
  (weekly-sum 'right))

(define-render-alist pretty-styles
  (header (line-style))
  (days-a (line-style))
  (days-b (line-style))
  (tasks (line-style))
  (summary (line-style)))

(define-render-alist pretty-summary
  (header (line-style))
  (general (line-style))
  (off-days (line-style))
  (hours (line-style))
  (balance (line-style)))

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

(define* (multicolumn text #:key (width 1) (alignment "|c|"))
  (format #f "\\multicolumn{~a}{~a}{~a}"
          width alignment text))

(define (table-columns . lst)
  (string-join lst " & " 'infix))

(define (render-hours h)
  (number->string (exact->inexact h)))

(define* (hline port #:key (times 1))
  (let loop ((iter times))
    (if (zero? iter)
        (format port "~%")
        (begin (format port "\\hline")
               (loop (- iter 1))))))

(define* (summary-for calendar
                      #:key
                      (port (current-output-port))
                      (pretty (pretty-items))
                      (styles (pretty-summary)))
  (define (table-line x)
    (format port "~a \\\\~%" x))
  (define (entry->string e)
    (maybe-convert (assq-ref pretty e)))
  (define (styled-columns style . lst)
    (apply table-columns (map style lst)))
  (let* ((get (lambda (x)
                (apply + (map (lambda (y) (assq-ref y x))
                              calendar))))
         (wd (get 'workdays))
         (vd (get 'vacation-days))
         (hd (get 'holidays))
         (wed (get 'weekend-days))
         (cp (get 'compensatory-days))
         (el (get 'extra-leave-days))
         (hours (get 'hours))
         (balance (get 'balance))
         (days (+ wd vd hd wed cp el))
         (weeks (/ days 7))
         (*header* (assq-ref styles 'header))
         (ls-header (car *header*))
         (cs-header (cdr *header*))
         (*general* (assq-ref styles 'general))
         (ls-general (car *general*))
         (cs-general (cdr *general*))
         (*off-days* (assq-ref styles 'off-days))
         (ls-off-days (car *off-days*))
         (cs-off-days (cdr *off-days*))
         (*hours* (assq-ref styles 'hours))
         (ls-hours (car *hours*))
         (cs-hours (cdr *hours*))
         (*balance* (assq-ref styles 'balance))
         (ls-balance (car *balance*))
         (cs-balance (cdr *balance*)))
    (display-to port (begin-environment 'center))
    (display-to port (begin-environment '(tabular "|l|r|")))
    (hline port)
    (table-line (ls-header (multicolumn (cs-header (entry->string 'summary))
                                        #:width 2)))
    (hline port)
    (table-line (ls-general (styled-columns
                             cs-general
                             (entry->string 'weeks)
                             (if (integer? weeks)
                                 (number->string weeks 10)
                                 (format #f "~,3f" (exact->inexact weeks))))))
    (hline port)
    (table-line (ls-general (styled-columns cs-general
                                            (entry->string 'days)
                                            (number->string days))))
    (hline port)
    (table-line (ls-general (styled-columns cs-general
                                            (entry->string 'workdays)
                                            (number->string wd))))
    (hline port)
    (table-line (ls-off-days (styled-columns cs-off-days
                                             (entry->string 'vacation-days)
                                             (number->string vd))))
    (hline port)
    (table-line (ls-off-days (styled-columns cs-off-days
                                             (entry->string 'compensatory-days)
                                             (number->string cp))))
    (hline port)
    (table-line (ls-off-days (styled-columns cs-off-days
                                             (entry->string 'extra-leave-days)
                                             (number->string el))))
    (hline port)
    (table-line (ls-off-days (styled-columns cs-off-days
                                             (entry->string 'holidays)
                                             (number->string hd))))
    (hline port)
    (table-line (ls-off-days (styled-columns cs-off-days
                                             (entry->string 'weekend-days)
                                             (number->string wed))))
    (hline port)
    (table-line (ls-hours (styled-columns cs-hours
                                          (entry->string 'required-hours)
                                          (render-hours (* 8 wd)))))
    (hline port)
    (table-line (ls-hours (styled-columns cs-hours
                                          (entry->string 'work-hours)
                                          (render-hours hours))))
    (hline port)
    (table-line (ls-balance (styled-columns cs-balance
                                            (entry->string 'balance)
                                            (format #f "~a~a"
                                                    (if (> balance 0)
                                                        "+" "")
                                                    (render-hours balance)))))
    (hline port)
    (display-to port (end-environment 'tabular))
    (display-to port (end-environment 'center))))

(define* (table-from calendar
                     #:key
                     (port (current-output-port))
                     (comment-length #f)
                     (subject-length #f)
                     (date-style 'english)
                     (alternating-shade #f)
                     (weekly-structure '(header days summary))
                     (days (pretty-days))
                     (months (pretty-months))
                     (pretty (pretty-items))
                     (styles (pretty-styles))
                     (alignment (pretty-alignment)))

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

  (define (maybe-substring maxlen string)
    (if maxlen
        (let ((strlen (string-length string)))
          (if (> strlen maxlen)
              (strcat (substring string 0 maxlen) "...")
              string))
        string))

  (define (chop-subject string)
    (maybe-substring subject-length string))

  (define (chop-comment string)
    (maybe-substring comment-length string))

  (define (render-task task)
    (let* ((style (assq-ref styles 'tasks))
           (row-style (car style))
           (col-style (cdr style))
           (id (assq-ref task 'ticket-id))
           (subject (assq-ref task 'ticket-subject))
           (project (assq-ref task 'project))
           (comment (assq-ref task 'comment)))
      (table-line
       (row-style
        (table-columns
         (multicolumn
          (col-style (format #f "~a~a: ~a"
                             (if id "\\#" "")
                             (if id id (latex-escape project))
                             (latex-escape (if id
                                               (chop-subject subject)
                                               (chop-comment comment)))))
          #:width 3
          #:alignment "|l|")
         (multicolumn
          (col-style (render-hours (assq-ref task 'time)))
          #:width 1
          #:alignment "r|")
         (multicolumn "" #:width 1 #:alignment "r|"))))
      (when (and id (memq 'comments weekly-structure))
        (table-line
         (row-style
          (table-columns
           (multicolumn
            (col-style (format #f "$\\quad$ ~a"
                               (latex-escape (chop-comment comment))))
            #:width 3
            #:alignment "|l|")
           (multicolumn "" #:width 1 #:alignment "r|")
           (multicolumn "" #:width 1 #:alignment "r|")))))))

  (define (make-weekly-day-printer)
    (let* ((n -1)
           (style-a (assq-ref styles 'days-a))
           (style-b (assq-ref styles (if alternating-shade 'days-b 'days-a)))
           (style-a-row (car style-a))
           (style-a-col (cdr style-a))
           (style-b-row (car style-b))
           (style-b-col (cdr style-b)))
      (lambda (date sym type dsum wsum)
        (set! n (+ 1 n))
        (let ((row-style (if (zero? (modulo n 2)) style-a-row style-b-row))
              (col-style (if (zero? (modulo n 2)) style-a-col style-b-col)))
          (table-line (row-style
                       (table-columns (col-style (render-date date))
                                      (col-style (day->name sym))
                                      (col-style (entry->string type))
                                      (col-style (render-hours dsum))
                                      (col-style (render-hours wsum)))))))))

  (define (render-day day weekly-sum cal-sum meta printer)
    (match day
      ((date (day-symbol day-type daily-sum) tasks)
       (when (memq 'days weekly-structure)
         (printer date day-symbol day-type
                  daily-sum
                  (+ daily-sum weekly-sum))
         (when (and (memq 'tasks weekly-structure)
                    (not (null? tasks)))
           (hline port)
           (for-each render-task tasks)
           (hline port)))
       daily-sum)
      (_ (throw 'broken-day-data day))))

  (define (key-value key value)
    (format #f "~a: ~a" (entry->string key) value))

  (define (multi-key-value . lst)
    (string-join (map (lambda (x) (apply key-value x)) lst)
                 ", " 'infix))

  (define (week-header week cal-sum)
    (hline port)
    (let* ((y (car (caadar week)))
           (w (assq-ref week 'week))
           (style (assq-ref styles 'header))
           (row-style (car style))
           (col-style (cdr style)))
      (table-line (row-style (table-columns
                              (multicolumn (col-style (key-value 'year y))
                                           #:width 2
                                           #:alignment "|l")
                              (multicolumn (col-style (key-value 'iso-week w))
                                           #:width 3
                                           #:alignment "l|")))))
    (when (memq 'days weekly-structure)
      (hline port)))

  (define (weekly-summary meta weekly-sum cal-sum cal-balance)
    (define (meta-kv key) (list key (assq-ref meta key)))
    (hline port)
    (let* ((b (assq-ref meta 'balance))
           (style (assq-ref styles 'summary))
           (row-style (car style))
           (col-style (cdr style)))
      (table-line
       (row-style (table-columns
                   (multicolumn
                    (col-style (multi-key-value (meta-kv 'workdays)
                                                (meta-kv 'vacation-days)
                                                (meta-kv 'holidays)))
                    #:width 3
                    #:alignment "|l|")
                   (multicolumn "" #:width 2 #:alignment "r|"))))
      (table-line
       (row-style (table-columns
                   (multicolumn
                    (col-style (multi-key-value (meta-kv 'compensatory-days)
                                                (meta-kv 'extra-leave-days)))
                    #:width 3
                    #:alignment "|l|")
                   (multicolumn
                    (col-style (strcat (entry->string 'calendar-sum) ":"))
                    #:alignment "r")
                   (multicolumn (col-style (render-hours cal-sum))
                                #:alignment "r|"))))
      (table-line
       (row-style (table-columns
                   (multicolumn
                    (col-style (multi-key-value
                                (meta-kv 'required)
                                (list 'hours (render-hours weekly-sum))
                                (list 'balance
                                      (strcat (if (> b 0) "+" "")
                                              (render-hours b)))))
                    #:width 3
                    #:alignment "|l|")
                   (multicolumn
                    (col-style (strcat (entry->string 'balance) ":"))
                    #:alignment "r")
                   (multicolumn
                    (col-style (strcat (if (> cal-balance 0) "+" "")
                                       (render-hours cal-balance)))
                    #:alignment "r|"))))))

  (define (render-week week cal-sum cal-balance)
    (match week
      ((days meta ...)
       (when (memq 'header weekly-structure)
         (week-header week cal-sum))
       (let ((day-printer (make-weekly-day-printer)))
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
                 (hline port)
                 (list weekly-sum balance))
               (begin
                 (loop (cdr rest) (+ weekly-sum
                                     (render-day (car rest)
                                                 weekly-sum
                                                 cal-sum
                                                 meta
                                                 day-printer))))))))
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
                                                    10pt
                                                    (listof totoc)))
                         (geometry (options a4paper
                                            includehead
                                            (top 2.5cm)
                                            (headsep 0cm)
                                            (bottom 2.5cm)
                                            (left 1.5cm)
                                            (right 1.5cm))))
  (display-to port
              (document-class class document-options)
              (use-package 'geometry geometry)
              (use-package 'luainputenc (options utf8))
              (use-package 'longtable)
              (use-package 'color)
              (use-package 'colortbl)
              (begin-environment 'document))
  (content)
  (display-to port (end-environment 'document)))
