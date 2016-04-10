;; Copyright (c) 2016 time-sheet workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; The JSON exports from Redmine (especially the ones facilitated by the
;; ‘get-redmine-timesheet’ script from the ‘contrib/’ subdirectory) do cover
;; quite a bit more information than did the CSV exports. This module jumps
;; through a number of hoops to keep all the information from these exports
;; intact while making the final result comply with the previously agreed on
;; format of the S expression data base.
;;
;; The external API of this importer module matches the one of the CSV importer
;; module.

(define-module (time-sheet json-import)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module ((srfi srfi-19) #:prefix srfi:)
  #:use-module (time-sheet utils data)
  #:use-module (time-sheet utils file)
  #:use-module ((json parser) #:prefix gj:)
  #:export (json->scm
            json->s-exp
            read-timesheet
            timesheet->alist
            timesheet->sorted-alist))

(define (parse-date str)
  (let ((date (srfi:string->date str "~Y-~m-~dT~H:~M:~S~z")))
    (list (list 'date
                (cons 'year (srfi:date-year date))
                (cons 'month (srfi:date-month date))
                (cons 'day (srfi:date-day date)))
          (list 'time
                (cons 'hour (srfi:date-hour date))
                (cons 'minute (srfi:date-minute date))
                (cons 'second (srfi:date-second date))
                (cons 'zone-offset (srfi:date-zone-offset date))))))

(define (convert-number x)
  (inexact->exact (if (string? x) (string->number x) x)))

(define (cb-time key value)
  (cons 'time (convert-number value)))

(define (cb-date key value)
  (cons 'date (string->date value)))

(define (cb-creation-time key value)
  (cons 'creation-time (parse-date value)))

(define (cb-update-time key value)
  (cons 'update-time (parse-date value)))

(define (cb-comment key value)
  (cons 'comment value))

(define (cb-entity-id key value)
  (cons 'type-id (if (string? value)
                     (string->number value)
                     value)))

(define (cb-entity-type key value)
  (cons 'type (string->symbol (string-downcase value))))

(define (cb-project key value)
  (cons 'project (assq-ref value 'name)))

(define (cb-user key value)
  (cons 'user (assq-ref value 'name)))

(define (cb-issue data)
  (let ((value* (assq 'issue data)))
    (if value*
        (let* ((value (cdr value*))
               (id (assq 'id value))
               (subject (assq 'subject value)))
          (append (if id
                      (list (cons 'ticket-id (cdr id)))
                      '())
                  (if subject
                      (list (cons 'ticket-subject (cdr subject)))
                      '())))
        '())))

(define (cb-activity key value)
  (cons 'activity (assq-ref value 'name)))

(define processors
  `((consume hours ,cb-time)
    (consume spent_on ,cb-date)
    (consume created_on ,cb-creation-time)
    (consume updated_on ,cb-update-time)
    (consume comments ,cb-comment)
    (consume entity_id ,cb-entity-id)
    (consume entity_type ,cb-entity-type)
    (consume project ,cb-project)
    (consume user ,cb-user)
    (consume activity ,cb-activity)
    (produce ,cb-issue)))

;; The guile-json module turns the exported JSON stream into hash-tables, which
;; we then convert into association lists. That is what is called ‘timesheet’
;; data in the context of this module. Now, in order to make this compatible
;; with the data produced by the other importers (really the CSV importer at
;; the time writing), we have to produce as well as convert key value pairs
;; from that original timesheet data.
;;
;; And here is how it works: The ‘processors’ table defines a number of
;; consumers and producers: Consumers take an original key/value pair and
;; returns a new key/value pair (or list of key/value pairs) from it. Producers
;; take the entire original association list and produce a new key/value pair
;; (or list of key/value pairs) from it.
;;
;; The key difference with respect to the final result is, that key/value pairs
;; fed into consumers will be *removed* from the original key/value pairs. Any
;; key/value pairs from the original value that are not matched to consumers,
;; will remain in the final result unchanged.
;;
;; This makes sure that the final result will be compatible with the data sets
;; produced by other importers, while at the same time keeping any additional
;; data, that may be useful at a later date.

(define (maybe-consume lst new)
  (let ((type (car new))
        (entry (cadr new)))
    (if (eq? type 'consume)
        (cons entry lst)
        lst)))

(define (cons-or-append acc new)
  (cond ((null? new) acc)
        ((pair? (car new)) (append new acc))
        ((pair? new) (cons new acc))
        (else acc)))

(define (accumulate data acc new)
  (let ((type (car new)))
    (if (eq? type 'consume)
        (let* ((entry (cadr new))
               (kv (assq entry data))
               (proc (caddr new)))
          (if (not kv)
              acc
              (cons-or-append acc (proc (car kv) (cdr kv)))))
        (let ((proc (cadr new)))
          (cons-or-append acc (proc data))))))

(define (symbol-underscore-to-dash x)
  (string->symbol (regexp-substitute/global #f "_"
                                            (symbol->string x)
                                            'pre "-" 'post)))

(define (kill-avatar data)
  (remove (lambda (x) (eq? (car x) 'avatar_urls)) data))

(define (clean-issue data)
  (map (lambda (x)
         (let ((key (car x))
               (value (cdr x)))
           (cond ((eq? key 'created-on)
                  (cons 'creation-time (parse-date value)))
                 ((eq? key 'updated-on)
                  (cons 'update-time (parse-date value)))
                 ((eq? key 'assigned-to)
                  (cons key (kill-avatar value)))
                 ((eq? key 'done-ratio)
                  (cons 'done (convert-number value)))
                 ((eq? key 'spent-hours)
                  (cons 'hours-spent (convert-number value)))
                 (else x))))
       (map (lambda (x)
              (cons (symbol-underscore-to-dash (car x)) (cdr x)))
            data)))

(define (timesheet-post-process data consumed acc)
  (let ((rest (remove (lambda (x) (memq (car x) consumed)) data)))
    (map (lambda (x)
           (if (eq? 'issue (car x))
               (cons 'issue (clean-issue (cdr x)))
               x))
         (append acc rest))))

(define (timesheet->alist* data)
  (let loop ((rest processors)
             (consumed '())
             (acc '()))
    (cond ((null? rest) (timesheet-post-process data consumed acc))
          (else (let ((this (car rest)))
                  (loop (cdr rest)
                        (maybe-consume consumed this)
                        (accumulate data acc this)))))))

(define (timesheet->alist data)
  (map timesheet->alist* data))

(define (timesheet->sorted-alist data)
  (sort-by-id (timesheet->alist data)))

(define (read-timesheet port)
  (assq-ref (hash-map->alist (gj:json->scm port)) 'time_entries))

(define* (json->scm file-or-port #:key (sort? #t))
  ((if sort? timesheet->sorted-alist timesheet->alist)
   (use-file-or-port file-or-port (lambda ()
                                    (read-timesheet (current-input-port))))))

(define* (json->s-exp file-or-port #:key (sort? #t))
  (pretty-print (json->scm file-or-port #:sort? sort?)))
