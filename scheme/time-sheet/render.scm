;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet render)
  #:use-module (time-sheet render latex)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 optargs)
  #:export (render-calendar))

(define (insert-span cal)
  (let ((first (car cal))
        (last (car (reverse cal))))
    (format #f "~a -- ~a"
            (string-join (map number->string (car (cadar first))) "-" 'infix)
            (string-join (map number->string (car (cadar last))) "-" 'infix))))

(define* (render-calendar #:key
                          (data '())
                          (meta #f)
                          (type 'pretty-print)
                          (detailed? #f)
                          (with-summary? #f)
                          (vacation-days #f)
                          (compensatory-days #f)
                          (extra-leave-days #f))
  (case type
    ((pretty-print)
     (pretty-print data #:width 150 #:max-expr-width 150))
    ((latex)
     (let ((struct '(header days summary))
           (*header* (line-style #:colour 'gray
                                 #:intensity 0.8
                                 #:size 'scriptsize
                                 #:text-colour 'rgb
                                 #:text-hue '(1 1 1)))
           (*days-a* (if detailed?
                         (line-style #:colour 'gray
                                     #:intensity 0.25)
                         (line-style)))
           (*days-b* (line-style #:colour 'gray
                                 #:intensity 0.25))
           (*tasks* (line-style #:size 'tiny))
           (*summary* (line-style #:colour 'gray
                                  #:intensity 0.1)))
       (latex-document
        #:content
        (lambda ()
          (when meta
            (newline)
            (display (begin-environment 'center))
            (display (font 'Huge (format #f "Time-sheet: ~a"
                                         (insert-span data))))
            (display (paragraph))
            (newline)
            (display "\\vspace{0.25cm}")
            (newline)
            (display
             (font 'Large
                   (format #f "~a \\texttt{\\textless{}~a\\textgreater{}}"
                           (assq-ref meta 'name)
                           (assq-ref meta 'email))))
            (display (paragraph))
            (newline)
            (display "\\vspace{0.25cm}")
            (newline)
            (display (end-environment 'center))
            (newline))
          (table-from data
                      #:weekly-structure (if detailed?
                                             (cons 'tasks
                                                   (if (eq? detailed? 'extreme)
                                                       (cons 'comments struct)
                                                       struct))
                                             struct)
                      #:alternating-shade (not detailed?)
                      #:styles (pretty-styles #:header *header*
                                              #:days-a *days-a*
                                              #:days-b *days-b*
                                              #:tasks *tasks*
                                              #:summary *summary*)
                      #:subject-length 80
                      #:comment-length 78
                      #:vacation-days vacation-days)
          (when with-summary?
            (summary-for data
                         #:vacation-days vacation-days
                         #:compensatory-days compensatory-days
                         #:extra-leave-days extra-leave-days
                         #:styles (pretty-summary #:header *header*
                                                  #:balance *days-b*
                                                  #:off-days *summary*)))))))
    (else (throw 'unknown-renderer type data))))
