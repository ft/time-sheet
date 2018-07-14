;; Copyright (c) 2015-2017 time-sheet workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet render)
  #:use-module (time-sheet render latex)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:export (render-calendar))

(define *i18l-title*
  '((english . "Time-Sheet")
    (german . "Stundenzettel")))

(define *i18l-days*
  '((english)
    (german #:monday "Montag"
            #:tuesday "Dienstag"
            #:wednesday "Mittwoch"
            #:thursday "Donnerstag"
            #:friday "Freitag"
            #:saturday "Samstag"
            #:sunday "Sonntag")))

(define *i18l-months*
  '((english)
    (german #:january Januar
            #:febuary Februar
            #:march "März"
            #:april April
            #:may Mai
            #:june Juni
            #:july Juli
            #:august August
            #:september September
            #:october Oktober
            #:november November
            #:december Dezember)))

(define *i18l-items*
  '((english)
    (german #:balance Konto
            #:calendar-sum Kalendersumme
            #:compensatory Ausgleich
            #:compensatory-days Ausgleichstage
            #:daily-sum Tagessumme
            #:day Tag
            #:days Tage
            #:extra-leave Sonderurlaub
            #:extra-leave-days Sonderurlaubstage
            #:sick-leave Krankheit
            #:sick-leave-days Krankheitstage
            #:holiday Feiertag
            #:holidays Feiertage
            #:hours Stunden
            #:iso-week Kalenderwoche
            #:left "Übrig"
            #:required "Benötigt"
            #:required-hours "Benötigte Stunden"
            #:summary Zusammenfassung
            #:taken Genommen
            #:type Art
            #:vacation Urlaub
            #:vacation-days Urlaubstage
            #:vacation-left Resturlaub
            #:weekend-days Wochenendtage
            #:weekend Wochenende
            #:weekly-sum Wochensumme
            #:weeks Wochen
            #:workday Arbeitstag
            #:workdays Arbeitstage
            #:work-hours Arbeitsstunden
            #:year Jahr)))

(define *i18l-date-style*
  '((english . english)
    (german . day-month)))

(define (insert-span cal)
  (let ((start (caar (assq-ref (car cal) 'data)))
        (stop (car (last (assq-ref (last cal) 'data)))))
    (format #f "~a -- ~a"
            (string-join (map number->string start) "-" 'infix)
            (string-join (map number->string stop) "-" 'infix))))

(define* (render-calendar #:key
                          (data '())
                          (meta #f)
                          (language 'english)
                          (type 'pretty-print)
                          (detailed? #f)
                          (with-summary? #f)
                          (vacation-days #f)
                          (compensatory-days #f)
                          (extra-leave-days #f)
                          (post-hook #f))
  (define (i18l fb lst)
    (apply fb (assq-ref lst language)))
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
            (display (font 'Huge (format #f "~a: ~a"
                                         (assq-ref *i18l-title* language)
                                         (insert-span data))))
            (display (paragraph))
            (newline)
            (display "\\vspace{0.25cm}")
            (newline)
            (display
             (font 'Large
                   (format #f "~a \\texttt{\\guilsinglleft{}~a\\guilsinglright{}}"
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
                      #:vacation-days vacation-days
                      #:date-style (assq-ref *i18l-date-style* language)
                      #:months (i18l pretty-months *i18l-months*)
                      #:days (i18l pretty-days *i18l-days*)
                      #:pretty (i18l pretty-items *i18l-items*))
          (when with-summary?
            (summary-for data
                         #:vacation-days vacation-days
                         #:compensatory-days compensatory-days
                         #:extra-leave-days extra-leave-days
                         #:styles (pretty-summary #:header *header*
                                                  #:balance *days-b*
                                                  #:off-days *summary*)
                         #:pretty (i18l pretty-items *i18l-items*)))
          (when (thunk? post-hook)
            (post-hook))))))
    (else (throw 'unknown-renderer type data))))
