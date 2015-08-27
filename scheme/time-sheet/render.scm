;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet render)
  #:use-module (time-sheet render latex)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 optargs)
  #:export (render-calendar))

(define* (render-calendar #:key
                          (data '())
                          (type 'pretty-print)
                          (detailed? #f)
                          (with-summary? #f))
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
                      #:comment-length 78)
          (when with-summary?
            (summary-for data
                         #:styles (pretty-summary #:header *header*
                                                  #:balance *days-b*
                                                  #:off-days *summary*)))))))
    (else (throw 'unknown-renderer type data))))
