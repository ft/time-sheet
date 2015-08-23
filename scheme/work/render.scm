(define-module (work render)
  #:use-module (work render latex)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 optargs)
  #:export (render-calendar))

(define* (render-calendar #:key
                          (data '())
                          (type 'pretty-print)
                          (detailed? #f))
  (case type
    ((pretty-print)
     (pretty-print data #:width 150 #:max-expr-width 150))
    ((latex)
     (let ((struct '(header days summary)))
       (latex-document
        #:content
        (lambda ()
          (table-from data
                      #:weekly-structure (if detailed?
                                             (cons 'tasks struct)
                                             struct)
                      #:alternating-shade (not detailed?)
                      #:styles (pretty-styles #:header
                                              (line-style #:colour 'gray
                                                          #:intensity 0.8
                                                          #:size 'scriptsize
                                                          #:text-colour 'rgb
                                                          #:text-hue '(1 1 1))
                                              #:days-a
                                              (if detailed?
                                                  (line-style #:colour 'gray
                                                              #:intensity 0.25)
                                                  (line-style))
                                              #:days-b
                                              (line-style #:colour 'gray
                                                          #:intensity 0.25)
                                              #:tasks (line-style #:size 'tiny)
                                              #:summary
                                              (line-style #:colour 'gray
                                                          #:intensity 0.1)))))))
    (else (throw 'unknown-renderer type data))))
