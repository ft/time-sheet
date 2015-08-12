(use-modules (work csv-import))

(define (statistics f)
  (apply + (map (lambda (x) (assq-ref x 'time))
                (csv->scm f #:sort? #f))))

(format #t "~a~%" (exact->inexact (apply + (map statistics
                                                (cdr (command-line))))))
