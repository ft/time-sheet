;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (time-sheet csv-import))

(define (statistics f)
  (apply + (map (lambda (x) (assq-ref x 'time))
                (csv->scm f #:sort? #f))))

(format #t "~a~%" (exact->inexact (apply + (map statistics
                                                (cdr (command-line))))))
