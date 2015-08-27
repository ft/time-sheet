;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet utils file)
  #:export (read-file))

(define (read-file file)
  (with-input-from-file file
    (lambda () (read (current-input-port)))))
