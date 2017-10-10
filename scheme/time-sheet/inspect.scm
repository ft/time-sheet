;; Copyright (c) 2015-2017 time-sheet workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet inspect)
  #:export (ensure-user))

(define (ensure-user user data)
  (map (lambda (x)
         (let ((this-user (assq-ref x 'person)))
           (cond ((not this-user)
                  (throw 'missing-user-name x))
                 ((not (string=? user this-user))
                  (throw 'wrong-user user x))
                 (else x))))
       data))
