;; Copyright (c) 2015-2017 time-sheet workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet merge)
  #:export (merge-time-sheets))

(define (id< a b)
  (< (assq-ref a 'id)
     (assq-ref b 'id)))

(define (merge-time-sheets lst)
  (let ((sorted (sort (apply append lst) id<)))
    (let loop ((rest sorted)
               (prev-entry #f))
      (cond ((null? rest) sorted)
            ((not prev-entry)
             (loop (cdr rest)
                   (car rest)))
            (else (let* ((this-entry (car rest))
                         (this (assq-ref this-entry 'id))
                         (prev (assq-ref prev-entry 'id)))
                    (if (= this prev)
                        (throw 'duplicate-id prev-entry this-entry)
                        (loop (cdr rest) this-entry))))))))
