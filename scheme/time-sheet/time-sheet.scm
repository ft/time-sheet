;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet time-sheet)
  #:export (ensure-user
            merge-time-sheets))

(define (ensure-user user data)
  (map (lambda (x)
         (let ((this-user (assq-ref x 'person)))
           (cond ((not this-user)
                  (throw 'missing-user-name x))
                 ((not (string=? user this-user))
                  (throw 'wrong-user user x))
                 (else x))))
       data))

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
