(define-module (utils file)
  #:export (read-file))

(define (read-file file)
  (with-input-from-file file
    (lambda () (read (current-input-port)))))
