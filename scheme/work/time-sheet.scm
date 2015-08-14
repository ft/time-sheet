(define-module (work time-sheet)
  #:export (merge-time-sheets))

;; TODO: This is waaaay to simple; merging should at least check, if entries
;;       exist more than once!
(define (merge-time-sheets lst)
  (apply append lst))
