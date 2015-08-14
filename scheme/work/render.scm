(define-module (work render)
  #:use-module (ice-9 pretty-print)
  #:export (render-calendar))

;; pretty-print: Uses (ice-9 pretty-print)
;; latex-portrait-full: TODO
;; latex-portrait-terse: TODO
;; latex-landscape-full: TODO
;; latex-landscape-terse: TODO
(define (render-calendar type calendar)
  (cond ((eq? type #:pretty-print)
         (pretty-print calendar #:width 150 #:max-expr-width 150))
        (else (throw 'unknown-renderer type calendar))))
