;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (time-sheet script)
  #:use-module (ice-9 getopt-long)
  #:export (script-options))

(define *modes* '("compact" "detailed" "complete"))

(define (valid-mode? mode)
  (member mode *modes*))

(define *languages* '("english" "german"))

(define (valid-language? lang)
  (member lang *languages*))

(define *optspec* `((help (single-char #\h) (value #f))
                    (mode (single-char #\m) (value #t)
                          (predicate ,valid-mode?))
                    (lang (single-char #\l) (value #t)
                          (predicate ,valid-language?))))

(define (bool-opt? opts opt)
  (option-ref opts opt #f))

(define (help-text cmd)
  (format #t "usage: ~a [OPTION(s)...]~%~%" cmd)
  (format #t "  --help, -h                   Display this help text.~%")
  (format #t "  --mode <MODE>, -m <MODE>     Specify a time-sheet mode.~%")
  (format #t "  --lang <LANG>, -l <LANG>     Specify language for time-sheet.~%")
  (format #t "~%Available modes: compact, detailed, complete.~%")
  (format #t "Available languages: german, english.~%")
  (quit 0))

(define (mode->detailed-predicate mode)
  (assq-ref '((compact . #f)
              (detailed . #t)
              (complete . extreme))
            mode))

(define (script-options cmdline)
  (let* ((cmd (car cmdline))
         (options (getopt-long cmdline *optspec*))
         (mode (string->symbol (option-ref options 'mode "compact")))
         (lang (string->symbol (option-ref options 'lang "english"))))
    (cond ((bool-opt? options 'help)
           (help-text cmd))
          (else (list (cons 'detailed? (mode->detailed-predicate mode))
                      (cons 'language lang))))))
