;; -*- scheme -*-

(use-modules (work csv-import))
(setlocale LC_ALL "")
(csv->s-exp (current-input-port))
