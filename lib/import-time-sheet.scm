;; -*- scheme -*-

;; Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (time-sheet csv-import))
(setlocale LC_ALL "")
(csv->s-exp (current-input-port))
