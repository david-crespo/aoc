#lang racket

(require "./utils.rkt")

(define (nums line)
  (map string->number (regexp-match* #px"[0-9]" line)))

(define (proc line)
  (+
    (* 10 (first (nums line)))
    (last (nums line))))

(apply + (map proc (get-example "01")))
(apply + (map proc (get-input "01")))
