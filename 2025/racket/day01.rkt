#lang racket

(require "./utils.rkt")

(define (count-zeros moves)
  (count
   (curry eq? 0)
   (foldl (lambda (val acc-list) (cons (modulo (+ val (car acc-list)) 100) acc-list)) '(50) moves)))

; part 1
(define (to-num s)
  (* (if (string=? (substring s 0 1) "R") 1 -1) (string->number (substring s 1))))
(count-zeros (map to-num (get-input "01")))

; part 2
; the hack is to convert R3 to R1 R1 R1 so the original 0 counter function just works for counting intermediate zeros
(define (to-ones s)
  (make-list (string->number (substring s 1)) (if (string=? (substring s 0 1) "R") 1 -1)))

(count-zeros (append-map to-ones (get-input "01")))
