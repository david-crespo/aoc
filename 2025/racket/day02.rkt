#lang racket

(require "./utils.rkt")

(define ranges
  (map (lambda (s) (map string->number (string-split s "-")))
       (string-split (car (get-input "02")) ",")))

(define (range->list a b)
  (foldl (lambda (_ acc) (cons (+ (car acc) 1) acc)) (list a) (make-list (- b a) 1)))

(define nums (append-map (lambda (range) (apply range->list range)) ranges))

; day 1
(define (invalid? s)
  (regexp-match? #px"^(\\d+)\\1$" s))

(apply + (filter (lambda (n) (invalid? (number->string n))) nums))

; day 2
(define (invalid2? s)
  (regexp-match? #px"^(\\d+)\\1+$" s))

(apply + (filter (lambda (n) (invalid2? (number->string n))) nums))
