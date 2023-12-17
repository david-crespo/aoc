#lang racket

(provide get-input)
(provide get-example)

(define (get-input num)
  (file->lines (format "../input/day~a.txt" num)))

(define (get-example num)
  (file->lines (format "../input/day~a-example.txt" num)))
