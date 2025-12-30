#lang racket

(provide get-input)

(define (get-input num)
  (file->lines (format "../input/day~a.txt" num)))
