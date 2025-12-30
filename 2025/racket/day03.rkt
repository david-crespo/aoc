#lang racket

(require "./utils.rkt")
(require racket/list)

(define banks
  (map (lambda (s) (map (compose string->number string) (string->list s))) (get-input "03-example")))

; (println banks)
(define (cat a b)
  (+ (* a 10) b))

; day 1
(define (joltage1 bank)
  (define tops (take (sort bank >) 2))
  (define A (car tops))
  (define iA (index-of bank A))
  (define B (car (cdr tops)))
  (define iB (index-of bank B))
  ; if the highest digit A is before the second highest B, the answer ; is AB.
  ; if the highest digit is last in the sequence, the answer is BA.
  ; otherwise the answer is AC, where C is the highest digit that comes after A
  (if (< iA iB)
      (cat A B)
      (if (equal? iA (- (length bank) 1))
          (cat B A)
          (cat A (apply max (drop bank (+ iA 1)))))))

(apply + (map joltage1 banks))
