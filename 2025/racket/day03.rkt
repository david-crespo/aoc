#lang racket

(require "./utils.rkt")
(require racket/list)

(define banks
  (map (lambda (s) (map (compose string->number string) (string->list s))) (get-input "03")))

; (println banks)

;; concatenate a bunch of digits together into a number
(define (cat . digits)
  (string->number (string-join (map number->string digits) "")))

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

; ok, now we need to do the same thing but for the biggest N-digit number ;
; instead of 2-digit. I think the idea is the same:
;   1. find the highest digit A that's at least N digits from the end
;   2. the number is A + joltage(rest, n-1)

; returns list of digits
(define (joltage2 n bank)
  (if (= n 0)
      '()
      (let* ([pre (take bank (max 0 (- (length bank) (- n 1))))]
             [highest (apply max pre)]
             [idxHighest (index-of pre highest)]
             [rest (drop bank (+ idxHighest 1))])
        (cons highest (joltage2 (- n 1) rest)))))

(define (joltage n bank)
  (apply cat (joltage2 n bank)))

; confirm it works for part 1
(apply + (map (curry joltage 2) banks))

(apply + (map (curry joltage 12) banks))

; Opus 4.5's suggestions for more idiomatic Racket in this file. Great explanation of for/fold.
; TIL: cond, match, drop-right, ~a, sub1/add1, for/list, for/fold
; https://gist.github.com/david-crespo/3e0b4c45c0109335d51eba3d1d5f12b0
