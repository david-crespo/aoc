#lang racket

(require "./utils.rkt")

(define (nums line)
  (map string->number (regexp-match* #px"[0-9]" line)))

(define (proc line)
  (+
    (* 10 (first (nums line)))
    (last (nums line))))

; (apply + (map proc (get-input "01-example")))
; (apply + (map proc (get-input "01")))

;;;;;;;;;;;;;;;;;;;;;

(define (string-numeric s) (regexp-match #px"^[0-9]+$" s))

(define num-names 
  (hash "one" 1
		"two" 2
		"three" 3
		"four" 4
		"five" 5
		"six" 6
		"seven" 7
		"eight" 8
		"nine" 9))

(define (to-num num)
  (if (string-numeric num) (string->number num) (hash-ref num-names num)))

(define num-regex "one|two|three|four|five|six|seven|eight|nine|[0-9]")
(define rev-regex "enin|thgie|neves|xis|evif|ruof|eerht|owt|eno|[0-9]")

(define rev-str (compose list->string reverse string->list))

(define (proc2 line)
  (+
    (* 10 (first (map to-num (regexp-match* num-regex line))))
	; get matches from the end by reversing the string and reversing the regex,
	; and then un-reversing. it's appalling
    (first (map (compose to-num rev-str) (regexp-match* rev-regex (rev-str line))))))

(apply + (map proc2 (get-input "01-example2")))
(apply + (map proc2 (get-input "01")))

