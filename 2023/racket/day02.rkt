#lang racket

(require "./utils.rkt")

(struct draw (red green blue))

(define (parse-line line)
  (define pair (string-split line ":"))
  (define id (string->number (last (string-split (first pair)))))
  ;; each draw is a list of (num, color) pairs. this sucks though
  (define draw-strs 
    (map 
	  (lambda (x) 
		(map 
		  (lambda (y) 
		    (cons 
		      (string->number (first (string-split y))) 
			  (last ( string-split y))))
		  (string-split x ",")))
      (string-split (last pair) ";")))
  values id draw-strs)


(for ([line (get-input "02-example")])
  (displayln line)
  (displayln (parse-line line))
  (displayln "")
)
