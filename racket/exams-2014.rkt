#lang racket
; ### 2014 Autumn ### ;
; Define a function add-numbers which takes an s-expression
; and returns the sum of all the numbers contained therein.
; E.g.,
; (add-numbers 17) => 17
; (add-numbers '(a (1 (2) 3) 4)) => 10
; (add-numbers '(the quick fox)) => 0
; Note: the Scheme predicate "number?" can be used to test if
; a given value is a number.

(define add-numbers
  (λ (li)
  (set! li (filter number? (flatten li))) ; Flatten inner lists filter out non-numbers
  (cond
    ((null? li) 0) ; If the list is null, return zero
    (else
      (+ (car li) (add-numbers (cdr li))))))) ; Otherwise sum the head of the list and recurse with the tail

; ### 2014 January ### ;
; Define a higher-order function deep-fetch which takes a
; predicate and an s-expression, and returns a list of all atoms
; inside the given s-expression which pass the given predicate.
; Examples:
; (deep-fetch number? '(the (quick 6 fox 8 9) slick 2))
; -> (6 8 9 2)
; (deep-fetch symbol? '(the (quick 6 fox 8 9) slick 2))
; -> (the quick fox slick)

(define deep-fetch
  (λ (p li)
  (filter p (flatten li)))) ; Flatten inner lists, filter by the predicate
