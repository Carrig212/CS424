#lang racket
; ### 2018 Autumn ### ;
; Define a Scheme function map-skip which takes a function and a
; list and returns the result of applying the given function to
; every other element of the given list, starting with the first
; element.
; Example:
; (map-skip (λ (x) (+ x 1000)) '(1 2 3 4 5 6))
; => (1001 2 1003 4 1005 6)

(define map-skip
  (λ (func li)
    (cond
      ((null? li) '()) ; If the list is null, return an empty list
      ((equal? 1 (length li)) (cons (func (car li)) '())) ; If the list is of size one, apply the function to the list and return it
      (else
        (append (list (func (car li)) (cadr li)) (map-skip func (drop li 2))))))) ; Otherwise, start a new list with the function applied to the head, the second element, and then recurse, dropping those two elements

; ### 2018 January ### ;
; Define a Scheme function tear which takes two arguments, a
; predicate p? and a list xs, and returns a list of two lists, the
; first of which is the elements of xs that pass p?, and the second
; of which is the elements of xs that fail it, both in order.
; Examples:
; (tear number? '(a b c 1 2 3 d e f))
; => ((1 2 3) (a b c d e f))
; (tear (lambda (x) (> x 5)) '(1 10 2 12 3 13))
; => ((10 12 13) (1 2 3))

(define tear
  (λ (p li)
    (list (filter p li) (filter (negate p) li)))) ; Create a list of two lists, one is the list filtered by the predicate, the other is the list filtered by the negation of the predicate
