#lang racket
; ### 2017 Autumn ### ;
; Define a Scheme function foo that takes two lists and yields a list combining all the
; elements in the two input lists, taking 1 from the first list, 2 from the second list, 3 from
; the first list, 4 from the second list, etc, until both are exhausted.
; Examples:
; (foo '(a b c d e f g) '(aa bb cc dd ee ff gg))
; => (a aa bb b c d cc dd ee ff e f g gg)
; (foo '(a b c d e f g) '())
; => (a b c d e f g)
; (foo '() '(aa bb cc dd ee ff gg))
; => (aa bb cc dd ee ff gg)

(define foo
  (λ (li1 li2)
  (oof li1 li2 1))) ; Pass the parameters to another function with a starting value of one

(define oof
  (λ (li1 li2 x)
  (cond
    ((and (null? li1) (null? li2)) '()) ; If both lists are null, return an empty list
    ((null? li1) li2) ; If the first is null, return the second
    ((null? li2) li1) ; If the second is null, return the first
    ((> x (length li1)) li1) ; If the x value is out of the bounds of li1, return li 1
    (else
      (append (append (take li1 x) (oof li2 (drop li1 x) (+ x 1)))))))) ; Otherwise, start a new list with the first x values of li1, and recurse with the lists swapped, x values removed from li1, and increment x

; ### 2017 January ### ;
; Define a Scheme function foo which finds all atoms inside an sexpression which pass a given predicate.
; Examples:
; (foo number? '(a (2 (c 3) 4)))
; => (2 3 4)
; (foo symbol? '(a (2 (c 3) 4)))
; => (a c)
; (foo symbol? 'a)
; => (a)
; (foo number? 'a)
; => ()

(define foof
  (λ (p li)
  (filter p (flatten li)))) ; Flatten the inner lists, filter the results by the predicate
