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
  (oof li1 li2 1)))

(define oof
  (λ (li1 li2 x)
  (cond
    ((and (null? li1) (null? li2)) '())
    ((null? li1) li2)
    ((null? li2) li1)
    ((> x (length li1)) li1)
    (else
      (append (append (take li1 x) (oof li2 (drop li1 x) (+ x 1))))))))
