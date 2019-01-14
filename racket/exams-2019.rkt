#lang racket
; ### 2019 January ### ;
; Define a scheme function SCATTER-GATHER which takes two
; arguments, a list INDICIES of indicies and a list VALS of values,
; and returns a list the same length as INDICIES but with each
; value K replaced by the K-th element of VALS, or if that is out of
; range, by #f.
; Example:
; (scatter-gather '(0 1 4 1 1 7 2) '(a b c d e))
; => (a b e b b #f c)

(define scatter-gather
  (λ (indicies vals)
  (cond
    ((or (null? indicies) (null? vals)) '())
    ((or (> (car indicies) (length vals)) (< (car indicies) 0)) (cons '#f (scatter-gather (cdr indicies) vals)))
    (else
      (cons (list-ref vals (car indicies)) (scatter-gather (cdr indicies) vals))))))
