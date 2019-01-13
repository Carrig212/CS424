#lang racket
; ### 2016 Autumn ### ;
; Define a function tr which takes a list of lists, all of the same
; length, and returns their "transpose", meaning a list of lists of
; the first elements, the second elements, etc. (All lists in test
; cases can be assumed to be non-empty.)
; Examples:
; (tr '((1 2 3) (4 5 6)))
; => ((1 4) (2 5) (3 6))
; (tr '((f o x e s) (s o c k s) (r o c k s)))
; => ((f s r) (o o o) (x c c) (e k k) (s s s))

(define tr
  (λ (li)
  (cond
    ((null? (car li)) '()) ; If the inner lists are null, return an empty list
    (else (cons (map car li) (tr (map cdr li))))))) ; Otherwise, make a new list of lists, the first list being the head of each inner list, and then recurse with the tail of each inner list

; ### 2016 January ### ;
; Define a Scheme function reverse-with-count which takes two
; lists, the second of which is a list of non-negative integers the
; same length as the first list, and returns a list of elements from
; the first list, in reverse order, each repeated a number of times
; as specified by the corresponding element of the second list.
; Examples:
; (reverse-with-count '(a b c) '(1 2 3)) => (c c c b b a)
; (reverse-with-count '(d c b a) '(3 0 0 1)) => (a d d d)

(define reverse-with-count
  (λ (li ln)
  (cond
    ((null? li) '()) ; If the list is null, return an empty list
    (else
      (append (make-list (car (reverse ln)) (car (reverse li))) (reverse-with-count (reverse (cdr (reverse li))) (reverse (cdr (reverse ln))))))))) ; Create a new list with the last element of li, the last element of ln times, then recurse with everything but the last elements of each li and ln (Scheme has no "init" function like Haskell does, so this looks horrible)
