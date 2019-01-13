#lang racket
; ### 2015 Autumn ### ;
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

; ### 2015 January ### --
; Define a Scheme function after-filter which takes a predicate p
; and a list xs and returns a list of those elements of xs which
; immediately follow an element which passes the predicate p.
; Examples:
; (after-filter number? '(a b 2 3 c 4 d))
; => '(3 c d)
; (after-filter symbol? '(a b 2 3 c 4 d))
; => '(b 2 4)

(define after-filter
  (λ (p li)
  (cond
    ((or (null? li) (null? (cdr li))) '()) ; If the list or it's tail are null, return an empty list
    ((p (car li)) (cons (cadr li) (after-filter p (cdr li)))) ; If the head passes the predicate, add the element immediately after it to a new list and recurse with the tail of the list
    (else
      (after-filter p (cdr li)))))) ; Otherwise, just recurse with the rest of the list
