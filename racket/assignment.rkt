#lang racket
; ### 2018 Assignment ### ;
; Let us represent the polynomial
; p(x) = a0 + a1 x + a2 x2 + ... + an xn
; as a list of coefficients
; (a0 a1 ... an)
; Note that the constant coefficient comes first.
; (The coefficients are just numbers, nothing fancy like symbolic expressions.)
; We will refer to such a list of coefficients below as a polynomial.

; ### Question 1 ### ;
; Define poly-eval where (poly-eval p x) evaluates the polynomial p at the point x.
; Example:
; (poly-eval '(1 2 3) 100) => 30201
; (poly-eval '() 7) => 0

(define poly-eval
  (λ (p x)
  (eval-poly p x 1))) ; Pass the parameters to a second function with a starting value of 1

(define eval-poly
  (λ (p x xx)
  (cond
    ((null? p) 0) ; If the list is null, return zero
    (else
      (+ (* (car p) xx) (eval-poly (cdr p) x (* xx x))))))) ; Otherwise, sum the result of multiplying the first element of the list by the xx value, and recurse with the tail of the list, and multiply xx by x

; ### Question 2 ### ;
; Define poly-mul, a polynomial addition routine, so (poly-mul p1 p2)
; returns the product of two polynomials.
; This would have the property that
; (poly-eval (poly-mul p1 p2) x) = (* (poly-eval p1 x) (poly-eval p2 x))

(define poly-mul
  (λ (p1 p2 x)
    (* (poly-eval p1 x) (poly-eval p2 x)))) ; Multiply the results of evaluating each polynomial

; ### Question 3 ### ;
; Define poly-diff, a derivative-taking routine, where (poly-diff p)
; returns the polynomial which is the derivative of the one it is passed.
; Example:
; (poly-diff '(1 10 100 1000 10000)) => (10 200 3000 40000)
; (poly-diff '(7)) => ()

(define poly-diff
  (λ (p)
  (diff-poly (cdr p) 1))) ; Pass the tail of the polynomial to a second function with a starting value of 1

(define diff-poly
  (λ (p x)
  (cond
    ((null? p) '()) ; If the list is null, return an empty list
    (else
      (cons (* (car p) x) (diff-poly (cdr p) (+ x 1))))))) ; Otherwise, start a list with the head of the polynomial multiplied by x, and recurse with the tail of the polynomial and the x value incremented

; ### Question 4 ### ;
; Define poly-int, an integration routine, where (poly-int p) returns
; the anti-derivative of the polynomial p, with a constant term of zero.
; This would have the property that
; (poly-diff (poly-int p)) = p
; for any polynomial p
; Example:
; (poly-int '(10 200 3000 40000)) => (0 10 100 1000 10000)
; (poly-int '()) => ()

(define poly-int
  (λ (p)
  (cons '0 (cons (car p) (int-poly (cdr p) 2))))) ; Start a list with zero, followed by the firs element of p, then pass the tail of p to a second function with a starting value of 2

(define int-poly
  (λ (p x)
  (cond
    ((null? p) '()) ; If the list is null, return an empty list
    (else
      (cons (/ (car p) x) (int-poly (cdr p) (+ x 1))))))) ; Otherwise, start a list with the head divided by x, and recurse with the tail of the list and an incremented x

; ### Question 5 ### ;
; Define grovel-poly-eval where (grovel-poly-eval s x) takes an s-expression s
; and a number x, and throughout s (even if deeply nested) replaces any list
; that begins with the symbol poly with the result of poly-eval of the remainder
; of that list at the given point x. You can assume that the remainder of a list
; beginning with the symbol poly is a valid polynomial.
; Example:
; (grovel-poly-eval '(the (poly 1 2) brown (fox (poly 7 0 1) jumps (poly) over)) 5)
; => (the 11 brown (fox 32 jumps 0 over))

(define grovel-poly-eval
  (λ (s x)
  (cond
    ((null? s) '()) ; If the s expression is null, returm an empty list
    ((list? (car s)) (cons (grovel-poly-eval (car s) x) (grovel-poly-eval (cdr s) x))) ; If the head of s is a list, start a list by recursing into that list, then again with the tail of that list
    ((equal? (car s) 'poly) (if (null? (cdr s)) '0 (poly-eval (cdr s) x))) ; If the head of the list is "poly", check for paramaters. If there's none, return 0. Otherwise, return the evaluated polynomial
    (else
      (cons (car s) (grovel-poly-eval (cdr s) x)))))) ; Otherwise, recurse with the tail of the s expression
