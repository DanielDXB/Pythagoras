#lang racket

(provide (all-defined-out))

(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


(define (dot-product X Y)
  ( if (empty? X)
       0
  (+ (* (first X) (first Y)) (dot-product (rest X) (rest Y)))))

(define (tail-recursion M V L)
  (if (empty? M)
      L
      (tail-recursion (rest M) V (append L (list (dot-product (first M) V))))))
  
(define (multiply M V)
  (tail-recursion M V '()))

(define (get-transformations-aux L)
  (cond
    ((empty? L) '())
    ((= (first L) 0) (cons 2 (get-transformations-aux (rest L))))
    ((= (first L) 1) (cons 3 (get-transformations-aux (rest L))))
    (else (cons 1 (get-transformations-aux (rest L))))))

(define (get-transformations-helper n M)
  (if (= n 1) M
      (get-transformations-helper (quotient (+ n 1) 3) (cons (modulo n 3) M))))

(define (get-transformations n)
  (get-transformations-aux (get-transformations-helper n '())))

(define (apply-matrix-transformations Ts ppt)
  (if (empty? Ts)
      ppt
  (cond
    ((= (first Ts) 1) (apply-matrix-transformations (rest Ts) (multiply T1 ppt)))
    ((= (first Ts) 2) (apply-matrix-transformations (rest Ts) (multiply T2 ppt)))
    (else (apply-matrix-transformations (rest Ts) (multiply T3 ppt)))
    )))

(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5)))