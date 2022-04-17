#lang racket

(provide (all-defined-out))

(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))

(define (dot-product X Y)
  (apply + (map * X Y)))

(define (multiply M V)
   (foldr (lambda (x acc) (apply list (apply + (map * x V)) acc))  null M))

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

(define (apply-functional-transformations Fs tuple)
    (foldl (lambda (x acc) (x acc)) tuple Fs))

(define (get-nth-tuple-helper function n L N1 N2 N3)
  (if (empty? L)
      n
      (cond
        ((= (first L) 3) (get-nth-tuple-helper function (function N3 n)(rest L) N1 N2 N3))
        ((= (first L) 2) (get-nth-tuple-helper function (function N2 n)(rest L) N1 N2 N3))
        (else (get-nth-tuple-helper function (function N1 n)(rest L) N1 N2 N3)))))
    
(define (get-nth-tuple function)
   (lambda (n)
    (lambda (L)
      (lambda (N1)
        (lambda (N2)
          (lambda (N3)
            (if (= n 1)
               L
               (get-nth-tuple-helper function L (get-transformations n) N1 N2 N3))))))))

(define (get-nth-ppt-from-matrix-transformations n)
  ((((((get-nth-tuple multiply) n) '(3 4 5)) T1) T2) T3))

(define (get-nth-quadruple n)
  ((((((get-nth-tuple apply) n) '(1 1 2 3)) Q1) Q2) Q3))

(define (get-nth-ppt-from-GH-quadruples-helper Q n acc)
  (if (zero? n)
      acc
  (cond
    ((= n 3) (get-nth-ppt-from-GH-quadruples-helper Q (- n 1) (cons (+ (* (first (rest Q)) (first (rest Q))) (* (first(rest(rest Q))) (first(rest(rest Q))))) acc)))
    ((= n 2) (get-nth-ppt-from-GH-quadruples-helper Q (- n 1) (cons (* 2 (first(rest Q)) (first(rest(rest Q)))) acc)))
    (else (get-nth-ppt-from-GH-quadruples-helper Q (- n 1) (cons (* (first Q) (first(rest(rest(rest Q))))) acc))))))
    
(define (get-nth-ppt-from-GH-quadruples n)
  (get-nth-ppt-from-GH-quadruples-helper (get-nth-quadruple n) 3 null))
