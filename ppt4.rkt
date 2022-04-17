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

(define ppt-stream-in-tree-order
  (let loop ((lista (stream-cons '(3 4 5) empty-stream)))
    (let* ((V (stream-first lista)) (t1 (multiply T1 V)) (t2 (multiply T2 V)) (t3 (multiply T3 V))
                                    (aux (let function ((t 3) (lista-aux empty-stream))
                                              (cond
                                                ((= t 1) (function (- t 1) (stream-cons t1 lista-aux)))
                                                ((= t 2) (function (- t 1) (stream-cons t2 lista-aux)))
                                                ((= t 3) (function (- t 1) (stream-cons t3 lista-aux)))
                                                (else lista-aux)))))
      (stream-cons (stream-first lista) (loop (stream-rest (stream-append lista aux)))))))

(define (pairs G H)
  (let loop ((right H) (j 1))
    (let loop-aux ((left G) (i j))
      (if (= i 0) (loop (stream-rest right) (+ j 1))
          (stream-cons (cons (stream-first left) (stream-first right)) (loop-aux (stream-rest left) (- i 1)))))))
