#lang racket

(define f (lambda (x) (x 5) (x 8)))

(f (lambda (k)(* k k)))

(+ (call/cc f) 3)

(define id (lambda (x) x))

(define (lazy* l)
  (lazy*-aux2 l id))

(define (mul a b)
  (begin
    (displayln (list a b))
    (* a b)))

(define (lazy*-aux l continuacao)
  (if (= (car l) 0) 0
      (if (null? (cdr l))
          (continuacao (car l))
          (lazy*-aux (cdr l) (lambda(n) (mul (car l) (continuacao n)))))))

(define (lazy*-aux2 l continuacao)
  (if (number? l)
      (if (= l 0)
          0
          (continuacao l))
      (if (null? (cdr l))
          (lazy*-aux2 (car l) continuacao)
          (lazy*-aux2 (car l) (lambda(p) (lazy*-aux2 (cdr l) (lambda (n) (continuacao (mul p n)))))))))