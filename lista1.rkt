#lang scheme

(define (sigma m n)
  (if (= m n) n
  (+ m (sigma (+ m 1) n)))
)

(define (exp m n)
  (if (= n 0) 1
  (* m (exp m (- n 1))))
)

(define (logg m n)
  (if (> m n) 0
  (+ 1 (logg m (/ n m))))
)

(define (fat n)
  (if (= n 0) 1
  (* n (fat (- n 1))))
)

(define (choose_roubado n k)
  (/ (fat n) (* (fat k) (fat (- n k))))
)

(define (choose n k)
  (if (= k 0) 1
  (if (= n k) 1
  (+ (choose (- n 1) k) (choose (- n 1) (- k 1)))))
)

(define (fib m)
  (if (<= m 1) 1
  (+ (fib (- m 1)) (fib (- m 2))))
)

(define (count x l)
  (if (null? l) 0
  (if (equal? (car l) x) (+ 1 (count x (cdr l)))
  (count x (cdr l))))
)

(define (countall x l)
  (if (null? l) 0
  (if (list? (car l)) (countall x (car l))
  (if (equal? (car l) x) (+ 1 (countall x (cdr l)))
  (countall x (cdr l)))))
)

(define (reverse l)
  (if (null? l) '()
  (append (reverse (cdr l)) (list (car l))))
)

(define (twist l)
  (if (null? l) '()
  (if (list? (car l)) (append (twist (cdr l)) (list (twist(car l))))
  (append (twist (cdr l)) (list (car l)))))
)

(define (flatten l)
  (if (null? l) '()
  (if (list? (car l)) (append (flatten (car l)) (flatten (cdr l)))
  (append (list(car l)) (flatten (cdr l)))))
)

(define (sublist l1 l2)
  (if (null? l1) #T
  (if (null? l2) #F
  (if (equal? (car l1) (car l2)) (sublist (cdr l1) (cdr l2))
  (sublist l1 (cdr l2)))))
)

(define (contig-sublist-in l1 l2)
  (if (null? l1) #t
  (if (null? l2) #f
  (if (equal? (car l1) (car l2)) (contig-sublist (cdr l1) (cdr l2))
  #f)))
)

(define (contig-sublist l1 l2)
  (if (null? l1) #t
  (if (null? l2) #f
  (if (equal? (car l1) (car l2)) (contig-sublist-in (cdr l1) (cdr l2))
  (contig-sublist l1 (cdr l2)))))
)

(define (remove x s)
  (if (null? s) '()
  (if (equal? x (car s)) (remove x (cdr s))
  (append (list (car s)) (remove x (cdr s)))))
)

(define (check x l)
  (if (null? l) #f
  (if (equal? x (car l)) #t
  (check x (cdr l))))
)

(define (subset l1 l2)
  (if (null? l1) #t
  (if (equal? (check (car l1) l2) #t) (subset (cdr l1) l2)
  #f))
)

(define (=set s1 s2)
  (if (equal? (subset s1 s2) #t)
  (if (equal? (subset s2 s1) #t) #t
  #f)
  #f)
)

(define mapcar (lambda (fun l)
  (if (null? l) null
  (cons (fun (car l))
  (mapcar fun (cdr l)))))
)

(define (cdr* l)
  (mapcar cdr l)
)

(define (max x y)
  (if (> x y) x
  y)
)

(define (quad x)
  (* x x)
)

(define combine (lambda (soma f zero)
               (lambda (lista)
                 (if (null? lista) zero
                     (soma (f (car lista))
                           ((combine soma f zero)(cdr lista)))))))

(define id (lambda (x) x))


(define max*
  (combine max id 0)
)

(define mapc (curry mapcar))

(define quad*
  (mapc quad)
)
