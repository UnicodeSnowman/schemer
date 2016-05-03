#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l))
       (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
       ((null? lat) #f)
       (else (or (eq? (car lat) a)
                 (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a)
       (cdr lat))
      (else (cons
        (car lat)
        (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((or (eq? (car lat) o1) (eq? (car lat) o2))
       (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertR new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat)
                  (multisubst new old (cdr lat)))))))

; take a and add 1 to it b times
(define +
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add1 (+ a (sub1 b)))))))

; take b and subtract 1 from it b times
(define -
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub1 (- a (sub1 b)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup)
               (addtup (cdr tup)))))))

(define x
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else (+ a (x a (sub1 b)))))))

; if tup1 is null on any iteration, just append tup2 to the existing list
; if tup2 is null...
; tup+ must always return a list, so this makes sense
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons
             (+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

; ensuring that (zero? a) case appears first, we can assert that invocations where (eq? a b) returns #t
; will result in a #f return value, since any time a reaches 0, we know we have a falsy end condition
(define >
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)
      (else (> (sub1 a) (sub1 b))))))

(define <
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)
      (else (< (sub1 a) (sub1 b))))))

; longer form version of this also an option: check if both a and b are 0, then check if a is 0,
; otherwise recur, subtracting 1 from a and b
; = used for testing number equality, eq? for others
(define =
  (lambda (a b)
    (cond
      ((> a b) #f)
      ((< a b) #f)
      (else #t))))