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

(define ^
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else (x a (^ a (sub1 b)))))))

(define /
  (lambda (a b)
    (cond
      ((< a b) 0)
      (else (add1 (/ (- a b) b))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat))
       (no-nums (cdr lat)))
      (else (cons (car lat)
                  (no-nums (cdr lat)))))))

; (no-nums '(5 pears 6 prunes 9 dates))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat))
       (cons (car lat)
             (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

; (all-nums '(5 pears 6 prunes 9 dates))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1)
            (number? a2))
       (= a1 a2))
      ((or (number? a1)
           (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat))
       (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

; (occur 'one '(one two one two three four one six))

(define one? (lambda (n) (= n 1)))

(define rempick-one
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
                  (rempick-one (sub1 n) (cdr lat)))))))

; (rempick-one 3 '(lemon meringue pie))

(define rember*
  (lambda (a l)
    (cond
      ((empty? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eqan? a (car l))
          (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else
       (cons (rember* a (car l))
             (rember* a (cdr l)))))))

;(rember* 'cup '((coffee)
;                cup
;                ((tea)
;                 cup)
;                (and
;                 (hick))
;                cup))


;(rember* 'sauce '(((tomato sauce))
;                  ((bean) sauce)
;                  (and ((flying)) sauce)))

(define insertR*
  (lambda (new old l)
    (cond
      ((empty? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eqan? old (car l))
          (cons old
                (cons new
                      (insertR* new old (cdr l)))))
         (else (cons (car l)
                     (insertR* new old (cdr l))))))
      (else
       (cons (insertR* new old (car l))
             (insertR* new old (cdr l)))))))

;(insertR* 'roast 'chuck '((how much (wood))
;                          could
;                          ((a (wood) chuck))
;                          (((chuck)))
;                          (if (a) ((wood chuck)))
;                          could chuck wood))

(define occur*
  (lambda (a l)
    (cond
      ((empty? l) 0)
      ((atom? (car l))
       (cond
         ((eqan? a (car l))
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else
       (+ (occur* a (car l))
          (occur* a (cdr l)))))))

;(occur* 'banana '((banana)
;                  (split ((((banana ice)))
;                          (cream (banana))
;                          sherbet))
;                  (banana)
;                  (bread)
;                  (banana brandy)))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new
                (subst* new old (cdr l))))
         (else (cons (car l)
                     (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))

;(subst* 'orange 'banana '((banana)
;                  (split ((((banana ice)))
;                          (cream (banana))
;                          sherbet))
;                  (banana)
;                  (bread)
;                  (banana brandy)))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
         (or (eq? (car l) a)
             (member* a (cdr l))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))

;(member* 'chips '((potato)
;                  (chips ((with) fish) (chips))))

; leftmost ONLY needs to recur on the car
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

;(leftmost '((potato)
;            (chips ((with) fish) (chips))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (equal? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (l1 l2)
    (cond
      ((and (atom? l1)
            (atom? l2))
            (eqan? l1 l2))
      ((or (atom? l1) (atom? l2)) #f)
      (else (eqlist? l1 l2)))))

; ==========
; 6 SHADOWS
; ==========

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))

;(numbered? '(1 + (4 x 5)))
;(numbered? '(1 + (4 toast 5)))
;(numbered? '(sausage + 12))


(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) (quote +))
       (+ (car nexp)
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote x))
       (+ (car nexp)
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote ^))
       (^ (car nexp)
          (value (car (cdr (cdr nexp))))))
      (else 'naw))))

;(value 13) ; 13
;(value '(1 + 3)) ; 4
;(value '(1 + (3 ^ 4))) ; 82
;(value 'cookie) ; no answer

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

; (1st-sub-exp '(+ 1 3))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

; (2nd-sub-exp '(+ 1 3))

(define operator
  (lambda (aexp)
    (car aexp)))

; (operator '(+ 1 3))

(define value-higher-order
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (+ (1st-sub-exp nexp)
          (2nd-sub-exp nexp)))
      ((eq? (operator nexp) (quote x))
       (x (1st-sub-exp nexp)
          (2nd-sub-exp nexp)))
      ((eq? (operator nexp) (quote ^))
       (^ (1st-sub-exp nexp)
          (2nd-sub-exp nexp)))
      (else 'naw))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

; ==========
; 7 FRIENDS AND RELATIONS
; ==========

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

; (set? '(apple peaches apple plum))
; (set? '(apple peaches pear plum))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else (cons (car lat)
                  (makeset (cdr lat)))))))

; (makeset '(apple peach pear peach plum apple lemon peach))

(define makeset-multirember
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat)
                  (makeset-multirember (multirember (car lat) (cdr lat))))))))

; (makeset-multirember '(apple peach pear peach plum apple lemon peach))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
                 (subset? (cdr set1) set2))))))

;(subset? '(5 chicken wings)
;         '(5 hamburgers
;           2 pieces fried chicken and
;           light duckling wings))

;(subset? '(4 pounds of horseradish)
;         '(four pounds chicken and 5 ounces horseradish))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

; (eqset? '(6 large chickens with wings) '(6 chickens with large wings))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))))))

; (intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2) (cons (car set1)
                                       (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

;(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))

; note: we recur through, first adding all atoms from set1 that are *not*
; in set2, then when set1 is empty, we append all of set2, thus including
; all atoms from set2
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))

;(union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set)))))))

;(intersectall '((a b c) (c a b d e) (e f g h a b)))
;(intersectall '((6 pears and)
;                (3 peaches and 6 peppers)
;                (8 pears and 6 plums)
;                (and 6 prunes with some apples)))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

;(a-pair? '(3 7))
;(a-pair? '(3 7 4))
;(a-pair? '((1) (2)))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2 (quote ())))))

(define revpair
  (lambda (pair)
    (build (second pair)
           (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons (revpair (car rel))
                  (revrel (cdr rel)))))))

;(revrel '((8 a) (pumpkin pie) (got sick)))

; ==========
; CH 8 LAMBDA THE ULTIMATE
; ==========

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      ((test? (car l) a) (cdr l))
      (else (cons (car l)
                  (rember-f test?
                            a
                            (cdr l)))))))

; currying...
;(define rember-f
;  (lambda (test?)
;    (lambda (a l)
;      (cond ...))))

;(rember-f = '5 '(6 2 5 3))
;(rember-f eq? '5 '(6 2 5 3))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new
           (cons old l)))))

; can do same as insertL for insertR (previously written above)

; can also do same for subst fn, also previously written above... but with
(define seqS
  (lambda (new _ l)
    (cons new l)))
; passed to insert-g as the seq fn

; same with rember and...
(define seqrem
  (lambda (new old l) l))

; neat-o!

; use col (a "collector" fn, or "continuation") to separate atoms in into one list or another based
; on whether or not they pass the condition (eq?, in this case). 
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col (quote ()) (quote())))
      ((eq? (car lat) a)
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define last-friend
  (lambda (x y)
    (length x)))

;(multirember&co 'tuna '(tuna and something) a-friend)
;(multirember&co 'tuna '(tuna and banana something) last-friend)

; NOTE: build functions (i.e. collector) to collect more than one value at a time!

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l)
       (col (quote ()) 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                                 (x (car l) p)
                                 s))))
         (else
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col newl p
                                 (+ (car l) s)))))))
      (else
       (evens-only*&co (car l)
                       (lambda (al ap as)
                         (evens-only*&co (cdr l)
                                         (lambda (dl dp ds)
                                           (col (cons al dl)
                                                (x ap dp)
                                                (+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product
                newl))))

;(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)

; ==========
; CH 9 ...AND AGAIN, AND AGAIN, AND AGAIN,...
; ==========

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else (eq? a sorn)))))

;(looking 'caviar '(6 2 4 caviar 5 7 3))
;(looking 'caviar '(6 2 grits caviar 5 7 3))

; looking is a "partial" (in scheme terminology) function, in that it doesn't always get closer to its goal
; when recursing. Other functions are "total"

; i.e. (define eternity (lambda (x) (eternity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

;(shift '((a b) c)) ; (a (b c))
;(shift '((a b) (c d))) ; (a (b (c d)))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (length* (first pora))
          (length* (second pora)))))))

;(length* '((a b) (c d)))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (x (weight* (first pora)) 2)
          (weight* (second pora)))))))

;(weight* '((a b) c))

; ...
; coming back to rest of chapter to fully comprehend this and Y-Combinator
; ...

; ==========
; CH 10 WHAT IS THE VALUE OF ALL OF THIS?
; ==========

(define new-entry build)

;(new-entry
; '(appetizer entree beverage)
; '(pate boeuf vin))

;(new-entry
; '(beer beer beer)
; '(beer beer beer))

(define lookup-in-entry-mine
  (lambda (name entry)
    (cond
      ((null? entry) (quote ()))
      ((eq?
        (first (first entry))
        name) (first (second entry)))
      (else
       (lookup-in-entry
        name
        (build (cdr (first entry))
               (cdr (second entry))))))))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name)
       (car values))
      (else (lookup-in-entry-help
             name
             (cdr names)
             (cdr values)
             entry-f)))))

(define lookup-in-entry
  (lambda (name entry entry-f)
         (lookup-in-entry-help
          name
          (first entry)
          (second entry)
          entry-f)))

(define not-found
  (lambda (name)
    "not found"))

;(lookup-in-entry 'entdddree
;                 (build '(appetizer entree beverage)
;                        '(food tastes good))
;                 not-found)

(define extend-table cons)

; table-fn is used if lookup-in-entry does not find the current
; entry... it recurs over lookup-in-table again, with cdr of table
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
       (lookup-in-entry
        name
        (car table)
        (lambda (name)
          (lookup-in-table
           name
           (cdr table)
           table-f)))))))

(lookup-in-table
 'entree
 '(((entree dessert)
    (spaghetti spumoni))
   ((appetizer entree beverage)
    (food tastes good)))
 (lambda (name)
   "Not Found"))