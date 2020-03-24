#lang racket/base

((genu '()) empty-state)
((genu '(u u)) empty-state)
((call/fresh (lambda(x)(genu x))) empty-state)
(take-inf 10 ((call/fresh (lambda(x)(genu x))) empty-state))
(map (lambda (s/c) (walk* (var 0) (car s/c)))
(take-inf 10 ((call/fresh (lambda(x)(genu x))) empty-state)))
(map-walk* (take-inf 10 ((call/fresh (lambda(x)(genu x))) empty-state)))

(map-walk*
(take-inf 5
((call/fresh
 (lambda(p)
   (call/fresh
     (lambda(x)
       (call/fresh
        (lambda(y)
          (call/fresh
           (lambda(z)
             (conj (== p `(,x ,y ,z))(add x y z)))))))))) empty-state)))

(map-walk*
(take-inf 5
((call/fresh
 (lambda(y)
   (add '(u) '(u u) y))) empty-state)))

(map-walk*
(take-inf 5
((call/fresh
 (lambda(y)
   (add '(u) y '(u u u)))) empty-state)))

((call/fresh
 (lambda(y)
   (add y '(u u u) '(u)))) empty-state)

(take-inf 1
((call/fresh
 (lambda(y)
   (add y '(u u u) '(u)))) empty-state))

#;(map-walk*
(take-inf 5
((call/fresh
 (lambda(y)
   (add y '(u u u) '(u)))) empty-state)))

