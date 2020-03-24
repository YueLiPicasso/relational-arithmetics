#lang racket/base

#;(require microKanren-debug)
(require microKanren)

(define-syntax map-walk*
  (syntax-rules ()
    ((_ state-stream)
     (map (lambda (s/c) (walk* (var 0) (car s/c))) state-stream))))

; arithm

(define (genu n)
  (disj (== n '())
        (call/fresh
         (lambda(m)
           (conj (== n `(u . ,m))(snooze (genu m)))))))
; note the snooze above
; otherwise evaluation of genu would not terminate 

(define (add x y z)
  (disj (conj (== x '())(== y z))
        (call/fresh
         (lambda(t)
           (call/fresh
            (lambda(ty)
              (conj (== x `(u . ,t))
                    (conj (== z `(u . ,ty))
                          (snooze (add t y ty))))))))))


(define (mult x y z)
  (disj (conj (== x '())(== z '()))
        (call/fresh
         (lambda(t)
           (call/fresh
            (lambda(t*y)
              (conj (== x `(u . ,t))
                    (conj (add y t*y z)
                          (snooze (mult y t t*y))))))))))


(map-walk*
 (take-inf 2 ((call/fresh
               (lambda(x)
                 (mult x '(u) '(u)))) empty-state)))

(map-walk*
 (take-inf 2 ((call/fresh
               (lambda(x)
                 (mult x '(u u) '(u u u u u)))) empty-state)))

(map-walk*
 (take-inf 2 ((call/fresh
               (lambda(x)
                 (mult x '(u u) '(u u u u u u)))) empty-state)))


(map-walk*
 (take-inf 100 ((call/fresh
                 (lambda(x)
                   (call/fresh
                    (lambda(y)
                      (call/fresh
                       (lambda(z)
                         (conj (== x `(,y ,z))
                               (mult  y z '(u u u u u u)))))))))
                empty-state))) 

(map-walk*
 (take-inf 20 ((call/fresh
                 (lambda(p)
                   (call/fresh
                    (lambda(x)
                      (call/fresh
                       (lambda(y)
                         (call/fresh
                          (lambda(z)
                            (conj (== p `(,x ,y ,z))
                                  (mult x y z)
                                  )))))))))
                empty-state))) 
