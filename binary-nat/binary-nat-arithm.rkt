#lang racket/base

#;(require microKanren-debug)
(require microKanren)

(define-syntax map-walk*
  (syntax-rules ()
    ((_ state-stream)
     (map (lambda (s/c) (walk* (var 0) (car s/c))) state-stream))))

; arithm

(define (zero b)(== b '()))

(define (pos b)
  (call/fresh
   (lambda(lsb)
     (call/fresh
      (lambda(bs)
        (== b `(,lsb . ,bs)))))))

(define (gt1 b)
  (call/fresh
   (lambda(x)
     (call/fresh
      (lambda(y)
        (call/fresh
         (lambda(z)
           (== b `(,x  ,y . ,z)))))))))

(define (genb b)
  (disj (== b '())
        (disj (call/fresh
               (lambda(x)
                 (conj (== b `(1 . ,x))
                       (snooze (genb x)))))
              (call/fresh
               (lambda(x)
                 (conj (== b `(0 . ,x))
                       (conj (pos x)
                             (snooze (genb x)))))))))

; bit full-adder,taking a carry-in bit,and two bits to add,
; returning a sum bit and a carry-out bit 
(define (full-adder cin x y sum cot)
  (disj (conj (== cin 0);clause 1
              (conj (== x 0)
                    (conj (== y 0)
                          (conj (== sum 0)
                                (== cot 0)))))
        (disj (conj (== cin 0);clause 2
                    (conj (== x 0)
                          (conj (== y 1)
                                (conj (== sum 1)
                                      (== cot 0)))))
              (disj (conj (== cin 0);clause 3
                          (conj (== x 1)
                                (conj (== y 0)
                                      (conj (== sum 1)
                                            (== cot 0)))))
                    (disj (conj (== cin 0);clause 4
                                (conj (== x 1)
                                      (conj (== y 1)
                                            (conj (== sum 0)
                                                  (== cot 1)))))
                          (disj (conj (== cin 1);clause 5
                                      (conj (== x 0)
                                            (conj (== y 0)
                                                  (conj (== sum 1)
                                                        (== cot 0)))))
                                (disj (conj (== cin 1);clause 6
                                            (conj (== x 0)
                                                  (conj (== y 1)
                                                        (conj (== sum 0)
                                                              (== cot 1)))))
                                      (disj (conj (== cin 1);clause 7
                                                  (conj (== x 1)
                                                        (conj (== y 0)
                                                              (conj (== sum 0)
                                                                    (== cot 1)))))
                                            (conj (== cin 1);clause 8
                                                  (conj (== x 1)
                                                        (conj (== y 1)
                                                              (conj (== sum 1)
                                                                    (== cot 1)))))))))))))