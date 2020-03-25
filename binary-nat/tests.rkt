#lang racket/base

(map-walk*
 (take-inf 1
           ((call/fresh
             (lambda(b)
               (gt1 b))) empty-state)))

(map-walk*
 (take-inf 10
           ((call/fresh
             (lambda(b)
               (genb b))) empty-state)))

(map-walk*
 (take-inf -1
           ((call/fresh
             (lambda(r)
               (call/fresh
                (lambda(cin)
                  (call/fresh
                   (lambda(x)
                     (call/fresh
                      (lambda(y)
                        (call/fresh
                         (lambda(sum)
                           (call/fresh
                            (lambda(cot)
                              (conj (== r `(,cin ,x ,y ,sum ,cot))
                                    (full-adder cin x y sum cot))))))))))))))empty-state)))