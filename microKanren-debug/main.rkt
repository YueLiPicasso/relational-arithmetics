#lang racket/base
(require racket/trace)
; microKanren

(provide (all-defined-out))

; logic variable and object

(define (var c) (vector c))

(define (var? x) (vector? x))

(define (var=? x1 x2)          
  (= (vector-ref x1 0) (vector-ref x2 0)))

(define (obj=? o1 o2) (eqv? o1 o2))

; unification

(define (walk u s)
  (let ((pr (and (var? u) (assf (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s) `((,x . ,v) . ,s))

(define (unify u v s)
  (let ((u (walk u s))(v (walk v s)))
    (cond
      ((and (var? u)(var? v)(var=? u v)) s)
      ((var? u)(ext-s u v s))
      ((var? v)(ext-s v u s))
      ((and (pair? u)(pair? v))
       (let ((s (unify (car u)(car v) s)))
         (and s (unify (cdr u)(cdr v) s))))
      (else (and (obj=? u v) s)))))

; four basic goal constructors

(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

(define (disj g1 g2)
  (lambda (s/c)
    (mplus (g1 s/c) (g2 s/c))))

(define (conj g1 g2)
  (lambda (s/c)
    (bind (g1 s/c) g2)))

; list monad (mzero, unit, mplus and bind)
; with capability for infinite stream
; and interleaved search

(define mzero '())

(define (unit s/c) (cons s/c mzero))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (delay (bind (force $) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))

(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1)(delay (mplus $2 (force $1)))) 
    (else (cons (car $1) (mplus $2 (cdr $1))))))   


; stream techniques highlight

(define-syntax delay ; suspension
  (syntax-rules ()
    ((delay e) (lambda () e))))

(define-syntax snooze ; the inverse-eta-delay goal constructor
  (syntax-rules ()
    ((snooze g)(lambda (s/c) (delay (g s/c))))))

(define-syntax force ; execute a delayed procedure
  (syntax-rules ()
    ((force g) (g))))


; auxiliaries

(define empty-state '(() . 0))

(define (take-inf n s-inf)
  (cond
    ((zero? n) '())
    ((null? s-inf) '())
    ((pair? s-inf)(cons (car s-inf) (take-inf (- n 1) (cdr s-inf))))
    ((procedure? s-inf)(take-inf n (force s-inf)))))

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v)
       (cons (walk* (car v) s)
             (walk* (cdr v) s)))
      (else v))))

(trace take-inf)