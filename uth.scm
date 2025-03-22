;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Chapter 10 from The Reasoned Schemer
;; Licensed under MIT

(define (var name) (vector name))

(define (var? x) (vector? x))

(define u (var 'u))
(define v (var 'v))
(define w (var 'w))
(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

(define (walk v s)
  (let ((a (and (var? v) (assv v s))))
    (cond
     ((pair? a) (walk (cdr a) s))
     (else v))))

(define (ext-s x v s)
  (cond
   ((occurs? x v s) #f)
   (else (cons `(,x . ,v) s))))

(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) (eqv? v x))
     ((pair? v)
      (or (occurs? x (car v) s)
	  (occurs? x (cdr v) s)))
     (else #f))))
