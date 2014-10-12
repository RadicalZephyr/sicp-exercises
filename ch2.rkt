#lang racket

(provide fast-exp-it
         fast-mult
         fast-mult-it)

(define (fast-exp-it b n)
  (fast-exp-iter 1 b n))


(define (fast-exp-iter a b n)
  (cond [(= n 0) a]
        [(even? n) (fast-exp-iter a (square b) (/ n 2))]
        [else (fast-exp-iter (* a b) b (dec n))]))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))


(define (fast-mult a b)
  (cond [(= b 1) a]
        [(even? b) (double (fast-mult a (halve b)))]
        [else (+ a (fast-mult a (dec b)))]))

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))


(define (fast-mult-it x y)
  (fast-mult-iter 0 x y))

(define (fast-mult-iter a x y )
  (cond [(= y 0) a]
        [(even? y) (fast-mult-iter a (double x) (halve y))]
        [else (fast-mult-iter (+ a x) x (dec y))]))

(define (double- f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter l f n)
    (if (= n 0)
        l
        (repeated-iter (compose f l) f (dec n))))
  (repeated-iter identity f n))
