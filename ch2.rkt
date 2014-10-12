#lang racket

(define (fast-exp-it b n)
  (fast-exp-iter 1 b n))


(define (fast-exp-iter a b n)
  (cond [(= n 0) a]
        [(even? n) (fast-exp-iter a (square b) (/ n 2))]
        [else (fast-exp-iter (* a b) b (dec n))]))

(define (square x)
  (* x x))

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
