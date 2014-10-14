#lang racket

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (lengthi items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; Ex. 2.17
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

;; Ex. 2.18

(define (reverse items)
  (define (rev-it accum items)
    (if (null? items)
        accum
        (rev-it (cons (car items) accum) (cdr items))))
  (rev-it '() items))


;; Ex. 2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination values)
  (car values))

(define (except-first-denomination values)
  (cdr values))

(define (no-more? values)
  (null? values))

;; The order of coin-values doesn't matter.  This is unexpected.  My
;; intuition was that the order should matter, but I guess because the
;; recursion proceeds to the point of using smallest coins only,
;; putting the smallest coins first doesn't make any difference.
