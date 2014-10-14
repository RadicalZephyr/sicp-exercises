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


;; Mapping over lists
(define nil '())

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;; Ex. 2.21

(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-listm items)
  (map square items))

;; Ex. 2.22

(define (square-listb items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

;; This doesn't work because in order to build a list, cons must take
;; an item, and then a list.  But Louis is giving the arguments as
;; (list item), thus making a reversed list.

(define (square-listb2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;; Ex. 2.23

(define (for-each f items)
  (cond [(null? items) true]
        [else (f (car items))
              (for-each f (cdr items))]))

;; (list 1 (list 2 (list 3 4)))
;; -> (1 (2 (3 4)))

;; Ex. 2.27

(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse tree)
  (define (rev-it accum items)
    (cond [(null? items)       accum]
          [(pair? (car items)) (rev-it (cons (deep-reverse (car items)) accum)
                                       (cdr items))]
          [else                (rev-it (cons (car items) accum)
                                       (cdr items))]))
  (rev-it nil tree))
