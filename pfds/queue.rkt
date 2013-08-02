#lang racket

;; standard functional queues

(provide empty-queue empty? enq head tail build-queue)

(struct queue (front rear))
(define empty-queue (queue null null))
(define (empty? q) (null? (queue-front q)))
(define (checkf f r) 
  (if (null? f) (queue (reverse r) null) (queue f r)))
(define (enq q x) (match q [(queue f r) (checkf f (cons x r))]))
(define (head q) 
  (match q [(queue f _) (if (null? f) (error 'head "empty queue") (car f))]))
(define (tail q)
  (match q [(queue f r) (if (null? f) (error 'tail "empty queue") (checkf (cdr f) r))]))
(define (build-queue n f)
  (let loop ([m 0] [q empty-queue])
    (if (= m n) q (loop (add1 m) (enq q (f m))))))