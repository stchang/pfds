#lang racket
(require "views.rkt")

;; standard functional queues

(provide empty-queue empty? enq head tail head+tail build-queue)

(struct queue (front rear))
(define empty-queue (queue null null))
(define (empty? q) (null? (queue-front q)))
(define (checkf f r) 
  (if (null? f) (queue (reverse r) null) (queue f r)))
(define (enq q x) (match q [(queue f r) (checkf f (cons x r))]))
(define (head q) 
  (match q [(queue f _) (if (null? f) (error 'head "empty queue") (car f))]))
(define (tail q)
  (match q 
    [(queue f r) 
     (if (null? f) 
         (error 'tail "empty queue") 
         (checkf (cdr f) r))]))
(define (head+tail q)
  (match q 
    [(queue f r) 
     (if (null? f)
         (error 'head+tail "empty queue")
         (values (car f) (checkf (cdr f) r)))]))
(define (build-queue n f)
  (let loop ([m 0] [q empty-queue])
    (if (= m n) q (loop (add1 m) (enq q (f m))))))

(provide (rename-out [queue-view queue]))



;(define-match-expander queue-view 
;  (syntax-rules ()
;    [(_ hd tl) (app head+tail-as-cons (cons hd tl))]))

;(define-match-expander queue-view 
;  (syntax-rules ()
;    [(_ hd tl) (app (λ (q) (list (head q) (tail q))) (list hd tl))]))

;(define-view (queue-view hd tl) (head tail))
(define (head+tail-as-list q)
  (define-values (h t) (head+tail q))
  (list h t))
(define-view (queue-view hd tl) head+tail-as-list);
;(define (queue-view x ...) queue-rear) ; this works, but only if the accessor is an identifier
; this does not work
;(define-view (queue-view x ...) (λ (q) (append (queue-front q) (reverse (queue-rear q)))))