#lang racket
(require "../queue.rkt")
(require rackunit)

;; sums front n elements of a q
(define (sum-front-n q n)
  (let loop ([m 0] [q q])
    (if (= m n) 0 (+ (head q) (loop (add1 m) (tail q))))))

(let* ([q-size 1000]
       [q-f add1]
       [q (build-queue q-size q-f)])
  (let loop ([n 0])
    (unless (= n q-size)
      (check-equal? (for/sum ([i n]) (q-f i))
                    (sum-front-n q n))
      (loop (add1 n)))))