#lang racket
(provide define-view)
(define-syntax define-view
  (syntax-rules ()
    [(_ (view-name field ...) (accessor ...))
     (define-match-expander view-name
       (syntax-rules ()
         [(_ field ...) (app (λ (x) (list (accessor x) ...)) (list field ...))]))]
    [(_ (view-name field ...) multi-accessor)
     (define-match-expander view-name
       (syntax-rules ()
         [(_ field ...) (app multi-accessor (list field ...))]))]))