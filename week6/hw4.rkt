
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
;; put your code below

;1
(define (sequence low high stride)
    (if (> low high)
    null
    (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
   (map (lambda (s)(string-append s suffix)) xs) 
)

(define (list-nth-mod xs n)
    (if (< n 0)
        (error "list-nth-mod: negative number")
        (if (null? xs)
            (error "list-nth-mod: empty list")
            (car (list-tail xs (remainder n (length xs))))
        )
    )
)

(define (stream-for-n-steps s n)
    (if (= n 0)
    null
    (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1))))
    )

(define funny-number-stream
    (letrec ([f (lambda (x) 
        (if (= (remainder x 5) 0)
            (cons (- 0 x) (lambda () (f (+ x 1))))
            (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

; (define dan-then-dog 
;     (letrec ([f (lambda (s) 
;         (if (string=? s "dan")
;         (cons (string-append s ".jpg") (lambda () (f "dog")))
;         (cons (string-append s ".jpg") (lambda () (f "dan")))))])
;     (lambda () (f "dan"))
;     ))

(define dan-then-dog 
    (letrec ([f (lambda (s) 
        (cons (string-append s ".jpg") (lambda () (
                (if (string=? s "dan") 
                (lambda () (f "dog"))
                (lambda () (f "dan")))))))])
    (lambda () (f "dan"))
    ))

(define (stream-add-zero s)
    (letrec ([f (lambda (xs) (
        cons (cons 0 (car (xs))) (lambda () (f (cdr (xs))))
    ))])
    (lambda () (f s)))
)

(define (cycle-lists xs ys)
    (letrec ([f (lambda (n)
        (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1))))
    )])
    (lambda () (f 0))
))

(define (vector-assoc v vec)
    (letrec ([f (lambda (pos) 
        (if (>= pos (vector-length vec))
        #f
        (if (equal? v (car (vector-ref vec pos)))
            (vector-ref vec pos)
            (f (+ pos 1))
        )))])
    (f 0))
)

(vector-assoc 6 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1)))

(define (cached-assoc xs n))