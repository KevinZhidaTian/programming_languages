
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
        (cond [(>= pos (vector-length vec)) #f]
            [(not (pair? (vector-ref vec pos))) (f (+ pos 1))]
            [(equal? v (car (vector-ref vec pos))) (vector-ref vec pos)]
            [#t (f (+ pos 1))]))])
    (f 0))
)

(define (cached-assoc xs n)
    (letrec ([cache (make-vector n #f)]
            [slot 0]
            [f (lambda (v)
                    (let ([ans (vector-assoc v cache)])
                        (if ans
                            ans
                            (let ([new-ans (assoc v xs)])
                                (if new-ans
                                    (begin
                                        (vector-set! cache slot new-ans)
                                        (set! slot (remainder (+ slot 1) n))
                                        new-ans)
                                    #f))
                        )
                    )
                )
            ])
    f)
)

(define-syntax while-less
    (syntax-rules (do)
        [(while-less e1 do e2)
        (let ([condition e1])
            (letrec ([f (lambda () 
                        (begin
                            (let ([result e2])
                                (if (< result condition) (f) #t))
                            )
                        )
                    ])
            (f))
        )
        ]
    )
)