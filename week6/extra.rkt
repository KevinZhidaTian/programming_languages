#lang racket

(define (palindromic lst)
    (letrec ([mid (floor (/ (length lst) 2))]
        [f (lambda (i result)
        (cond 
            [(= (length lst) 1) lst]
            [(= i (length lst)) result]
            [(> (remainder (length lst) 2) 0) (f (+ i 1) (append result (list (list-ref lst mid))))]
            [#t (f (+ i 1) (append result (list (+ (list-ref lst i) (list-ref lst (- (- (length lst) 1) i))))))])
        )])
    (f 0 null))
)

; (palindromic (list 1 2 3))

(define fibonacci
    (letrec ([f (lambda (i)
                (cond [(= i 0) (cons 0 (f (+ i 1)))]
                    [(= i 1) (cons 1 (f (+ i 1)))]
                    [#t (cons (+ (f (- i 1))(f (- i 2))) (f (+ i 1)))])
                )])
    (f 0))
)

; (car (cdr (fibonacci)))
(define ones (lambda () (cons 1 ones)))
; (car (ones))
(define (stream-for-n-steps s n)
    (if (= n 0)
    null
    (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1))))
    )

(for ([elem (in-stream ones)])
  (displayln elem))

; (stream-for-n-steps ones 2)