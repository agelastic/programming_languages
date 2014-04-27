
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; problem 1

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; problem 2

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; problem 3

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; problem 4

(define (stream-for-n-steps s n)
  (if (<= n 0)
      null
      (let ([streamval (s)]) 
        (cons (car streamval) (stream-for-n-steps (cdr streamval) (- n 1))))))

; problem 5
; a straightforward application of "stream-maker" pattern from the lecture

(define funny-number-stream
  (letrec ([f (lambda (x) 
                (if (equal? (remainder x 5) 0)
                    (cons (- 0 x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

; problem 6

(define dan-then-dog
  (letrec ([f (lambda (x)
                (cons x (lambda () 
                          (f (if (equal? "dan.jpg" x) "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

; problem 7

(define (stream-add-zero str)
  ; this was an awesome mindbender!
  (lambda () 
    (cons (cons 0 (car (str))) (stream-add-zero (cdr (str))))))

; problem 8

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) 
                (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) 
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

; problem 9

(define (vector-assoc v vec)
  (letrec ([search (lambda (x)
                     ; a helper func to search the vector starting at position x
                     (if (>= x (vector-length vec))
                         #f
                         (let ([y (vector-ref vec x)])
                           (if (and (pair? y) (equal? v (car y))) 
                               y
                               (search (+ x 1))))))])
    (search 0)))

; problem 10
; uses solution for problem 9

(define (cached-assoc xs n) 
  (letrec ([cache (make-vector n #f)] 
           [i 0] ; next cache slot to fill
           [f (lambda (v) 
                (let ([cachehit (vector-assoc v cache)])
                  (if cachehit
                      cachehit
                      (let ([listhit (assoc v xs)]) 
                        ; vector cache not hit, try assoc
                        (if listhit
                            (begin 
                              (vector-set! cache i listhit)
                              (set! i (remainder (+ i 1) n)) ; round-robin of length n
                              listhit)
                            #f)))))])
    f))

; problem 11

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([v1 e1]
              [f (lambda ()
                   ; a recursive thunk
                   (let ([v2 e2])
                     (if (< v2 v1)
                         (f)
                         #t)))])
       (f))]))