; Exercise 1.6

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 4)

; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

; Fibonacci numbers
; Iterative process
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)

; Excercise 1.11
; Recursive Process
(define (f-rec n)
  (if (< n 3)
    n
    (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3(f-rec (- n 3))))
  )
)

; Iterative Process
(define (f n)
  (f-iter 2 1 0 n)
)
(define (f-iter a b c n )
  (if (= n 0)
    c
    (f-iter (+ a (* 2 b) (* 3 c)) a b (- n 1))
  )
)

(f 3)
(f-rec 3)
