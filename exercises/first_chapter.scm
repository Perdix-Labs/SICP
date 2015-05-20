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

; Exercise 1.11
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

; Exercise 1.12
; Assumes r and c are within bounds
; of a pascal's triangle.
; r = row
; c = column
(define (pascal r c)
  (if (or (= r 1) (= c 1) (= r c))
    1
    (+ (pascal (- r 1) (- c 1))
       (pascal (- r 1) c))
  )
)
(pascal 5 2)

; Exercise 1.16
(define (fast-exp b n)
  (iter b n 1)
)

(define (iter b n a)
  (cond ((= n 0) a)
        ((even? n) (iter (square b) (/ n 2) a))
        (else (iter b (- n 1) (* a b)))
  )
)

(define (square x) (* x x))

(define (even? n)
  (= (remainder n 2) 0)
)

(fast-exp 3 3)
(fast-exp 3 4)

; Exercise 1.17
; Note: solved the 1.17 as is was for 1.18 solution.
(define (fast-mult a b)
  (mult-iter a b 0)
)

(define (mult-iter a b product)
  (cond ((= b 0) product)
        ((even? b) (mult-iter (double a) (halve b) product))
        (else (mult-iter a (- b 1) (+ product a)))
  )
)

; Assumes previously defined by language
(define (halve x) (/ x 2))
(define (double x) (+ x x))

(fast-mult 10 2)
(fast-mult 5 25)

; Exercise 1.19
; Here's how I solved the problem http://imgur.com/a/VYHZ3
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (compute-p-prime p q)
                   (compute-q-prime p q)
                   (/ count 2)))
         (else (fib-iter (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1))))
)

(define (compute-p-prime p q)
  (+ (* q q) (* p p)))

(define (compute-q-prime p q)
  (+ (* 2 p q) (* q q)))

(fib 5)
(fib 8)
(fib 20)
(fib 300)
(fib 5000)
(fib 10000)
(fib 100000)
(fib 1000000)
(fib 10000000) ; Got a bit excited
