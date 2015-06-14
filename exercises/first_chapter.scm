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

; Exercise 1.21
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; Exercise 1.22
(define (time-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))
    (report-prime "no prime")))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
; runtime is not defined by default on racket
(define (runtime) (current-milliseconds))

(define (search-for-primes startingFrom)
  ; get the 3 smallest primes greater than startingFrom
  (n-smallest-primes (+ startingFrom 1) 3))
(define (n-smallest-primes a count)
  (cond ((= count 0) 0)
        ((even? a)
         (n-smallest-primes (+ 1 a) count))
        ((prime? a)
         (time-prime-test a)
         (n-smallest-primes (+ 2 a) (- count 1)))
        (else (n-smallest-primes (+ 2 a) count))
  )
)

(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)
(search-for-primes 1000000)
; try bigger data, difference of time is not appreciated
(search-for-primes 1000000000)
(search-for-primes 10000000000)
(search-for-primes 100000000000)

; Exercise 1.29
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b)))
)

; assumes n is even number
(define (simpson f a b n)
  (define (h) (/ (- b a) n))
  (define (y k) (f (+ a (* k (h)))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))
    )
  )
  (define (next k) (+ k 1))
  (* (sum term 0 next n)
     (/ (h) 3))
)

(define (cube x) (* x x x))

(simpson cube 0 1 1000)

; Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))
    )
  )
  (iter a 0)
)
; Testing
(define (identity x) x)
(define (inc x) (+ x 1))
; result of sum from 1 to 1000 must be 500500
(sum identity 1 inc 1000)

; Exercise 1.31
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))
    )
  )
  (iter a 1)
)

(product-iter identity 1 inc 10)

(define (factorial x)
  (product-iter identity 1 inc x)
)

(factorial 4)

(define (approximate-pi n)
  (define (term a)
    (if (even? a)
      (/ (+ a 2) (+ a 1))
      (/ (+ a 1) (+ a 2))
    )
  )
  (define (next a) (+ a 1))
  (* (product-iter term 1 next n)
     4.0 ; convert the int division to float
  )
)

(approximate-pi 10)
(approximate-pi 1000)

(define (product-rec term a next b)
  (if (> a b)
    1
    (* (term a)
       (product-rec term (next a) next b))
  )
)

(product-rec identity 1 inc 10)

; Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))
  )
)

(accumulate * 1 identity 1 inc 10)
(accumulate + 0 identity 1 inc 10)

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))
    )
  )
  (iter a null-value)
)

(accumulate-iter * 1 identity 1 inc 10)
(accumulate-iter + 0 identity 1 inc 10)

; Exercise 1.33
(define (filtered-accumulate filterBy combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (if (filterBy a)
        (iter (next a) (combiner result (term a)))
        (iter (next a) result)
      )
    )
  )
  (iter a null-value)
)

(filtered-accumulate prime? + 0 square 1 inc 5)

(define (relative-primes? x n)
  (= (gcd x n) 1)
)

(define (product-of-relative-primes n)
  (define (filter-relative-primes x)
    (relative-primes? x n))
  (filtered-accumulate filter-relative-primes * 1 identity 1 inc n)
)

(product-of-relative-primes 5)
(product-of-relative-primes 10)
(product-of-relative-primes 100)

; Exercise 1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display "guess: ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess)
)

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(golden-ratio)

; Exercise 1.36
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
; With average damping
(define (average x y) (/ (+ x y) 2))
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)

; Exercise 1.37
(define (cont-frac n d k)
  (define (aux i)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (aux (+ i 1))) )
    )
  )
  (aux 1)
)

(define (cont-frac-iter n d k)
  ; works backwards from k to 0
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))
    )
  )
  (iter k 0)
)

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                11)

; Exercise 1.38
(define (euler-expansion n)
  (+ 2
     (cont-frac-iter (lambda (i) 1.0)
                     (lambda (i)
                       (if (= (remainder i 3) 2)
                         (/ (+ i 1) 1.5)
                         1))
                     n)
  )
)
(euler-expansion 100)

; Exercise 1.39
(define (tan-cf x k)
  (cont-frac (lambda(i)
               (if (= i 1)
                 x
                 (- (* x x))
               ))
             (lambda(i)
                 (- (* i 2) 1))
             k)
)
(tan-cf 100.0 500)

; Newtons Method
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx))
)
(define dx 0.00001)
(define (newtons-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x))))
)
(define (newtons-method g guess)
  (fixed-point (newtons-transform g) guess))

; Exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)
  )
)
(newtons-method (cubic 1 2 3) 1)

;Exercise 1.41
(define (double f)
  (lambda (x) (f (f x)))
)
((double inc) 12)
(((double (double double)) inc) 5) ; Result 21

;Exercise 1.42
(define (compose f g) (lambda(x) (f (g x))))
((compose square inc) 6) ; Result 49
