; Represent Rational Numbers
(define (make-rat-help n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

; Test it
(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat one-third)

(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

;Exercise 2.1
(define (make-rat n d)
  (if (or (and (negative? n) (negative? d))
          (negative? d))
    (make-rat-help (* -1 n) (* -1 d))
    (make-rat-help n d)))

;Exercise 2.2
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (define (avg x y)
    (/ (+ x y)
       2))
  (make-point
    (avg (x-point (start-segment segment)) (x-point (end-segment segment)))
    (avg (y-point (start-segment segment)) (y-point (end-segment segment)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display " , ")
  (display (y-point p))
  (display ")")
  (newline))

(define (test-midpoint-segment)
  (print-point (midpoint-segment
                 (make-segment (make-point 0 0) (make-point 10 0)))))

;Exercise 2.3
;Represent a Rectangle with a couple of pairs of segments,
;Segments are stored in clocwise order starting from the top.
(define (make-rectangle top-seg right-seg bot-seg left-seg)
  (cons (cons top-seg right-seg) (cons bot-seg left-seg)))

(define (first-half-rectangle rectangle)
  (car rectangle))

(define (last-half-rectangle rectangle)
  (cdr rectangle))

(define (top-rectangle rectangle)
  (car (first-half-rectangle rectangle)))

(define (right-rectangle rectangle)
  (cdr (first-half-rectangle rectangle)))

(define (bot-rectangle rectangle)
  (car (last-half-rectangle rectangle)))

(define (left-rectangle rectangle)
  (cdr (last-half-rectangle rectangle)))

(define (distance-segment segment)
  (sqrt (+
          (expt (- (x-point (end-segment segment)) (x-point (start-segment segment)))
                2)
          (expt (- (y-point (end-segment segment)) (y-point (start-segment segment)))
                2)
          )))

(define (perimeter-rectangle rectangle)
  (* (+ (distance-segment (top-rectangle rectangle))
        (distance-segment (right-rectangle rectangle)))
     2))

(define (area-rectangle rectangle)
  (* (distance-segment (top-rectangle rectangle))
     (distance-segment (right-rectangle rectangle))))

(define (test-perimeter-and-area)
  (define rect (make-rectangle
                 (make-segment (make-point 0 5) (make-point 10 5))
                 (make-segment (make-point 10 5) (make-point 10 0))
                 (make-segment (make-point 10 0) (make-point 0 0))
                 (make-segment (make-point 0 0) (make-point 0 5))))
  (display "Perimeter:")
  (display (perimeter-rectangle rect))
  (display "Area:")
  (display (area-rectangle rect))
  (newline))

;Exercise 2.4
;Procedural representations of cons, car and cdr.
(define (cons-p x y)
  (lambda (m) (m x y)))

(define (car-p z)
  (z (lambda (p q) p)))

(define (cdr-p z)
  (z (lambda (p q) q)))

;Exercise 2.5
;Represent pairs with numbers.
;Solution: Iteratively divide first by two for car.
;The remaining amount iteratively divide by 3.
(define (cons-num a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car-num c)
  (define (car-rec value result)
    (if (= (remainder value 2) 0)
      (car-rec (/ value 2) (+ result 1))
      result))
  (car-rec c 0))

(define (cdr-num c)
  (define (cdr-rec value result)
    (if (= (remainder value 3) 0)
      (cdr-rec (/ value 3) (+ result 1))
      result))
  (cdr-rec (/ c (expt 2 (car-num c))) 0))

;Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

; Just as add-1 wraps the result of applying f n times
; we wrap the result applying f m times to n f
(define (plus m n)
  (lambda (f) (lambda (x) ((m f) ((n f ) x)))))

; test helper method
(define (inc n)
  (+ n 1))

;Section 2.1.4 Extended Exercise: Interval Arithmetic.
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
; Exercise 2.7
(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

;Exercie 2.8
(define (sub-interval x y)
  (make-interval (abs (- (lower-bound x) (lower-bound y)))
                 (abs (- (upper-bound x) (lower-bound y)))))

;Exercise 2.9
(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x))
     2))

(define (width-sum a b)
  (+ (width-interval a) (width-interval b)))

(define (width-sub a b)
  (abs (- (width-interval a) (width-interval b))))

;Exercise 2.10
(define (div-interval-span-zero x y)
  (if (= 0 (width-interval y))
    (error "Width of denominator interval can't be zero.")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

;Exercise 2.17
(define (last-pair list1)
        (if (= 1 (length list1))
          (car list1)
          (last-pair (cdr list1))))

;Exercise 2.18
(define (reverse list1)
  (append (reverse (cdr list1)) (list (car list1))))

;Exercise 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount (except-first-denomination coin-values))
             (cc (- amount (first-denomination coin-values)) coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (= 0 (length coin-values)))

(define us-coins (list 50 25 10 5 1))
(cc 100 us-coins) ; answer is 292

; Exercise 2.20
(define (same-parity x . rest-args)
  (define (parity-fn x) (modulo x 2))
  (define parity-of-x (parity-fn x))
  (define (same-parity-helper . args)
    (cond ((= 0 (length args)) null)
          ((= parity-of-x (parity-fn (car args)))
            (cons (car args) (apply same-parity-helper (cdr args))))
          (else
            (apply same-parity-helper (cdr args)))))
  (apply same-parity-helper rest-args))

; Exercise 2.21
(define (square-list items)
  (if (null? items)
    null
    (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (suquare-list items)
  (map (lambda (x) (* x x)) items))
