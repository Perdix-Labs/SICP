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