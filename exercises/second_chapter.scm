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

; Exercise 2.22
; This square-list implementation doesn't work properly because
; is appending with cons a list on the first element and then the
; square result on the second element.
; This logic generates a nested list as the answer.
; ie: (((() . 1) . 4) . 9)
(define (square-list items)
  (define (square x) (* x x))
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items null))

; Exercise 2.23
(define (for-each proc list-arg)
  (cond ((null? list-arg) true)
        (else
          (proc (car list-arg))
          (for-each proc (cdr list-arg)))))

;Exercise 2.27
(define (deep-reverse list1)
  (cond ((null? list1) null)
        ((pair? (car list1))
                (append (deep-reverse (cdr list1))
                        (list (deep-reverse (car list1)))))
        (else
          (append (deep-reverse (cdr list1)) (list (car list1))))))

;Exercise 2.28
(define (fringe tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else
          (append (fringe (car tree))
                  (fringe (cdr tree))))))

;Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (total-weight mobile)
  (+ (total-branch-weight (left-branch mobile))
     (total-branch-weight (right-branch mobile))))

; Test
(define child-branch (make-branch 0 4))
(define mobile (make-mobile child-branch child-branch))
(total-weight mobile)


(define (branch-torque branch)
  (* (branch-length branch)
     (total-branch-weight branch)))

(define (balanced-mobile? mobile)
  (and (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile)))
       (balanced-branch? (left-branch mobile))
       (balanced-branch? (right-branch mobile))))

(define (balanced-branch? branch)
  (if (pair? (branch-structure branch))
      (balanced-mobile? (branch-structure branch))
      true))

;Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) null)
        ((pair? tree)
         (cons (square-tree (car tree)) (square-tree (cdr tree))))
        ((not (pair? tree))
         (* tree tree))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (* sub-tree sub-tree)))
       tree))

;Exercise 2.31
(define (tree-map fn tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map fn sub-tree)
           (fn sub-tree)))
       tree))
(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

;Exercise 2.32
;We have to prepend the first element from the set into
;the rest of the subsets.
;Example: (subsets (list 1 2))
;First pass:
;rest = '(() (2))
;(append '(() (2)) ((1) (1 2))
(define (subsets s)
  (if (null? s)
    (list null)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x)
                          (append (list (car s)) x))
                        rest)))))

;Sequence Operations
(define (filter-c predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter-c predicate (cdr sequence))))
        (else (filter-c predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    null
    (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;Exercise 2.33
(define (map-c p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (append-c seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-c sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coefficient-sequence))

;Exercise 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

;Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    null
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

;Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x)
         (dot-product x v))
       m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           (matrix-*-vector cols m-row))
         m)))

;Exercise 2.38
(define (fold-left-c op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(define (fold-right-c op initial sequence)
  (accumulate op initial sequence))

(fold-right-c / 1 (list 1 2 3))
(fold-left-c / 1 (list 1 2 3))
(fold-right-c list null (list 1 2 3))
(fold-left-c list null (list 1 2 3))
; Any operation that has a commutative property should produce
; the same result for fold-left and fold-right
; Example:
(fold-right-c + 0 (list 1 2 3))
(fold-left-c + 0 (list 1 2 3))

; Exercise 2.39
(define (reverse-with-fold-right sequence)
  (fold-right-c (lambda (x y) (append y (list x))) null sequence))

(define (reverse-with-fold-left sequence)
  (fold-left-c (lambda (x y) (append (list y) x)) null sequence))

; Pre work for exercise 2.40
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

; prime? is defined in the exercise 1.21
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair)
                                 (cadr pair))))
; Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter-c prime-sum? (unique-pairs n))))
