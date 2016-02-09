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

; procedure to generate permutations of a given set.
(define (permutations s)
  (if (null? s)
    (list null)
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove-c x s))))
             s)))
(define (remove-c item sequence)
  (filter-c (lambda (x) (not (= x item)))
            sequence))

; Exercise 2.41
; Generates ordered triplets from 1 to n.
(define (ordered-triplets n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k)
                           (list i j k))
                         (enumerate-interval 1 (- j 1))))
                    (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(define (make-triplet-sum triplet)
  (append triplet (list (accumulate + 0 triplet))))

; Definition using unique-triplets
(define (ordered-triplet-sum n s)
  (define (triplet-sum? triplet)
    (= s (accumulate + 0 triplet)))
  (map make-triplet-sum (filter-c triplet-sum? (ordered-triplets n))))

; Exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter-c (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board null)

(define (make-position row col)
  (cons row col))

(define (position-row position)
  (car position))

(define (position-col position)
  (cdr position))

(define (adjoin-position row col positions)
  (append positions (list (make-position row col))))

(define (safe? col positions)
  (let ((kth-queen (list-ref positions (- col 1)))
        (other-queens (filter-c (lambda (q)
                                  (not (= col (position-col q))))
                                positions)))
  (define (attacks? q1 q2)
    (or (= (position-row q1) (position-row q2))
        (= (abs (- (position-row q1) (position-row q2)))
           (abs (- (position-col q1) (position-col q2))))))

  (define (iter q board)
    (or (null? board)
        (and (not (attacks? q (car board)))
             (iter q (cdr board)))))
  (iter kth-queen other-queens)))

; Exercise 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1)
           (xcor-vect vect2))
        (+ (ycor-vect vect1)
           (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
  (make-vect (- (xcor-vect vect1)
           (xcor-vect vect2))
        (- (ycor-vect vect1)
           (ycor-vect vect2))))

(define (scale-vect vect scalar)
  (make-vect (* scalar (xcor-vect vect))
             (* scalar (ycor-vect vect))))

; Exercise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
 (cdr (cdr frame)))

; Exercise 2.48
(define (make-segment sx sy ex ey)
  (list (make-vect sx sy)
        (make-vect ex ey)))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (car (cdr segment)))

; Exercise 2.49
(use sicp)
(include "painters.scm")

(write-painter-to-svg (segments->painter outline) "2.49-outline.svg")
(write-painter-to-svg (segments->painter x) "2.49-x.svg")
(write-painter-to-svg (segments->painter diamond) "2.49-diamond.svg")
(write-painter-to-svg (segments->painter wave) "2.49-wave.svg")

; Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0) ; origin
                     (make-vect 0.0 0.0) ; edge1
                     (make-vect 1.0 1.0))) ; edge2

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

(write-painter-to-svg (flip-horiz (segments->painter wave)) "2.50-wave-fip-horiz.svg")
(write-painter-to-svg (rotate90 (segments->painter wave)) "2.50-wave-rotate90.svg")
(write-painter-to-svg (rotate180 (segments->painter wave)) "2.50-wave-rotate180.svg")
(write-painter-to-svg (rotate270 (segments->painter wave)) "2.50-wave-rotate270.svg")

; Exercise 2.51
(define (below-direct painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point))
          (paint-top
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(write-painter-to-svg (below-direct (segments->painter diamond)
                             (segments->painter x))
                      "2.51-x-diamond-below-direct.svg")

(define (below-indirect painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(write-painter-to-svg (below-indirect (segments->painter diamond)
                             (segments->painter x))
                      "2.51-x-diamond-below-indirect.svg")

; Section 2.3 Symbolic Data

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; Exercise 2.54
(define (equal? a b)
  (cond ((and (symbol? a) (symbol? b))
         (eq? a b))
        ((and (number? a) (number? b))
         (= a b))
        ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ((and (null? a) (null? b))
         #t)))

; 2.3.2 Example: Symbolic Differentiation
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)  ; Exercise 2.56
         (make-product
           (make-product (exponent exp)
                         (make-exponentiation (base exp)
                                              (- (exponent exp) 1)))
           (deriv (base exp) var)))
        (else
          (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0))
         0)
        ((=number? m1 1)
         m2)
        ((=number? m2 1)
         m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else
          (list '* m1 m2))))

(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) ; Moddified to add exercise 2.57
  (if (null? (cdddr s))
    (caddr s)
    (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) ; Moddified to add exercise 2.57
  (if (null? (cdddr p))
    (caddr p)
    (cons '* (cddr p))))

; Exercise 2.56
(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0)
         1)
        ((=number? exponent 1)
         base)
        ((and (number? base) (number? exponent))
         (exp base exponent))
        (else
          (list '** base exponent))))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

; 2.3.3 Example: Representing Sets
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else
          (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else
          (intersection-set (cdr set1) set2))))

; Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
          (cons (car set1)
                (union-set (cdr set1) set2)))))

; Exercise 2.60
(define (element-of-set-with-dups? x set)
  (element-of-set? x set)) ; Same as exercise 2.59

(define (adjoin-set-with-dups x set)
  (cons x set))

(define (union-set-with-dups set1 set2)
  (append set1 set2))

(define (intersection-set-with-dups set1 set2)
  (intersection-set set1 set2)) ; Same as exercise 2.59

; element-of-set and intersection-set remain the same complexity O(n)
; adjoin-set and union-set improve from O(n) to be O(1) since they
; no longer need to check for duplicates before adding members or
; appending sets. Thus this implementation would be preferable when
; we don't mind too much about memory when repeating elements and
; we have a lots of union and adjoin operations.

; Sets as Ordered Lists.
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else
          (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1))
          (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((> x1 x2)
             (intersection-set set1 (cdr set2)))))))

; Exercise 2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set))
         (cons x set))
        (else
          (cons (car set) (adjoin-set x (cdr set))))))

; Exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set (cdr set1) set2)))
                  ((> x1 x2)
                   (cons x2 (union-set set1 (cdr set2)))))))))
