;;; exercise 2.1
(define (make_rat p q) (cond ((and (<= 0 p) (<= 0 q)) (cons p q))
    ((and (>= 0 p) (>= 0 q)) (cons (* -1 p) (* -1 q)))
    ((and (>= 0 p) (<= 0 q)) (cons p q))
    ((and (<= 0 p) (>= 0 q)) (cons (* -1 p) (* -1 q)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add_rat x y) (make_rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))

(define (sub_rat x y) (make_rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))

(define (mul_rat x y) (make_rat (* (numer x) (numer y)) (* (denom x) (denom y))))

(define (div_rat x y) (make_rat (* (numer x) (denom y)) (* (denom x) (numer y))))

(define (equal_rat? x y) (= (* (numer x) (denom y)) (* (numer y) (denom x))))

(define (print_rat x) (newline) (display (numer x)) (display "/") (display (denom x)))


;;exercise 2.2

(define (average a b) (/ (+ a b) 2))

(define (make_point x y) (cons x y))

(define (x_point p) (car p))

(define (y_point p) (cdr p))

(define (make_segment p1 p2) (cons p1 p2))

(define (start_segment l) (car l))

(define (end_segment l) (cdr l))

(define (mid_point l) (make_point (average (x_point (start_segment l)) (x_point (end_segment l)))
    (average (y_point (start_segment l)) (y_point (end_segment l)))))


;;exercise 2.3
(define (make_rectangle p1 p2) (cons p1 p2))

(define (get_topleft r) (car r))

(define (get_bottomright r) (cdr r))

(define (get_width r) (abs (- (x_point (get_topleft r)) (x_point (get_bottomright r)))))

(define (get_height r) (abs (- (y_point (get_topleft r)) (y_point (get_bottomright r)))))

(define (rectangle_perimeter r) (* 2 (+ (get_width r) (get_height r))))

(define (rectangle_area r) (* (get_width r) (get_height r)))

(rectangle_area (make_rectangle (make_point 2 2) (make_point -1 -1)))

(rectangle_perimeter (make_rectangle (make_point 2 2) (make_point -1 0)))

;; other representation could be that we construct rectangle using width and height
;; In that case only get_width and get_height changes for procedures present at the higher barrier of abstraction

;; there is another way of representing the rectangle using "diagonal segment", in this case also, make_rectangle, get_width, get_height changes


;;exercise 2.4

(define (cons x y) (lambda (m) (m x y)))

(define (car z) (z (lambda (p q) p)))

(define (cdr z) (z (lambda (p q) q)))

;;exercise 2.5

;exercise 1.16

(define (even? n) (= (remainder n 2) 0))

(define (expt b x y n) (cond ((= n 0) y) ((even? n) (expt b (* x x) y (/ n 2))) (else (expt b x (* x y) (- n 1))))))

(define (exp b n) (expt b b 1 n)) 

(define (cons x y) (* (exp 2 x) (exp 3 y)))

(define (car z) (define (iter n result) (if (> (remainder result 2) 0) n (iter (+ n 1) (/ result 2))))
    (iter 0 z))

(define (cdr z) (define (iter n result) (if (> (remainder result 3) 0) n (iter (+ n 1) (/ result 3))))
    (iter 0 z))

;; extended exercises
;;exercise 2.7

(define (make_interval a b) (cons a b))

(define (lower_bound z) (car z))

(define (upper_bound z) (cdr z))

;;; exercise 2.8

(define (sub_interval x y) (make_interval (min (- (lower_bound x) (lower_bound y)) (- (upper_bound x) (upper_bound y)))
    (max (- (lower_bound x) (lower_bound y)) (- (upper_bound x) (upper_bound y)))))

;;;exercise 2.10

(define (mul_interval x y)
  (let ((p1 (* (lower_bound x) (lower_bound y)))
        (p2 (* (lower_bound x) (upper_bound y)))
        (p3 (* (upper_bound x) (lower_bound y)))
        (p4 (* (upper_bound x) (upper_bound y))))
    (make_interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div_interval x y)
  (if (or (= 0 (upper_bound y)) (= 0 (lower_bound y))) (display "denominator 0!")   
  (mul_interval x 
                (make_interval (/ 1.0 (upper_bound y))
                               (/ 1.0 (lower_bound y))))))

(div_interval (make_interval 1 2) (make_interval 0 4))

;; exercise 2.11

(define (mul_interval x y)
    (let ((x1 (lower_bound x))
        (y1 (upper_bound x))
        (x2 (lower_bound y))
        (y2 (upper_bound y)))
        (cond ((or (and (< 0 x1) (> 0 y1) (< 0 x2) (> 0 y2))
              (and (> 0 x1) (< 0 y1) (> 0 x2) (< 0 y2))
              (and (> 0 x1) (< 0 y1) (< 0 x2) (> 0 y2))
              (and (< 0 x1) (> 0 y1) (> 0 x2) (< 0 y2)))
              (make_interval (min (* (max x1 y1) (min x2 y2)) (* (min x1 y1) (max x2 y2))) (max (* (max x1 y1) (max x2 y2)) (* (min x1 y1) (min x2 y2)))))
              ((or (and (< 0 x1) (< 0 y1) (< 0 x2) (> 0 y2))
              (and (< 0 x1) (< 0 y1) (> 0 x2) (< 0 y2)))
              (make_interval (* (max x1 y1) (min x2 y2)) (* (max x1 y1) (max x2 y2))))
              ((or (and (> 0 x1) (< 0 y1) (< 0 x2) (< 0 y2))
              (and (< 0 x1) (> 0 y1) (< 0 x2) (< 0 y2)))
              (make_interval (* (min x1 y1) (max x2 y2)) (* (max x1 y1) (max x2 y2))))
              ((and (> 0 x1) (> 0 y1) (< 0 x2) (< 0 y2))
              (make_interval (* (min x1 y1) (max x2 y2)) (* (max x1 y1) (min x2 y2))))
              ((and (< 0 x1) (< 0 y1) (> 0 x2) (> 0 y2))
              (make_interval (* (max x1 y1) (min x2 y2)) (* (min x1 y1) (max x2 y2))))
              ((or (and (> 0 x1) (> 0 y1) (> 0 x2) (< 0 y2))
              (and (> 0 x1) (> 0 y1) (< 0 x2) (> 0 y2)))
              (make_interval (* (min x1 y1) (max x2 y2)) (* (min x1 y1) (min x2 y2))))
              ((or (and (< 0 x1) (> 0 y1) (> 0 x2) (> 0 y2))
              (and (> 0 x1) (< 0 y1) (> 0 x2) (> 0 y2)))
              (make_interval (* (max x1 y1) (min x2 y2)) (* (min x1 y1) (min x2 y2))))
              ((and (< 0 x1) (< 0 y1) (< 0 x2) (< 0 y2))
              (make_interval (* (min x1 y1) (min x2 y2)) (* (max x1 y1) (max x2 y2))))
              ((and (> 0 x1) (> 0 y1) (> 0 x2) (> 0 y2))
              (make_interval (* (max x1 y1) (max x2 y2)) (* (min x1 y1) (min x2 y2)))))))

;;; exercise 2.12

(define (make_interval_center_percentage c p) (make_interval (- c (* c p)) (+ c (* c p))))

(define (percentage z) (/ (- (upper_bound z) (lower_bound z)) (* (center z) 2)))

(define (center z) (/ (+ (lower_bound z) (upper_bound z)) 2.0)) 


(center (make_interval_center_percentage 3.5 .015))


;;; exercise 2.14

(define (add_interval x y)
  (make_interval (+ (lower_bound x) (lower_bound y))
                 (+ (upper_bound x) (upper_bound y))))

(define (par1 r1 r2) (div_interval (mul_interval r1 r2) (add_interval r1 r2)))


(define (par2 r1 r2)zxv
  (let ((one (make_interval 1 1))) 
    (div_interval one
                  (add_interval (div_interval one r1)
                                (div_interval one r2)))))

(define i1 (make_interval_center_percentage 3.5 .000005))

(define i2 (make_interval_center_percentage 3.5 .00000005))

(par1 i1 i2)
(par2 i1 i2)

;;;exercise 2.17
(define squares (list 1 4 9 16 25))

(define odds (list 1 3 5 7))

(define (append l1 l2) (if (null? l1) l2 (cons (car l1) (append (cdr l1) l2))))

(append odds squares)

(append squares odds)

(define (last_pair l) (if (or (null? l) (null? (cdr l))) l (last_pair (cdr l))))

;;;exercise 2.18

(define (reverse l) (define (reverse_iter lx a) (if (null? lx) a (reverse_iter (cdr lx) (cons (car lx) a)))) (reverse_iter l (list )))

;;;exercise 2.19
;;coin change

(define (count-change amount)
  (cc amount coin_list))
(define (cc amount coin_list)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coin_list)) 0)
        (else (+ (cc amount
                     (cdr coin_list))
                 (cc (- amount
                        (car coin_list))
                     coin_list)))))


(define us_list (list 1 10 5 25 50))

;; exercise 2.20

(define (same_parity . l) 
    (define (check x) (= (remainder (car l) 2) (remainder x 2)))
    (define (same_parity_recur lx)
        (cond 
            ((null? lx) (list )) 
            ((check (car lx)) (cons (car lx) (same_parity_recur (cdr lx))))
            (else (same_parity_recur (cdr lx))))) (same_parity_recur l))

(define (scale_list l f) (if (null? l) (list ) (cons (* (car l) f) (scale_list (cdr l) f))))

(define (map proc l) (if (null? l) (list ) (cons (proc (car l)) (map proc (cdr l)))))

;;exercise 2.21

(define (square-list items)
  (if (null? items)
      (list )
      (cons (square (car items)) (square-list (cdr items)))))


(define (square-list items)
  (map square items))

;;exercise 2.23

(define (for-each f l) (if (not (null? l)) (f (car l))) (if (null? l) #t (for-each f (cdr l))))


;;exercise 2.25

(1 3 (5 7) 9)

(car (cdr (car (cdr (cdr test)))))

((7))

(car (car test))

(1 (2 (3 (4 (5 (6 7))))))

(define test (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr test))))))))))))

;;;exercise 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

;; exercise 2.27

(define (deep_reverse l) 
    (define (reverse_iter lx a)
        (cond ((null? lx) a)
              ((not (pair? lx)) lx)
              (else (reverse_iter (cdr lx) (cons (reverse_iter (car lx) (list )) a)))))
    (reverse_iter l (list )))


(deep_reverse (list (list 1 2) (list 3 4)))

;Value 16: ((4 3) (2 1))

(deep_reverse (1 (2 (3 (4 (5 (6 7)))))))

;;exercise 2.28

(define (append l1 l2) (if (null? l1) l2 (cons (car l1) (append (cdr l1) l2))))

(define (fringe l) 
        (cond ((null? l) (list ))
              ((not (pair? l)) (list l))
              (else (append (fringe (car l)) (fringe (cdr l))))))

;;exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (get_left a) (car a))

(define (get_right a) (car (cdr a)))

(define (left-branch mobile) (get_left mobile))

(define (right-branch mobile) (get_right mobile))

(define (branch-length branch) (get_left branch))

(define (branch-structure branch) (get_right branch))

(define (total_weight mobile)
        (cond ((null? mobile) 0)
              ((not (pair? mobile)) mobile)
              (else (+ (total_weight (branch-structure (left-branch mobile))) (total_weight (branch-structure (right-branch mobile)))))))

(define l1 (make-branch 1 1))

(define l2 (make-branch 1 1))

(define l3 (make-branch 1 1))

(define l4 (make-branch 1 1))

(define m1 (make-mobile l1 l2))

(define m2 (make-mobile l3 l4))

(define l5 (make-branch 1 m1))

(define l6 (make-branch 1 m2))

(define root (make-mobile l5 l6))

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (balanced_mobile? mobile)
    (cond ((null? mobile) #t)
          ((not (pair? mobile)) #t)
          (else (and (= (torque (left-branch mobile)) (torque (right-branch mobile))) (balanced_mobile? (branch-structure (left-branch mobile))) (balanced_mobile? (branch-structure (right-branch mobile)))))))

(define (torque branch) (if (pair? (branch-structure branch)) 
    (* (branch-length branch) (total_weight (branch-structure branch)))
    (* (branch-length branch) (branch-structure branch))))

(define (scale_tree tree factor) 
    (cond ((null? tree) (list ))
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale_tree (car tree) factor) (scale_tree (cdr tree) factor)))))

(scale_tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(define (scale_tree tree factor) 
    (map (lambda (subtree) 
            (cond ((null? subtree) subtree) 
                ((pair? subtree) (scale_tree subtree factor))
                (else (* subtree factor)))) tree))


;;;exercise 2.30

(define (square_tree tree) 
    (map (lambda (subtree) 
            (cond ((null? subtree) subtree) 
                ((pair? subtree) (square_tree subtree))
                (else (square subtree)))) tree))

(define (square_tree tree) 
    (cond ((null? tree) tree)
        ((pair? tree) (cons (square_tree (car tree)) (square_tree (cdr tree))))
        (else (square tree))))

(square_tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;Value 17: (1 (4 (9 16) 25) (36 49))

;;exercise 2.31

(define (tree_map tree f)
    (cond ((null? tree) tree)
        ((pair? tree) (cons (tree_map (car tree) f) (tree_map (cdr tree) f)))
        (else (f tree))))

(define (square_tree tree) (tree_map tree square))


;;exercise 2.32

(define (subsets s)
    (if (null? s)
        (list (list ))
        (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (l) (append (list (car s)) l)) rest)))))


(define (filter predicate sequence)
    (cond ((null? sequence) (list ))
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))  

(define (accumulate op initial sequence)
    (cond ((null? sequence) initial)
        (else (op (car sequence) (accumulate op initial (cdr sequence))))))

(define (enumerate-interval low high)
    (cond ((> low high) (list ))
        (else (cons low (enumerate-interval (+ low 1) high)))))

(define (enumerate-leaves root)
    (cond ((null? root) (list ))
        ((pair? root) (append (enumerate-leaves (car root)) (enumerate-leaves (cdr root))))
        (else (list root))))

(define (sum-odd-square-leaves root)
    (accumulate + 0 (map square (filter odd? (enumerate-leaves root)))))

(define (even-fib n)
    (accumulate cons (list ) (filter even? (map fibonacci (enumerate-interval 0 n)))))

;; exercise 2.33

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) (list ) sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))


(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;;exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;exercise 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-leaves t))))

;;exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list )
      (cons (accumulate op init (map (lambda (l) (car l)) seqs))
            (accumulate-n op init (map (lambda (l) (cdr l)) seqs)))))

;;exercise 2.37
(define (matrix-*-vector m v)
  (map (lambda (l) (accumulate + 0 (accumulate-n * 1 (list l v)))) m))

(define (transpose mat)
  (accumulate-n cons (list ) mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

;;exercise 2.38


(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest) result
            (iter (op result (car rest)) (cdr rest))))
    (iter initial sequence))

(define (fold-right op initial sequence)
    (accumulate op initial sequence))


;;exercise 2.39

(define (reverse sequence)
  (fold-right (lambda (x y)  (append y (list x))) (list ) sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) (list ) sequence))


;;
(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (flatmap proc l)
    (accumulate append () (map proc l)))

(define (prime-sum-pairs n)
    (map make-pair-sum 
        (filter prime-sum? 
            (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))))

;; permutations

(define (remove x s)
    (filter (lambda (y) (not (= x y))) s))

(define (permutations s)
    (if (null? s) (list (list ))
        (flatmap (lambda (x) (map (lambda (l) (append (list x) l)) (permutations (remove x s)))) s)))


;; exercise 2.40

(define (unique-pairs n)
    (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
    (map make-pair-sum 
        (filter prime-sum? 
            (unique-pairs n))))


;; exercise 2.41

(define (unique-triples n)
    (flatmap (lambda (pair) (map (lambda (x) (append pair (list x))) (enumerate-interval 1 (- (cadr pair) 1))))
        (unique-pairs n)))

(define (ordered-triples n s)
    (filter (lambda (l) (= (accumulate + 0 l) s)) (unique-triples n)))







;;exercise 2.53
(define (memq item x)
    (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;;exercise 2.54

(define (equal? x y)
    (cond ((and (null? x) (null? y)) #t)
        ((and (not (pair? x)) (not (pair? y))) (eq? x y))
        ((and (pair? (car x)) (pair? (car y))) (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
        ((and (not (pair? (car x))) (not (pair? (car y)))) (and (eq? (car x) (car y)) (equal? (cdr x) (cdr y))))
        (else #f)))


;;Symbolic diffrentiation

(define (variable? e)
   (symbol? e))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
    (list '+ a1 a2))

(define (make-product a1 a2)
    (list '* a1 a2))

(define (sum? e)
    (and (pair? e) (eq? (car e) '+)))

(define (addend s)
    (cadr s))

(define (augend s)
    (caddr s))

(define (product? e)
    (and (pair? e) (eq? (car e) '*)))

(define (multiplier p)
    (cadr p))

(define (multiplicand p)
    (caddr p))


(define (deriv exp var)
    (cond ((number? exp) 0)
        ((variable? exp)
            (if (same-variable? exp var) 1 0))
        ((sum? exp) 
            (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
        ((product? exp) 
            (make-sum 
                (make-product (multiplier exp) (deriv (multiplicand exp) var)) 
                (make-product (multiplicand exp) (deriv (multiplier exp) var))))))

;;exercise 2.56

(define (=number? e n)
    (and (number? e) (= e n)))


(define (make-exponent u n)
    (cond ((=number? n 0) 1)
        ((=number? u 0) 0)
    (else (list '** u n))))

(define (exponentiation? e)
    (and (pair? e) (eq? (car e) '**)))

(define (base e)
    (caddr e))

(define (exponent e)
    (cadr e))

(define (deriv exp var)
    (cond ((number? exp) 0)
        ((variable? exp)
            (if (same-variable? exp var) 1 0))
        ((sum? exp) 
            (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
        ((product? exp) 
            (make-sum 
                (make-product (multiplier exp) (deriv (multiplicand exp) var)) 
                (make-product (multiplicand exp) (deriv (multiplier exp) var))))
        ((exponentiation? exp) 
            (make-product (make-product (base exp) (make-exponent (exponent exp) (- (base exp) 1))) (deriv (exponent exp) var)))))

(define (augend e)
    (cond ((null? (cdddr e)) (caddr e))
        (else (append '(+) (cddr e)))))

(define (multiplicand e)
    (cond ((null? (cdddr e)) (caddr e))
        (else (append '(*) (cddr e)))))

;;sets as unordered lists

(define (element-of-set? x set)
    (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (cond ((element-of-set? x set) set)
        (else (cons x set))))

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) (list ))
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;exercise 2.59

(define (union-set set1 set2)
    (cond ((or (null? set1) (null? set2)) set2)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))


;;sets as ordered list

(define (element-of-set? x set)
    (cond ((null? set) #f)
        ((= (car set) x) #t)
        ((< (car set) x) (element-of-set? x (cdr set)))
        (else #f)))


(element-of-set? 0 (list 1 2 3 4 5 6 7))

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) (list ))
        ((= (car set1) (car set2)) (cons (car set1) (intersection-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (intersection-set (cdr set1) set2))
        (else (intersection-set set1 (cdr set2)))))

;;exercise 2.61
(define (adjoin-set x set)
    (cond ((null? set) (cons x (list )))
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))
        ((< x (car set)) (cons x set))
        (else set)))

;;exercise 2.62

(define (union-set set1 set2)
    (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
        ((> (car set1) (car set2)) (cons (car set2) (union-set set1 (cdr set2))))
        ((= (car set1) (car set2)) (union-set set1 (cdr set2)))))























































































































































































































































































































