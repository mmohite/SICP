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

;; there is another way of representing the rectangle using "diagoanl segment", in this case also, make_rectangle, get_width, get_height changes



































