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

(define (par1 r1 r2) (div_interval (mul_interval r1 r2) (add_interval r1 r2)))

(define (par2 r1 r2)












































































