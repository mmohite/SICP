;exercise 1.16

(define (expt b x y n) (cond ((= n 0) y) ((even? n) (expt b (* x x) y (/ n 2))) (else (expt b x (* x y) (- n 1))))))

(define (exp b n) (expt b b 1 n)) 
;;; Loop invariant between subsequent calls -> y * x^n' = b^n when n' = 0, y = b^n

(define (even? n) (= (remainder n 2) 0))

(exp 2 10)

(expt 2 8)

;;; exercise 1.17

(define (fast_multiply a b) (cond ((= b 0) 0) ((even? b) (double (fast_multiply a (halve b)))) (else (+ a (fast_multiply a (- b 1))))))

(define (double n) (* n 2))
(define (halve n) (/ n 2))


;;; exercise 1.18
;;; Loop invariant between subsequent calls -> y + x * b' = a * b, when b' = 0 y = a * b

(define (fast_multiply_iter a x y b) (cond ((= b 0) y) ((even? b) (fast_multiply_iter a (double x) y (halve b))) (else (fast_multiply_iter a x (+ x y) (- b 1)))))

(define (fast_multiply_iterative a b) (fast_multiply_iter a a 0 b))

(fast_multiply_iterative 5 7)

(fast_multiply_iterative 8 4)

(fast_multiply_iterative 4 9)



;;; exercise 1.19

(define (fibonacci n) (fib_fast_iter 1 0 0 1 n))

(define (fib_fast_iter a b p q n) 
    (cond ((= n 0) b) 
    ((even? n) (fib_fast_iter a b (+ (square p) (square q)) (+ (square q) (* 2 p q)) (/ n 2))) 
    (else (fib_fast_iter (+ (* (+ a b) q) (* b p)) (+ (* a q) (* b p)) p q (- n 1)))))



;;; exercise 1.22

(define (smallest_divisor n divisor) (cond ((< n (square divisor)) n) ((divisible? n divisor) divisor) (else (smallest_divisor n (+ divisor 1)))))

(define (divisible? n divisor) (= (remainder n divisor) 0))

(define (prime? n) (= (smallest_divisor n 2) n))

(define (timed_prime_test n) (newline) (display n) (start_prime_test n (real-time-clock)))

(define (start_prime_test n start_time) (if (prime? n) (report_time (- (real-time-clock) start_time))))

(define (report_time elapsed_time) (display " **** ") (display elapsed_time))


;;; exercise 1.23

(define (next divisor) (if (= divisor 2) 3 (+ divisor 2)))

(define (smallest_divisor n divisor) (cond ((< n (square divisor)) n) ((divisible? n divisor) divisor) (else (smallest_divisor n (next divisor)))))

;;; exercise 1.24

(define (expmod base exp m) (cond ((= exp 0) 1) ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m)) (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fast_prime n) (try_it (+ (random (- n 1)) 1) n))

(define (try_it a n) (= (expmod a n n) a))

;;; exercise 1.27

(define (test_carmichael_number n count) (cond ((= count n) #t) ((try_it count n) (test_carmichael_number n (+ count 1))) (else #f)))



;;; exercise 1.29

(define (sum term a next b) (if (> a b) 0 (+ (term a) (sum term (next a) next b))))
(define (integral f a b dx) (define (next_term x) (+ x dx)) (* (sum cube (+ a (/ dx 2)) next_term b) dx))


(define (sum_simpson term a next b next_multiplier multiplier total count n) 
(if (= count n) (+ total (term a))
(sum_simpson term (next a) next b next_multiplier  (+ (* multiplier (term a)) total) (+ count 1) n)))


(define (simpsons_rule f a b n) 
    (define h (/ (- b a) n))
    (define (next_multiplier x) (if (= x 4) 2 4))
    (define (next_term x) (+ x h)) 
    (/ (* (sum_simpson f a next_term b next_multiplier 1 0 0 n) h) 3.0))

;;; exercise 1.30 
(define (sum term a next b) (define (iter a result) (if (> a b) result (iter (next a) (+ result (term a)))))
    (iter a 0))


;;; exercise 1.31

(define (product_recur term a next b) (if (> a b) 1 (* (term a) (product_recur term (next a) next b))))
(define (factorial n) (define (next_term a) (+ a 1)) (define (term a) a) (product_recur term 1 next_term n))

(define (pi_calculation n) (define (next_term a) (+ a 2))
    (define (term a) (square (/ a (+ a 1))))
    (* (product_iter term 2.0 next_term n) 2 (+ n 1)))


(define (product_iter term a next b) (define (iter a result) (if (> a b) result (iter (next a) (* (term a) result)))) (iter a 1))


;;; exercise 1.32

(define (accumulate combiner null_value term a next b) (if (a > b) null_value (combiner (term a) (accumulate combiner null_value term a next b))))

(define (accumulate combiner null_value term a next b)
    (define (iter a result) 
        (if (> a b) 
            result 
        (iter (next a) (combiner (term a) result))))
    (iter a null_value))

(define (product term a next b) (define (combiner a b) (* a b)) (accumulate combiner 1.0 term a next b))

(define (sum term a next b) (define (combiner a b) (+ a b)) (accumulate combiner 0 term a next b))



;;; exercise 1.33

(define (filtered_accumulate pred? combiner null_value term a next b) 
    (define (iter a result) 
        (cond ((> a b) result)
        ((pred? a) (iter (next a) (combiner (term a) result)))
        (else (iter (next a) result))))
    (iter a null_value))

(define (sum_primes a b) 
    (define (combiner x y) (+ x y)) 
    (define (next_term x) (+ x 1)) 
    (filtered_accumulate prime? combiner 0 square a next_term b))


;;; exercise 1.34
(define (f g) (g 2))

;Value: f

(f f)

;The object 2 is not applicable.
;To continue, call RESTART with an option number:
; (RESTART 3) => Specify a procedure to use in its place.
; (RESTART 2) => Return to read-eval-print level 2.
; (RESTART 1) => Return to read-eval-print level 1.


(define (average a b) (/ (+ a b) 2))
(define (close_enough? a b) (< (abs (- a b)) .0000001))


(define (search f neg_value pos_value) (let ((a (f neg_value)) (b (f pos_value))(avg (average neg_value pos_value))) 
    (cond ((close_enough? (f avg) 0) avg) 
    ((< (f avg) 0) (search f avg pos_value))
    ((> (f avg) 0) (search f neg_value avg))
    ((= (f avg) 0) avg)
)))


(define (half_interval f a b) (cond ((and (> (f a) 0) (< (f b) 0)) (search f b a))
    ((and (> (f b) 0) (< (f a) 0)) (search f a b))
    (else (display "wrong input"))))


(define (fixed_point f guess) (let ((next (f guess))) (newline) (display guess)  (if (close_enough? guess next) next (fixed_point f next))))

(fixed_point sin 2.0)

;Value: .1799525495914556

(fixed_point cos 1.0)
;Value: .7387603198742113


(define (sqrt x) (fixed_point (lambda (y) (/ (+ y (/ x y)) 2.0)) 1.0))

;; exercise 1.36

(define (power ) (fixed_point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2.0)) 10.0))

;; exercise 1.37
(define (cont-frac n d k) (define (iter i result) (if (= i 0) result (iter (- i 1) (/ (n i) (+ (d i) result))))) (iter k 0))

;;Example

(cont-frac (lambda (x) 1.0) (lambda (y) 1.0) 11)  

;Value: .6180555555555556

;; exercise 1.38

(define (euler_identity k) (+ 2.0 (cont-frac (lambda (x) 1.0) (lambda (y) (if (= (remainder y 3.0) 2.0) (- y (/ (- y 2.0) 3.0)) 1.0)) k)))


;; exercise 1.39
(define (tangent x k) (/ (* -1 (cont-frac (lambda (a) (* -1 (square x))) (lambda (b) (- (* 2.0 b) 1)) (- k 1))) x))



(define (average_damp f) (lambda (x) (average x (f x))))

(define (sqrt x) (fixed_point (average_damp (lambda (y) (/ x y))) 1.0))

(define (cube_root x) (fixed_point (average_damp (lambda (y) (/ x (square y)))) 1.0))


(define dx 0.0000001)

(define (derivative g) (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton_equation g x) (lambda (x) (- x (/ (g x) ((derivative g) x)))))

(define (newton_method g x) (fixed_point (newton_equation g x) 1.0))

(define (sqrt x) (newton_method (lambda (y) (- (square y) x)) x))

;; exercise 1.40

(define (cubic a b c) (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))


;; exercise 1.41
(define (double f) (lambda (x) (f (f x))))
(define (inc x) (+ x 1))
(((double (double double)) inc) 5)


;;exercise 1.42

(define (compose f g) (lambda (x) (f (g x))))


;;exercise 1.43

(define (compose_n f n) (define (iter count result) (if (= count 0) result (iter (- count 1) (compose f result)))) (iter n (lambda (x) x)))

((compose_n square 2) 5)

;Value: 625


;;;exercise 1.44

(define (smooth f) (lambda (x) (/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3.0)))

(define (smooth_n f n) ((compose_n smooth n) f))

;;;exercise 1.45

(define (nth_root x n) (fixed_point ((compose_n average_damp (- n 2)) (lambda (y) (/ x (exp y (- n 1))))) 1.0))

;;;exercise 1.46

(define (iterative-improve improve good_enough?) 
    (define (iterate guess) 
    (if (good_enough? guess (improve guess)) 
    (improve guess)
    (iterate (improve guess))))
    (lambda (guess) (iterate (improve guess))))

(define (fixed_point f guess) ((iterative-improve f close_enough?) guess))




