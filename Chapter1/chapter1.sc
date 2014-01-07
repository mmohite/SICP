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



