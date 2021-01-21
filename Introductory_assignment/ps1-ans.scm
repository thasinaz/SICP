(define fact
  (lambda (n)
    (if (= n 0)
        1
        (* n (fact (- n 1))))))

(fact 243)

(define comb
  (lambda (n k)
    (/ (fact n)
       (* (fact k)
          (fact (- n k))))))

(comb 243 90)

(define foo1
  (lambda (x)
    (* x x)))

(define foo2
  (lambda (x y)
    (/ x y)))

(define foo3
  (lambda (x)
    (lambda (y)
      (/ x y))))

(define foo4
  (lambda (x)
    (x 3)))

(define foo5
  (lambda(x)
    (cond ((= x 2)
           (lambda () x))
          (else
           (lambda () (* x 3))))))

(define foo6
  (lambda (x)
    (x (lambda (y) (y y)))))

(foo2 3 1)

((foo3 3) 1)

(foo4 +)

((foo5 1))
