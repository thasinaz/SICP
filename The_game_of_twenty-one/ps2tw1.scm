;;; Scheme code for Twenty-One Simulator [PS2 Fall '90]

(define (twenty-one player-strategy house-strategy)
  (let ((house-initial-hand (make-new-hand (deal))))
    (let ((player-hand
           (play-hand player-strategy
                      (make-new-hand (deal))
                      (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
          0                                ; ``bust'': player loses
          (let ((house-hand
                 (play-hand house-strategy
                            house-initial-hand
                            (hand-up-card player-hand))))
            (cond ((> (hand-total house-hand) 21)
                   1)                      ; ``bust'': house loses
                  ((> (hand-total player-hand)
                      (hand-total house-hand))
                   1)                      ; house loses
                  (else 0)))))))           ; player loses

(define (play-hand strategy my-hand opponent-up-card)
  (cond ((> (hand-total my-hand) 21) my-hand) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
         (play-hand strategy
                    (hand-add-card my-hand (deal))
                    opponent-up-card))
        (else my-hand)))                ; stay


(define (deal) (+ 1 (random 10)))

(define (make-new-hand first-card)
  (make-hand first-card first-card))

(define (make-hand up-card total)
  (cons up-card total))

(define (hand-up-card hand)
  (car hand))

(define (hand-total hand)
  (cdr hand))

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (+ new-card (hand-total hand))))

(define (hit? your-hand opponent-up-card)
  (newline)
  (display "Opponent up card ")
  (display opponent-up-card)
  (newline)
  (display "Your Total: ")
  (display (hand-total your-hand))
  (newline)
  (display "Hit? ")
  (user-says-y?))


(define (user-says-y?) (eq? (read) 'y))



(define (stop-at n)
  (lambda (hand opponent-up-card)
    (< (hand-total hand) n)))

(define (test-strategy player house n)
  (if (= n 0)
      0
      (+ (twenty-one player house) (test-strategy player house (- n 1)))))

(define (watch-player strategy)
  (lambda (hand opponent-up-card)
    (newline)
    (display "Opponent up card ")
    (display opponent-up-card)
    (newline)
    (display "Your Total: ")
    (display (hand-total hand))
    (newline)
    (let ((decision (strategy hand opponent-up-card)))
      (if decision
          (display "Hit")
          (display "Stay"))
      decision)))

(define (louis hand opponent-up-card)
  (let ((total (hand-total hand))
        (opponent-up-card-value (card-value opponent-up-card)))
    (cond ((< total 12) #t)
          ((> total 16) #f)
          ((and (= total 12)
                (< opponent-up-card-value 4))
           #t)
          ((and (= total 16)
                (= opponent-up-card-value 10))
           #f)
          ((> opponent-up-card-value 6) #t)
          (else #f))))

(define (both strategy-1 strategy-2)
  (lambda (hand opponent-up-card)
    (and (strategy-1 hand opponent-up-card)
         (strategy-2 hand opponent-up-card))))


(define (make-card value suit)
  (cons value suit))

(define (card-value card)
  (car card))

(define (card-suit card)
  (cdr card))

(define (card-set-card card-set)
  (car card-set))

(define (card-set-subset card-set)
  (cdr card-set))

(define (card-set-add-card card-set new-card)
  (cons new-card card-set))

(define (deal)
  (cons (+ 1 (random 10))
        (+ 1 (random 4))))

(define empty-card-set (cons (make-card 0 0) 0))

(define (make-new-hand first-card)
  (make-hand first-card
             (cons first-card empty-card-set)))

(define (make-hand up-card card-set)
  (cons up-card card-set))

(define (hand-up-card hand)
  (car hand))

(define (hand-card-set hand)
  (cdr hand))

(define (hand-total hand)
  (define (iter card-set acc)
    (let ((card (card-set-card card-set)))
      (if (= (card-value card) 0)
          acc
          (iter (cdr card-set)
                (+ acc (card-value card))))))

  (iter (hand-card-set hand) 0))

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (card-set-add-card (hand-card-set hand) new-card)))


(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (term a)))))

  (iter a null-value))

(define (inc n)
  (+ n 1))

(define (make-deck term a next b)
  (accumulate card-set-add-card
              empty-card-set
              term
              a
              next
              b))

(define (fresh-deck)
  (define (card-term x)
    (let ((n (- 52 x)))
      (if (< n 40)
          (make-card (+ (floor (/ n 4)) 1)
                     (+ (remainder n 4) 1))
          (make-card 10
                     (+ (remainder n 4) 1)))))

  (make-deck card-term
             1
             inc
             52))

(define (fast-exp null-value combiner base n)
  (define (iter result a n)
    (cond ((= n 0) result)
          ((even? n) (iter result (combiner a a) (/ n 2)))
          (else (iter (combiner result a) a (- n 1)))))

  (iter null-value base n))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (identity x)
  x)

(define (n-step-deck deck n)
  ((fast-exp identity compose cdr n) deck))

(define (push-n src n dest)
  (if (or (= n 0) (= (car (car src)) 0))
      dest
      (cons (car src)
            (push-n (cdr src)
                    (- n 1)
                    dest))))

(define (merge f deck1 sz1 deck2 sz2)
  (let ((n (min sz1 (f))))
    (if (= n 0)
        (push-n deck2 sz2 empty-card-set)
        (push-n deck1
                n
                (merge f
                       deck2
                       sz2
                       (n-step-deck deck1 n)
                       (- sz1 n))))))

(define (shuffle deck)
  (merge (lambda () 1) deck 26 (n-step-deck deck 26) 26))

(define (shuffle-rand deck)
  (merge (lambda () (+ (random 5) 1)) deck 26 (n-step-deck deck 26) 26))

(define (empty-deck? deck)
  (= (car (car deck)) 0))

(define (twenty-one player-strategy house-strategy deck)
  (let ((house-initial-hand (make-new-hand (car deck)))
        (deck (cdr deck)))
    (if (empty-deck? deck)
        (cons 1 empty-card-set)
        (let ((player-hand-and-deck
              (play-hand player-strategy
                          (make-new-hand (car deck))
                          (hand-up-card house-initial-hand)
                          (cdr deck))))
          (let ((deck (cdr player-hand-and-deck))
                (player-total (hand-total (car player-hand-and-deck))))
            (cond ((= player-total 0) (cons 1 empty-card-set))
                  ((> player-total 21) (cons 0 deck))                                ; ``bust'': player loses
                  (else
                   (let ((house-hand-and-deck
                         (play-hand house-strategy
                                     house-initial-hand
                                     (hand-up-card (car player-hand-and-deck))
                                     deck)))
                     (let ((house-total (hand-total (car house-hand-and-deck)))
                           (deck (cdr house-hand-and-deck)))
                       (cond ((= house-total 0) (cons 1 empty-card-set))
                             ((> house-total 21) (cons 1 empty-card-set))                      ; ``bust'': house loses
                             ((> player-total
                                 house-total)
                              (cons 1 deck))                      ; house loses
                             (else (cons 0 deck))))))))))))           ; player loses

(define empty-hand (make-new-hand (make-card 0 0)))

(define (play-hand strategy my-hand opponent-up-card deck)
  (cond ((> (hand-total my-hand) 21) (cons my-hand deck)) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
         (if (empty-deck? deck)
             (cons empty-hand empty-card-set)
             (play-hand strategy
                        (hand-add-card my-hand (car deck))
                        opponent-up-card
                        (cdr deck))))
        (else (cons my-hand deck))))                ; stay

(define (test-strategy player house shuffle)
  (let ((deck (shuffle (fresh-deck))))
    (define (test deck)
      (if (empty-deck? deck)
          0
          (let ((rc (twenty-one player house deck)))
            (+ (car rc) (test (cdr rc))))))

    (test deck)))

(define (shuffle-n-times n)
  (fast-exp identity compose shuffle-rand n))
