;;; code for PS4.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The PLAY-LOOP procedure takes as its arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds. A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and
;;  a history of the other player's previous plays. The procedure
;;  returns either the symbol C (for "cooperate") or D ("defect").
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
          (else (let ((result0 (strat0 history0 history1))
                      (result1 (strat1 history1 history0)))
                  (play-loop-iter strat0 strat1 (1+ count)
                                  (extend-history result0 history0)
                                  (extend-history result1 history1)
                                  limit)))))
  (play-loop-iter strat0 strat1 0 '() '() (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following procedures are used to compute and
;; print out the players' scores at the end of an
;; iterated game.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "Player 1 Score: ")
    (display (/ (car scores) number-of-games))
    (newline)
    (display "Player 2 Score: ")
    (display (/ (cadr scores) number-of-games))
    (newline)))


(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0) (list score0 score1))
          (else (let ((game (make-game (most-recent-play history0)
                                       (most-recent-play history1))))
                  (get-scores-helper (rest-of-plays history0)
                                     (rest-of-plays history1)
                                     (+ (get-player-points 0 game) score0)
                                     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define (get-point-list game)
  (cadr (assoc game *game-association-list*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This list provides the "game matrix". It is used
;; by the scorekeeping procedures above.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *game-association-list*
  '(((c c) (3 3))
    ((c d) (0 5))
    ((d c) (5 0))
    ((d d) (1 1))))



;; Some selectors and constructors

(define make-game list)


(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)


;; A sampler of strategies

(define (all-defect my-history other-history)
  'd)

(define (poor-trusting-fool my-history other-history)
  'c)

(define (random-strategy my-history other-history)
  (if (= (random 2) 0) 'c 'd))

(define (go-by-majority my-history other-history)
  (define (count-instances-of symbol hist)
    (cond ((empty-history? hist) 0)
          ((eq? (most-recent-play hist) symbol)
           (1+ (count-instances-of symbol (rest-of-plays hist))))
          (else (count-instances-of symbol (rest-of-plays hist)))))
  (let ((ds (count-instances-of 'd other-history))
        (cs (count-instances-of 'c other-history)))
    (if (> ds cs) 'd 'c)))


(define (tit-for-tat my-history other-history)
  (if (empty-history? my-history)
      'c
      (most-recent-play other-history)))


;;; other possibly useful procedures

(define (get-nth-from-last-play n history)
  (list-ref history n))

(define (get-players-action player-no game)
  (list-ref game player-no))

(define (get-nth-from-last-game n my-history other-history)
  (make-game (get-nth-from-last-play n my-history)
             (get-nth-from-last-play n other-history)))


(define (tit-for-two-tats my-history other-history)
  (cond ((empty-history? my-history) 'c)
        ((eq? (most-recent-play other-history) 'c) 'c)
        ((empty-history? (rest-of-plays my-history)) 'c)
        ((eq? (most-recent-play (rest-of-plays other-history)) 'c) 'c)
        (else 'd)))


(define (make-tit-for-n-tats n)
  (define (tit-for-n-tats my-history other-history n)
    (cond ((< n 1) 'd)
          ((empty-history? my-history) 'c)
          ((eq? (most-recent-play other-history) 'c) 'c)
          (else (tit-for-n-tats (rest-of-plays my-history)
                                (rest-of-plays other-history)
                                (- n 1)))))

  (lambda (my-history other-history)
    (tit-for-n-tats my-history other-history n)))


(define (make-dual-strategy strat0 strat1 switch-point)
  (if (> switch-point 0)
      (lambda (my-history other-history)
        (if (even? (floor (/ (length my-history)
                             switch-point)))
            (strat0 my-history other-history)
            (strat1 my-history other-history)))
      (error "switch-point less than 1" switch-point)))

(define (make-triple-strategy strat0 strat1 strat2 switch-point0 switch-point1)
  (make-dual-strategy (make-dual-strategy strat0
                                          strat1
                                          switch-point0)
                      strat2
                      switch-point1))


(define (niceify strat niceness-factor)
  (define (lucky-enough?)
    (< (random 1.0) niceness-factor))

  (lambda (my-history other-history)
    (cond ((eq? (strat my-history other-history) 'c) 'c)
          ((lucky-enough?) 'c)
          (else 'd))))

(define slightly-nice-all-defect (niceify all-defect 0.1))

(define slightly-nice-tit-for-tat (niceify tit-for-tat 0.1))


(define (play-loop strat0 strat1 strat2)
  (define (play-loop-iter strat0 strat1 strat2 count history0 history1 history2 limit)
    (cond ((= count limit) (print-out-results history0 history1 history2 limit))
          (else (let ((result0 (strat0 history0 history1 history2))
                      (result1 (strat1 history1 history0 history2))
                      (result2 (strat2 history2 history0 history1)))
                  (play-loop-iter strat0 strat1 strat2 (1+ count)
                                  (extend-history result0 history0)
                                  (extend-history result1 history1)
                                  (extend-history result2 history2)
                                  limit)))))
  (play-loop-iter strat0 strat1 strat2 0 '() '() '() (+ 90 (random 21))))

(define (print-out-results history0 history1 history2 number-of-games)
  (let ((scores (get-scores history0 history1 history2)))
    (newline)
    (display "Player 1 Score: ")
    (display (/ (car scores) number-of-games))
    (newline)
    (display "Player 2 Score: ")
    (display (/ (cadr scores) number-of-games))
    (newline)
    (display "Player 3 Score: ")
    (display (/ (caddr scores) number-of-games))
    (newline)))

(define (get-scores history0 history1 history2)
  (define (get-scores-helper history0 history1 history2 score0 score1 score2)
    (cond ((empty-history? history0) (list score0 score1 score2))
          (else (let ((game (make-game (most-recent-play history0)
                                       (most-recent-play history1)
                                       (most-recent-play history2))))
                  (get-scores-helper (rest-of-plays history0)
                                     (rest-of-plays history1)
                                     (rest-of-plays history2)
                                     (+ (get-player-points 0 game) score0)
                                     (+ (get-player-points 1 game) score1)
                                     (+ (get-player-points 2 game) score2))))))
  (get-scores-helper history0 history1 history2 0 0 0))

(define *game-association-list*
  '(((c c c) (7 7 7))
    ((c c d) (3 3 9))
    ((c d c) (3 9 3))
    ((d c c) (9 3 3))
    ((c d d) (0 5 5))
    ((d c d) (5 0 5))
    ((d d c) (5 5 0))
    ((d d d) (1 1 1))))


(define (poor-trusting-fool-3 my-history other-history0 other-history1)
  'c)

(define (all-defect-3 my-history other-history0 other-history1)
  'd)

(define (random-strategy-3 my-history other-history0 other-history1)
  (if (= (random 2) 0) 'c 'd))

(define (tough-tit-for-tat my-history other-history0 other-history1)
  (cond ((empty-history? my-history) 'c)
        ((eq? (most-recent-play other-history0) 'd) 'd)
        ((eq? (most-recent-play other-history1) 'd) 'd)
        (else 'c)))

(define (soft-tit-for-tat my-history other-history0 other-history1)
  (cond ((empty-history? my-history) 'c)
        ((eq? (most-recent-play other-history0) 'c) 'c)
        ((eq? (most-recent-play other-history1) 'c) 'c)
        (else 'd)))


(define (get-probability-of-c hist-0 hist-1 hist-2)
  (define *type-association-list*
    '(((c c) 0)
      ((c d) 1)
      ((d c) 1)
      ((d d) 2)))

  (define (update-count count result)
    (if (null? count)
        (update-count (cons 0 0) result)
        (let ((c (car count))
              (times (cdr count)))
          (cons (if (eq? result 'c)
                    (+ c 1)
                    c)
                (+ times 1)))))

  (define (get-probability count)
    (if (null? count)
        ()
        (/ (car count) (cdr count))))

  (define (get-probability-of-c-iter hist-0 prev-hist-1 prev-hist-2 count-0 count-1 count-2)
    (if (empty-history? prev-hist-1)
        (map get-probability (list count-0 count-1 count-2))
        (let ((result (most-recent-play hist-0))
              (type (cadr (assoc (list (most-recent-play prev-hist-1)
                                       (most-recent-play prev-hist-2))
                                 *type-association-list*))))
          (cond ((= type 0)
                 (get-probability-of-c-iter (rest-of-plays hist-0)
                                            (rest-of-plays prev-hist-1)
                                            (rest-of-plays prev-hist-2)
                                            (update-count count-0 result)
                                            count-1
                                            count-2))
                ((= type 1)
                 (get-probability-of-c-iter (rest-of-plays hist-0)
                                            (rest-of-plays prev-hist-1)
                                            (rest-of-plays prev-hist-2)
                                            count-0
                                            (update-count count-1 result)
                                            count-2))
                (else (get-probability-of-c-iter (rest-of-plays hist-0)
                                                 (rest-of-plays prev-hist-1)
                                                 (rest-of-plays prev-hist-2)
                                                 count-0
                                                 count-1
                                                 (update-count count-2 result)))))))

  (if (empty-history? hist-0)
      (list () () ())
      (get-probability-of-c-iter hist-0 (rest-of-plays hist-1) (rest-of-plays hist-2) () () ())))

(define (is-he-a-fool? hist0 hist1 hist2)
  (equal? '(1 1 1) (get-probability-of-c hist0 hist1 hist2)))

(define (could-he-be-a-fool? hist0 hist1 hist2)
  (equal? (list true true true)
          (map (lambda (elt) (or (null? elt) (eqv? elt 1)))
               (get-probability-of-c hist0 hist1 hist2))))

(define (is-soft-tit-for-tat? hist0 hist1 hist2)
  (equal? '(1 1 0) (get-probability-of-c hist0 hist1 hist2)))

(define (could-soft-tit-for-tat? hist0 hist1 hist2)
  (let ((probability-of-c (get-probability-of-c hist0 hist1 hist2)))
    (let ((probability-0 (list-ref probability-of-c 0))
          (probability-1 (list-ref probability-of-c 1))
          (probability-2 (list-ref probability-of-c 2)))
      (and (or (null? probability-0) (eqv? probability-0 1))
           (or (null? probability-1) (eqv? probability-1 1))
           (or (null? probability-2) (eqv? probability-2 0))))))

(define (dont-tolerate-fools my-history other-history0 other-history1)
  (cond ((< (length my-history) 10) 'c)
        ((and (could-he-be-a-fool? other-history0 other-history1 my-history)
              (could-he-be-a-fool? other-history1 other-history0 my-history))
         'd)
        (else 'c)))


(define (make-combined-strategies strat0 strat1 combining)
  (lambda (my-history other-history0 other-history1)
    (combining (strat0 my-history
                       other-history0)
               (strat1 my-history
                       other-history1))))
