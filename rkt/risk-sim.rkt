#lang racket

(define (generate-attacker-win-percent-table max-attackers max-defenders trials)
  (for/list ([attackers (in-range 1 (add1 max-attackers))])
    (for/list ([defenders (in-range 1 (add1 max-defenders))])
      (simulate-attacker-win-percent attackers defenders trials))))

(define (simulate-attacker-win-percent attackers defenders trials)
  (define-values (attacker-wins defender-wins)
    (for/fold ([attacker-wins 0]
               [defender-wins 0])
              ([i (in-range trials)])
      (if (eq? 'attacker-wins (simulate-battle attackers defenders))
          (values (add1 attacker-wins) defender-wins)
          (values attacker-wins (add1 defender-wins)))))
  (/ attacker-wins (* trials 1.0)))

(define (simulate-battle attackers defenders)
  (cond
    [(<= attackers 0) 'defender-wins]
    [(<= defenders 0) 'attacker-wins]
    [else
     (let-values
         ([(attacker-wins defender-wins)
           (winners (rolls (min attackers 3)) (rolls (min defenders 2)))])       
       (simulate-battle (- attackers defender-wins) (- defenders attacker-wins)))]))

(define (rolls n)
  (define unsorted-rolls
    (for/list ([i (in-range n)])
      (add1 (random 6))))
  (sort unsorted-rolls >))

(define (winners attacker-rolls defender-rolls)
  (define (winners/i attacker-rolls defender-rolls accumulated-attacker-wins accumulated-defender-wins)
    (cond
      [(or (null? attacker-rolls) (null? defender-rolls))
       (values accumulated-attacker-wins accumulated-defender-wins)]
      [(> (first attacker-rolls) (first defender-rolls))
       (winners/i (rest attacker-rolls) (rest defender-rolls)
                  (add1 accumulated-attacker-wins) accumulated-defender-wins)]
      [else
       (winners/i (rest attacker-rolls) (rest defender-rolls)
                  accumulated-attacker-wins (add1 accumulated-defender-wins))]))
  (winners/i attacker-rolls defender-rolls 0 0))  

       
            
            