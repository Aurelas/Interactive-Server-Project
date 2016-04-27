#lang racket
;Images.rkt contains sub-code for generating various images used in "MainCode"
;They were moved here to clean-up the code, and allow for it to be better categorized and easier to read.
(require 2htdp/image)
(require "WebsiteCode.rkt")
(provide goal-leader)
(provide assist-leader)
(provide point-leader)
(provide win-leader)
(provide goalie-stats)
(provide get-stat)
(provide ordered-list)
(provide player-list)
;Creates a simple image which shows the top point scorer, and his point total
(define (goal-leader)
  (overlay/offset
  (overlay/offset (text "Goals" 20 "gold") 0 28
                  (text (get-goals most_goals) 25 "gold")) 0 40
                  (text (string-append (substring (get-firstname most_goals) 0 1) ". " (get-lastname most_goals)) 16 "gold")
                  ))
;Creates a simple image which shows the top assist scorer, and his assist total
 (define (assist-leader)
  (overlay/offset
  (overlay/offset (text "Assists" 20 "gold") 0 28
                  (text (get-assists most_assists) 25 "gold")) 0 40
                  (text (string-append (substring (get-firstname most_assists) 0 1) ". " (get-lastname most_assists)) 16 "gold")                  
                  ))

;Creates a simple image which shows the top point scorer, and his point total
(define (point-leader)
  (overlay/offset
  (overlay/offset (text "Points" 20 "gold") 0 28
                  (text (get-points most_points) 25 "gold")) 0 40
                  (text (string-append (substring (get-firstname most_points) 0 1) ". " (get-lastname most_points)) 16 "gold")
                  ))

;Creates a simple image which shows the top goalie of wins, and his win total
(define (win-leader)
  (define p (get-player (car most_wins)))
  (overlay/offset
  (overlay/offset (text "Wins" 20 "gold") 0 28
                  (text (number->string (cadr most_wins)) 25 "gold")) 0 40
                  (text (string-append (substring (get-firstname p) 0 1) ". " (get-lastname p)) 16 "gold")
                  ))
;Creates a simple image which displays the goalies number, his name, and his w/l/t record
(define (goalie-stats x)
  (define p (get-player (car x)))
  (beside (text (string-append " "(get-number p) " ") 74 "gold")
           (above/align "left"
                 (text (string-append (substring (get-firstname p) 0 1) ". " (get-lastname p)) 14 "gold")
                 (text (string-append "W: " (number->string (cadr x))) 14 "gold")
                 (text (string-append "L: " (number->string (caddr x))) 14 "gold")
                 (text (string-append "T: " (number->string (cadddr x))) 14 "gold")))  
  )


; Takes a player-id (a number) and gets the "stat" from that player and compares it to other players
; ultimately giving a ranking of the player against other players.
; The "stat" is determined by a function 'f', with a tiebreaker function 'g'.
; Typical f's and g's are get-goals/get-assists/get-points

;Original by Michael Antrobus ->see https://github.com/oplS16projects/Interactive-Server-Project/tree/MikePatch2
;Edited by John Brown. Made dynamic, added tie breaker. 
(define (get-stat player-id f g)
  (define (helper player-id i rank)
    (if (> i (length aP))
        (+ rank 1)
    (if (null? (get-player player-id))
        rank
        (if (null? (get-points (get-player player-id)))
            rank
            (if (null? (get-player i))
                (helper player-id (+ i 1) (length aP))
                    (let ([a (string->number (f (get-player player-id)) 10)]
                          [b (string->number (f (get-player i)) 10)]
                          [c (string->number (g (get-player player-id)) 10)]
                          [d (string->number (g (get-player i)) 10)]
                          [e (get-fullname (get-player player-id))]
                          [f (get-fullname (get-player i))]
                          )
                      (if (equal? e f) (helper player-id (+ i 1 ) (- rank 1))
                      (if (> a b)
                        (helper player-id (+ i 1 ) (- rank 1))
                         (if (and (= a b) (or (> c d) (= player-id 1)))
                            (helper player-id (+ i 1 ) (- rank 1))
                            (helper player-id (+ i 1) rank))
                        )))
                    ))))) 
  (helper player-id 1 (length aP)))

;Creates a list which places players in order based on their rank from "get-stat"
;Pass in f g as functions. "f" determines the stat to order the players on. "g" is the tiebreaker
;Typical f's and g's are get-goals/get-assists/get-points
;~JBrown
(define (ordered-list f g)
  (define (small-loop players x n)
    (if (null? players) '()
        (if (equal? (get-stat n f g) x)
            n
            (small-loop (cdr players) x (+ n 1)))))
  (define (big-loop lst x)
      (define a (small-loop aP x 1))
      (if (null? a) lst
          (big-loop (append lst (list a)) (+ x 1))))
  (big-loop '() 1))



;Used to standardize the size of elements in "player-list"
;Useful for tweaking the elements with only changing one line of code
;~JBrown
(define txt-size 16)
(define rect-height 21)

;Generates an image containing all of the players listed in aP that can be sorted any 1 of 4 ways or their reversals.
;n is expected to be 1-4.
;1: Sort by player number.
;2: Sort by goals scored
;3: Sort by assists scored
;4: Sort by points scored
;r is expected to be #t or #f.
;if it is #t then category n will be reversed and will sort from low to high.
;the categeory defined by n will be drawn in green, the rest in white
;player names will be drawn in black over green
;~JBrown
(define (player-list n r)
  (define c1 "white")
  (define c2 "white")
  (define c3 "white")
  (define c4 "white")
  (define lst '())
  (define (loop img lst c1 c2 c3 c4)    
    (if (null? lst) img
        (let ([a (get-player (car lst))])
        (loop (above img
              (beside (overlay (text (get-number a) txt-size c1)
                               (rectangle 35 rect-height "solid" "black"))
                      (overlay/align "left" "middle" (text (string-append "  " (substring (get-firstname a) 0 1) ". " (get-lastname a)) txt-size "black")
                               (rectangle 120 rect-height "solid" "forestgreen"))
                      (overlay (text (get-goals a) txt-size c2)
                               (rectangle 35 rect-height "solid" "black"))
                      (overlay (text (get-assists a) txt-size c3)
                               (rectangle 35 rect-height "solid" "black"))
                      (overlay (text (get-points a) txt-size c4)
                               (rectangle 35 rect-height "solid" "black"))))
              (cdr lst) c1 c2 c3 c4))))
  (cond
    [(= n 1) (begin (set! c1 "forestgreen") (set! lst (ordered-list get-number get-points)))]
    [(= n 2) (begin (set! c2 "forestgreen")(set! lst (ordered-list get-goals get-points)))]
    [(= n 3) (begin (set! c3 "forestgreen")(set! lst (ordered-list get-assists get-points)))]
    [(= n 4) (begin (set! c4 "forestgreen")(set! lst (ordered-list get-points get-goals)))])
  (if r (set! lst (reverse lst)) (set! lst lst))
  (loop (above (beside (overlay (text "#" txt-size c1)
                               (rectangle 35 rect-height "solid" "black"))
                      (overlay (text "NAME" txt-size "black")
                               (rectangle 120 rect-height "solid" "forestgreen"))
                      (overlay (text "G" txt-size c2)
                               (rectangle 35 rect-height "solid" "black"))
                      (overlay (text "A" txt-size c3)
                               (rectangle 35 rect-height "solid" "black"))
                      (overlay (text "P" txt-size c4)
                               (rectangle 35 rect-height "solid" "black")))
               (rectangle 260 2 "solid" "gold"))
        lst c1 c2 c3 c4)
  )












