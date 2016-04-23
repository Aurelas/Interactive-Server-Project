#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "WebsiteCode.rkt")
(provide goal-leader)
(provide assist-leader)
(provide point-leader)
(provide win-leader)

(define (get-num-list x lst)
  (define (loop n x lst)
    (if (null? lst) '()
        (if (= n x) (car lst)
            (loop (+ n 1) x (cdr lst)))))
  (loop 1 x lst)
  )

;Creates a simple image which shows the top goal scorer, and his goal total
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

(define (goalie-stats x)
  (define p (get-player (car x)))
  (beside (text (string-append " "(get-number p) " ") 74 "gold")
           (above/align "left"
                 (text (string-append (substring (get-firstname p) 0 1) ". " (get-lastname p)) 14 "gold")
                 (text (string-append "W: " (number->string (cadr x))) 14 "gold")
                 (text (string-append "L: " (number->string (caddr x))) 14 "gold")
                 (text (string-append "T: " (number->string (cadddr x))) 14 "gold")))  
  )

;;Work in progress, will give rank of stats

;(define (get-rank-goals player-id)
;  (define (helper player-id i rank)
;    (if (eqv? (get-player player-id) '()) 
;        rank
;        (if (null? (get-goals (get-player player-id)))
;            rank
;            (if (eqv? (get-player i) '())
;                rank
;                (if (null? (get-goals (get-player i)))
;                    rank
;                    (if (> (get-goals (get-player player-id)) (get-goals (get-player i)))
;                        (helper player-id (+ i 1) (- rank 1))
;                        (helper player-id (+ i 1) rank)))))))
;  (helper player-id 1 12))

;(get-rank-goals 1)
    


  

(define (get-player-id-from-click ymouse)
  (if (and (> ymouse 20) (<= ymouse 42))
      1
      (if (and (> ymouse 42) (<= ymouse 64))
          2
          (if (and (> ymouse 64) (<= ymouse 88))
              3
              (if (and (> ymouse 88) (<= ymouse 110))
                  4
                  (if (and (> ymouse 110) (<= ymouse 134))
                      5
                      (if (and (> ymouse 134) (<= ymouse 158))
                          6
                          (if (and (> ymouse 158) (<= ymouse 180))
                              7
                              (if (and (> ymouse 180) (<= ymouse 204))
                                  8
                                  (if (and (> ymouse 204) (<= ymouse 226))
                                      9
                                      (if (and (> ymouse 226) (<= ymouse 250))
                                          10
                                          (if (and (> ymouse 250) (<= ymouse 270))
                                              11
                                              (if (and (> ymouse 270) (<= ymouse 295))
                                                  12
                                                  (if (and (> ymouse 295) (<= ymouse 320))
                                                      13
                                                      0))))))))))))))


(define (player-stats x)
(define y (get-player-id-from-click x))
(above
(beside
(overlay
(overlay
        (text "Player #" 20 "gold")
        (rectangle 78 48 "solid" "black"))
(rectangle 80 50 "solid" "gold"))

(overlay
(overlay
        (text "Player" 20 "gold")
        (rectangle 178 48 "solid" "black"))
(rectangle 180 50 "solid" "gold"))

(overlay
(overlay
        (text "Goals" 20 "gold")
        (rectangle 78 48 "solid" "black"))
(rectangle 80 50 "solid" "gold"))

(overlay
(overlay
        (text "Assists" 20 "gold")
        (rectangle 78 48 "solid" "black"))
(rectangle 80 50 "solid" "gold"))

(overlay
(overlay
        (text "Points" 20 "gold")
        (rectangle 80 48 "solid" "black"))
(rectangle 80 50 "solid" "gold")))




(beside
(overlay (text (if (eqv? (get-player y) '()) "empty" (if (null? (get-number (get-player y))) "empty" (get-number (get-player y)))) 20 "gold")
(overlay (rectangle 78 48 "solid" "black")
         (rectangle 80 50 "solid" "gold")))

(overlay (text (if (eqv? (get-player y) '()) "empty" (if (null? (get-fullname (get-player y))) "empty" (get-fullname (get-player y)))) 20 "gold")
(overlay (rectangle 178 48 "solid" "black")
         (rectangle 180 50 "solid" "gold")))

(overlay (text (if (eqv? (get-player y) '()) "empty" (if (null? (get-goals (get-player y))) "empty" (get-goals (get-player y)))) 20 "gold")
(overlay (rectangle 78 48 "solid" "black")
         (rectangle 80 50 "solid" "gold")))

(overlay (text (if (eqv? (get-player y) '()) "empty" (if (null? (get-assists (get-player y))) "empty" (get-assists (get-player y)))) 20 "gold")
(overlay (rectangle 78 48 "solid" "black")
         (rectangle 80 50 "solid" "gold")))

(overlay (text (if (eqv? (get-player y) '()) "empty" (if (null? (get-points (get-player y))) "empty" (get-points (get-player y)))) 20 "gold")
(overlay (rectangle 78 48 "solid" "black")
         (rectangle 80 50 "solid" "gold"))))

(beside
(overlay
(overlay
        (text "Rank:" 20 "gold")
        (rectangle 80 48 "solid" "black"))
(rectangle 80 50 "solid" "gold"))

(overlay (text " - " 20 "gold")
(overlay (rectangle 178 48 "solid" "black")
         (rectangle 180 50 "solid" "gold")))


(overlay (rectangle 78 48 "solid" "black")
         (rectangle 80 50 "solid" "gold"))

(overlay (rectangle 78 48 "solid" "black")
         (rectangle 80 50 "solid" "gold"))

(overlay (rectangle 78 48 "solid" "black")
         (rectangle 80 50 "solid" "gold")))


(rectangle 500 500 "solid" "black")))

(define player-list-image
(above
(beside
(overlay
(overlay (text "1" 20 "gold")
         (rectangle 48 20 "solid" "black"))
(rectangle 50 22 "solid" "gold"))

(overlay
 (text (if (eqv? (get-player 1) '()) "empty" (if (null? (get-fullname (get-player 1))) "empty" (get-fullname (get-player 1)))) 15 "gold" )
 (overlay (rectangle 450 20 "solid" "black")
         (rectangle 450 22 "solid" "gold"))))
(beside
(overlay
(overlay (text "2" 20 "gold")
         (rectangle 48 20 "solid" "black"))
(rectangle 50 22 "solid" "gold"))

(overlay
 (text (if (eqv? (get-player 2) '()) "empty" (if (null? (get-fullname (get-player 2))) "empty" (get-fullname (get-player 2)))) 15 "gold" )
 (overlay (rectangle 450 20 "solid" "black")
         (rectangle 450 22 "solid" "gold"))))

(beside
(overlay
(overlay (text "3" 20 "gold")
         (rectangle 48 20 "solid" "black"))
(rectangle 50 22 "solid" "gold"))

 (overlay
 (text (if (eqv? (get-player 3) '()) "empty" (if (null? (get-fullname (get-player 3))) "empty" (get-fullname (get-player 3)))) 15 "gold" )
 (overlay (rectangle 450 20 "solid" "black")
         (rectangle 450 22 "solid" "gold"))))

(beside
(overlay
(overlay (text "4" 20 "gold")
         (rectangle 48 20 "solid" "black"))
(rectangle 50 22 "solid" "gold"))

 (overlay
 (text (if (eqv? (get-player 4) '()) "empty" (if (null? (get-fullname (get-player 4))) "empty" (get-fullname (get-player 4)))) 15 "gold" )
 (overlay (rectangle 450 20 "solid" "black")
         (rectangle 450 22 "solid" "gold"))))

(beside
(overlay
(overlay (text "5" 20 "gold")
         (rectangle 48 20 "solid" "black"))
(rectangle 50 22 "solid" "gold"))

 (overlay
 (text (if (eqv? (get-player 5) '()) "empty" (if (null? (get-fullname (get-player 5))) "empty" (get-fullname (get-player 5)))) 15 "gold" )
 (overlay (rectangle 450 20 "solid" "black")
         (rectangle 450 22 "solid" "gold"))))

(beside
(overlay
(overlay (text "6" 20 "gold")
         (rectangle 48 20 "solid" "black"))
(rectangle 50 22 "solid" "gold"))

 (overlay
 (text (if (eqv? (get-player 6) '()) "empty" (if (null? (get-fullname (get-player 6))) "empty" (get-fullname (get-player 6)))) 15 "gold" )
 (overlay (rectangle 450 20 "solid" "black")
         (rectangle 450 22 "solid" "gold"))))

(beside
(overlay
(overlay (text "7" 20 "gold")
         (rectangle 48 20 "solid" "black"))
(rectangle 50 22 "solid" "gold"))

 (overlay
 (text (if (eqv? (get-player 7) '()) "empty" (if (null? (get-fullname (get-player 7))) "empty" (get-fullname (get-player 7)))) 15 "gold" )
 (overlay (rectangle 450 20 "solid" "black")
         (rectangle 450 22 "solid" "gold"))))

(beside
(overlay
(overlay (text "8" 20 "gold")
         (rectangle 48 20 "solid" "black"))
(rectangle 50 22 "solid" "gold"))

 (overlay
 (text (if (eqv? (get-player 8) '()) "empty" (if (null? (get-fullname (get-player 8))) "empty" (get-fullname (get-player 8)))) 15 "gold" )
 (overlay (rectangle 450 20 "solid" "black")
         (rectangle 450 22 "solid" "gold"))))

(beside
(overlay
(overlay (text "9" 20 "gold")
         (rectangle 48 20 "solid" "black"))
(rectangle 50 22 "solid" "gold"))

 (overlay
 (text (if (eqv? (get-player 9) '()) "empty" (if (null? (get-fullname (get-player 9))) "empty" (get-fullname (get-player 9)))) 15 "gold" )
 (overlay (rectangle 450 20 "solid" "black")
         (rectangle 450 22 "solid" "gold"))))

(beside
(overlay
(overlay (text "10" 20 "gold")
         (rectangle 48 20 "solid" "black"))
(rectangle 50 22 "solid" "gold"))

 (overlay
 (text (if (eqv? (get-player 10) '()) "empty" (if (null? (get-fullname (get-player 10))) "empty" (get-fullname (get-player 10)))) 15 "gold" )
 (overlay (rectangle 450 20 "solid" "black")
         (rectangle 450 22 "solid" "gold"))))

(beside
(overlay
(overlay (text "11" 20 "gold")
         (rectangle 48 20 "solid" "black"))
(rectangle 50 22 "solid" "gold"))

 (overlay
 (text (if (eqv? (get-player 11) '()) "empty" (if (null? (get-fullname (get-player 11))) "empty" (get-fullname (get-player 11)))) 15 "gold" )
 (overlay (rectangle 450 20 "solid" "black")
         (rectangle 450 22 "solid" "gold"))))

(beside
(overlay
(overlay (text "12" 20 "gold")
         (rectangle 48 20 "solid" "black"))
(rectangle 50 22 "solid" "gold"))

 (overlay
 (text (if (eqv? (get-player 12) '()) "empty" (if (null? (get-fullname (get-player 12))) "empty" (get-fullname (get-player 12)))) 15 "gold" )
 (overlay (rectangle 450 20 "solid" "black")
         (rectangle 450 22 "solid" "gold"))))

(beside
(overlay
(overlay (text "13" 20 "gold")
         (rectangle 48 20 "solid" "black"))
(rectangle 50 22 "solid" "gold"))

 (overlay
 (text (if (eqv? (get-player 13) '()) "empty" (if (null? (get-fullname (get-player 13))) "empty" (get-fullname (get-player 13)))) 15 "gold" )
 (overlay (rectangle 450 20 "solid" "black")
         (rectangle 450 22 "solid" "gold"))))

(rectangle 400 50 "solid" "black")

))


(define (grab-date lst)
  (car lst))
(define (grab-dragon-score lst)
  (car (cdr (cdr lst))))
(define (grab-opp-score lst)
  (car (cdr (cdr (cdr lst)))))

(define (games-image x)
(above
 
(overlay
(beside (text "Game:  " 20 "gold")
        (text (number->string x) 20 "gold"))
         (rectangle 500 40 "solid" "black"))

(overlay
(beside (text "Time:  " 20 "gold")
        (text (grab-date (get-num-list x all-games)) 20 "gold"))
         (rectangle 500 40 "solid" "black"))

(overlay (text "Score:  " 20 "gold")
         (rectangle 500 40 "solid" "black"))
(overlay
(overlay/offset
         (text "Home Team: " 20 "gold")
         300 0
         (text "Opponent: " 20 "gold"))
(rectangle 500 40 "solid" "black"))

(overlay
(beside
        (text "TeamNameHomePH   " 20 "gold")

(beside
        (text (grab-dragon-score (get-num-list x all-games)) 20 "gold")
        (text " - " 20 "gold")
        (text (grab-opp-score (get-num-list x all-games)) 20 "gold"))
        (text "  TeamNameOppPH   " 20 "gold"))
(rectangle 500 100 "solid" "black"))

(rectangle 500 100 "solid" "black")




))
 

 

(define goalie-image ;goalie stats
(above
(overlay/offset
(overlay/offset
(overlay (text "Dragons" 42 "gold")
         (rectangle 500 80 "solid" "forestgreen"))
0 60
(overlay (text "Goaltenders" 16 "gold")
         (rectangle 500 40 "solid" "black")))
0 100
(above (rectangle 500 4 "solid" "gold")
(beside
        (overlay (goalie-stats (car Goalies))
         (rectangle 244 76 "solid" "black"))
        (rectangle 4 76 "solid" "black")
        (rectangle 4 76 "solid" "gold")
        (rectangle 4 76 "solid" "black")
        (overlay (goalie-stats (caddr Goalies))
         (rectangle 244 76 "solid" "black")))))
(rectangle 500 100 "solid" "black")))
(define tp3 (trending-top-3))
;Creates the main image to be drawn on screen, and later saved
(define dragons-image   ;point leaders
  
(above 
(overlay/offset
(overlay/offset
(overlay (text "Dragons" 42 "gold")
         (rectangle 500 80 "solid" "forestgreen"))
0 60
(overlay (text "Point Leaders" 16 "gold")
         (rectangle 500 40 "solid" "black")))
0 100
(beside
        (overlay (goal-leader)
         (rectangle 125 80 "solid" "black"))
        (overlay (assist-leader)
         (rectangle 125 80 "solid" "black"))
        (overlay (point-leader)
         (rectangle 125 80 "solid" "black"))
        (overlay (win-leader)
         (rectangle 125 80 "solid" "black"))))
(overlay

(above
 (text "Trending Players" 16 "gold") 
 (text (get-fullname (get-player (car tp3))) 12 "gold")
 (text (get-fullname (get-player (cadr tp3))) 12 "gold")
 (text (get-fullname (get-player (caddr tp3))) 12 "gold"))
(rectangle 500 100 "solid" "black")
)
   
)
 )
;dragons-image

(define draw_img dragons-image)
;saves the image generated to the temp directory
;(displayln (string-append "Image saved to: " (path->string(find-system-path 'temp-dir)) "\\img.png"))
;(save-image dragons-image (string->path (string-append (path->string(find-system-path 'temp-dir)) "\\img.png")))

(define (create-scene x)
  (above
   (beside (overlay
            (text "Point Leaders" 18 "gold")
            (overlay
            (rectangle 123 18 "solid" "black")
            (rectangle 125 20 "solid" "gray")            
            ))
           (overlay            
            (text "Goalie Stats" 18 "gold")
            (overlay
            (rectangle 123 18 "solid" "black")
            (rectangle 125 20 "solid" "gray")            
            ))
           (overlay
            (text "Players" 18 "gold")
            (overlay
            (rectangle 123 18 "solid" "black")
            (rectangle 125 20 "solid" "gray")
            ))
           (overlay
            (text "Games" 18 "gold")
            (overlay
            (rectangle 123 18 "solid" "black")
            (rectangle 125 20 "solid" "gray")
            )))
         draw_img))
 
 
;(animate create-scene)

(define (change w x-mouse y-mouse a-key)
  (if (mouse=? a-key "button-down")
      (if (and (< x-mouse 125) (< y-mouse 20))
          (set! draw_img dragons-image)
       (if (and (and (> x-mouse 125) (< x-mouse 250)) (< y-mouse 20))
           (set! draw_img goalie-image)
           (if (and (and (> x-mouse 250) (< x-mouse 375)) (< y-mouse 20))
               (set! draw_img player-list-image)                  
               (if (and (eqv? draw_img player-list-image) (and (> x-mouse 50) (and (< y-mouse 500) (> y-mouse 20))))
                   (set! draw_img (player-stats y-mouse))
                   (if (and (> x-mouse 375) (< y-mouse 20))
                       (set! draw_img (games-image 1))
                       0)))
           
           ))0)
 )
(big-bang '(75 . 75) (to-draw create-scene) (on-mouse change))




; TODO
; Use http://m.jdbjohnbrown.net/ as reference
; Uses arrows keys up/down to rotate through games
; create a 3rd button labeled "games" add functionality to also iterate through past games and eventual games