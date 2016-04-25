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




; Takes a player-id (a number) and gets the goals from that player and compares it to other players
; ultimately giving a ranking of the player against other players.

(define (get-rank-goals player-id)
  (define (helper player-id i rank)
    (if (> i (length aP))
        (+ rank 1)
    (if (null? (get-player player-id))
        rank
        (if (null? (get-goals (get-player player-id)))
            rank
            (if (null? (get-player i))
                (helper player-id (+ i 1) (length aP))
                (if (null? (get-goals (get-player i )))
                    (helper player-id (+ i 1) (length aP))
                    (if (>= (string->number (get-goals (get-player player-id)) 10) (string->number (get-goals (get-player i)) 10))
                        (helper player-id (+ i 1 ) (- rank 1))
                        (helper player-id (+ i 1) rank))))))))
 
  (helper player-id 1 (length aP)))

; Same as above but with 'assists'

(define (get-rank-assists player-id)
  (define (helper player-id i rank)
    (if (> i (length aP))
        (+ rank 1)
    (if (null? (get-player player-id))
        rank
        (if (null? (get-assists (get-player player-id)))
            rank
            (if (null? (get-player i))
                (helper player-id (+ i 1) (length aP))
                (if (null? (get-assists (get-player i )))
                    (helper player-id (+ i 1) (length aP))
                    (if (>= (string->number (get-assists (get-player player-id)) 10) (string->number (get-assists (get-player i)) 10))
                        (helper player-id (+ i 1 ) (- rank 1))
                        (helper player-id (+ i 1) rank))))))))
 
  (helper player-id 1 (length aP)))

; Same as above but with 'points'

(define (get-rank-points player-id)
  (define (helper player-id i rank)
    (if (> i (length aP))
        (+ rank 1)
    (if (null? (get-player player-id))
        rank
        (if (null? (get-points (get-player player-id)))
            rank
            (if (null? (get-player i))
                (helper player-id (+ i 1) (length aP))
                (if (null? (get-points (get-player i )))
                    (helper player-id (+ i 1) (length aP))
                    (if (>= (string->number (get-points (get-player player-id)) 10) (string->number (get-points (get-player i)) 10))
                        (helper player-id (+ i 1 ) (- rank 1))
                        (helper player-id (+ i 1) rank))))))))
 
  (helper player-id 1 (length aP)))

    

; Uses the location on the y-axis of the mouse to report back a plyer-id.
; Is mainly used on the 'players' screen where plays appear in a list format.

  

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


;Creates the player-stats screen
; Takes x which is the y-mouse location of the mouse so that we can call the above function inside
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



; Multiple checks like these throughout the code, just to make sure that if we grab a player with data that hasn't been implemented yet
; It wont completely break the code.
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

(overlay (text(number->string(get-rank-goals y)) 20 "gold")
(overlay (rectangle 78 48 "solid" "black")
         (rectangle 80 50 "solid" "gold")))

(overlay (text(number->string(get-rank-assists y)) 20 "gold")
(overlay (rectangle 78 48 "solid" "black")
         (rectangle 80 50 "solid" "gold")))

(overlay (text(number->string(get-rank-points y)) 20 "gold")
(overlay (rectangle 78 48 "solid" "black")
         (rectangle 80 50 "solid" "gold"))))


(rectangle 500 500 "solid" "black")))


;Draws the player list image
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

; These functions take a list which in this case is the list of all-games defined previously
; They will then (given a game) parse the string and rip out specific data to be printed to the screen

(define (grab-date lst)
  (car lst))
(define (grab-dragon-score lst)
  (car (cdr (cdr lst))))
(define (grab-opp-score lst)
  (car (cdr (cdr (cdr lst)))))

; Creates the games-image

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
         (text "HomeTeam " 20 "gold")
         300 0
         (text "Opponent: " 20 "gold"))
(rectangle 500 40 "solid" "black"))

(overlay
(beside
        (text "Dragons   " 20 "gold")

(beside
        (text (grab-dragon-score (get-num-list x all-games)) 20 "gold")
        (text " - " 20 "gold")
        (text (grab-opp-score (get-num-list x all-games)) 20 "gold"))
        (text "   Opponent  " 20 "gold"))
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
(rectangle 500 100 "solid" "black")))
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
 
 
;Processes all of our mouse clicks depeding on where the mouse is when a button is clicked and if we are on the
; correct screen to process unique click input.

(define (change w x-mouse y-mouse a-key)
  (if (mouse=? a-key "button-down")
      (if (and (< x-mouse 125) (< y-mouse 20))
          (begin (set! draw_img dragons-image) (games-image-flag false))
       (if (and (and (> x-mouse 125) (< x-mouse 250)) (< y-mouse 20))
           (begin (set! draw_img goalie-image) (games-image-flag false))
           (if (and (and (> x-mouse 250) (< x-mouse 375)) (< y-mouse 20))
               (begin (set! draw_img player-list-image) (games-image-flag false))                 
               (if (and (eqv? draw_img player-list-image) (and (> x-mouse 50) (and (< y-mouse 500) (> y-mouse 20))))
                   (begin (set! draw_img (player-stats y-mouse)) (games-image-flag false))
                   (if (and (> x-mouse 375) (< y-mouse 20))
                       (begin (set! draw_img (games-image n)) (games-image-flag true))
                       0)))
           
           ))0)
 )

;These are helper functions and defines needed by the key-change function which is used to
; parse arrow keys sent by the user.

; Flag will keep track if we are on the correct screen of the program to start accepting arrow key input
(define (games-image-flag f)
  (set! flag f))
;defined as false because the first screen of the program does not accept arrow key input
(define flag #f)
; N is our 'game number' , when we start to take arrow key input we will traverse the list of games,
; n keeps track of what game we are on at a specific time
(define n 1)


;Handles arrow key input if we are on the correct screen 
(define (key-change w a-key) ; w is worldstate and a-key is the key to prcoess
  (if (equal? flag true)
  (cond
    [(key=? a-key "up")     (begin (if (= n 1)(set! n 1)(set! n (- n 1)))(set! draw_img (games-image n)) (games-image-flag true))]
    [(key=? a-key "down")   (begin (if (= n (length all-games))(set! n (length all-games))(set! n (+ n 1)))(set! draw_img (games-image n)) (games-image-flag true))]
    [else w])
  w))

           
(big-bang '(75 . 75) (to-draw create-scene) (on-mouse change) (on-key key-change))

; TODO
; Use http://m.jdbjohnbrown.net/ as reference
; Uses arrows keys up/down to rotate through games
; create a 3rd button labeled "games" add functionality to also iterate through past games and eventual games
