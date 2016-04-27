#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "WebsiteCode.rkt")
(require "tools.rkt")
(require "Images.rkt")
(define plyr-lst-stat 4)
(define plyr-lst-rev #f)
(define page4 #f)

  



;Updates the player-stats screen based on user clicks.
;Changes how the stats are categorized
; Takes x which is the x-mouse location of the mouse so that we can call the below function inside
(define (player-stats x)
  (define a (- x 120))
  (cond [(< a 36)(if (= plyr-lst-stat 1)(set! plyr-lst-rev (not plyr-lst-rev))(set! plyr-lst-stat 1))]
         [(and (> a 154)(< a 190))(if (= plyr-lst-stat 2)(set! plyr-lst-rev (not plyr-lst-rev))(set! plyr-lst-stat 2))]
         [(and (> a 191)(< a 226))(if (= plyr-lst-stat 3)(set! plyr-lst-rev (not plyr-lst-rev))(set! plyr-lst-stat 3))]
         [(and (> a 225))(if (= plyr-lst-stat 4)(set! plyr-lst-rev (not plyr-lst-rev))(set! plyr-lst-stat 4))])
  (player-list-image)
  )



;Shows players in order based on 1 of 4 categories defined by the above function.
;Can also show thestats in reverse if double clicked.
(define (player-list-image)
  (overlay/align "middle" "top"
  (player-list plyr-lst-stat plyr-lst-rev)
  (rectangle 500 500 "solid" "black")))


; These functions take a list which in this case is the list of all-games defined previously
; They will then (given a game) parse the string and rip out specific data to be printed to the screen

(define (grab-date lst)
  (car lst))
(define (grab-dragon-score lst)
  (car (cdr (cdr lst))))
(define (grab-opp-score lst)
  (car (cdr (cdr (cdr lst)))))

; Creates the games-image
;Prints the date and scores of each game
;along with who the opponenet of each game was.
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
(beside
        (overlay/align "middle" "top" 
        (above
        (text "Dragons   " 20 "gold")
        (text (grab-dragon-score (get-num-list x all-games)) 20 "gold")
        )
        (rectangle 225 340 "solid" "black")
        )
        (overlay/align "middle" "top" 
         (text " - " 20 "gold")
         (rectangle 50 340 "solid" "black"))
        (overlay/align "middle" "top"  
        (above
        (text (string-append "   " (get-team-name (string->number (cadr (get-num-list x all-games)))) "  ") 20 "gold")
        (text (grab-opp-score (get-num-list x all-games)) 20 "gold"))        
        (rectangle 225 340 "solid" "black"))
)))
 

 
;Creates an image which shows the stats of the two Goalies
;Combines "DRAGONS" title with two images generates by "goalie-stats"
(define goalie-image 
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


;gets a list containing 3 numbers from "WebsiteCode.rkt"
;these #'s represent the 3 players who have been playing the most consistantly well based
;on a complex algorithm
(define tp3 (trending-top-3))

;Creates the main image to be drawn on screen
;Draws "DRAGONS" in big letters as this is the home page
;Shows the goal, assist, point, and win leaders.
;Draws the "trending players" as defined above
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

;sets the default draw_image to be the home screen a.k.a. the "point leaders" screen
(define draw_img dragons-image)

;Draws the menu & draw_img to the universe.
;draw_img can be a dynamic image from 4 different pages
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
  (cond [(mouse=? a-key "button-down")
      (cond [(and (< x-mouse 125) (< y-mouse 20))      
              (begin (set! draw_img dragons-image) (games-image-flag false))]             
             [(and  (> x-mouse 125) (< x-mouse 250) (< y-mouse 20))
              (begin (set! draw_img goalie-image) (games-image-flag false))]
             [(and (> x-mouse 250) (< x-mouse 375)(< y-mouse 20))
               (begin (set! draw_img (player-list-image)) (set! page4 #t)(games-image-flag false))]                
             [(and page4
                   (and (> x-mouse 120) (< y-mouse 40) (> y-mouse 20)(< x-mouse 380)))
              (begin (set! draw_img (player-stats x-mouse)) (games-image-flag false))]
             [(and (> x-mouse 375) (< y-mouse 20))
              (begin (set! draw_img (games-image n)) (games-image-flag true))]
           
           )
      (cond [(and(< y-mouse 20)(not(and(> x-mouse 250) (< x-mouse 375)))) (set! page4 #f)])
      ])
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

;the "main" function of the code that shows us all of the above information          
(big-bang '(75 . 75) (to-draw create-scene) (on-mouse change) (on-key key-change))
