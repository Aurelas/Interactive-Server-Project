#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "WebsiteCode.rkt")
(provide goal-leader)
(provide assist-leader)
(provide point-leader)
(provide win-leader)

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

(define goalie-image
(overlay/offset
(overlay/offset
(overlay (text "Dragons" 42 "gold")
         (rectangle 400 80 "solid" "forestgreen"))
0 60
(overlay (text "Goaltenders" 16 "gold")
         (rectangle 400 40 "solid" "black")))
0 100
(above (rectangle 400 4 "solid" "gold")
(beside
        (overlay (goalie-stats (car Goalies))
         (rectangle 194 76 "solid" "black"))
        (rectangle 4 76 "solid" "black")
        (rectangle 4 76 "solid" "gold")
        (rectangle 4 76 "solid" "black")
        (overlay (goalie-stats (caddr Goalies))
         (rectangle 194 76 "solid" "black"))
        ))))

;Creates the main image to be drawn on screen, and later saved
(define dragons-image
(overlay/offset
(overlay/offset
(overlay (text "Dragons" 42 "gold")
         (rectangle 400 80 "solid" "forestgreen"))
0 60
(overlay (text "Point Leaders" 16 "gold")
         (rectangle 400 40 "solid" "black")))
0 100
(beside
        (overlay (goal-leader)
         (rectangle 100 80 "solid" "black"))
        (overlay (assist-leader)
         (rectangle 100 80 "solid" "black"))
        (overlay (point-leader)
         (rectangle 100 80 "solid" "black"))
        (overlay (win-leader)
         (rectangle 100 80 "solid" "black")))))
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
            (rectangle 198 18 "solid" "black")
            (rectangle 200 20 "solid" "gray")            
            ))
           (overlay            
            (text "Goalie Stats" 18 "gold")
            (overlay
            (rectangle 198 18 "solid" "black")
            (rectangle 200 20 "solid" "gray")            
            )))
         draw_img))
 
 
;(animate create-scene)

(define (change w x-mouse y-mouse a-key)
  (if (mouse=? a-key "button-down")
      (if (and (< x-mouse 200) (< y-mouse 20))
          (set! draw_img dragons-image)
       (if (and (> x-mouse 200) (< y-mouse 20))
           (set! draw_img goalie-image)
           0
           ))0)
 )
(big-bang '(75 .75) (to-draw create-scene) (on-mouse change))