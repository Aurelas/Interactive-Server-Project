#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(provide goal-leader)
(provide assist-leader)
(provide point-leader)
(provide win-leader)
(require net/url)
(provide create-game)
(provide nostats)
(provide all-games)
(provide Goalies)
(provide most_goals)
(provide most_points)
(provide most_assists)
(provide most_wins)
(provide get-player)
(provide get-firstname)
(provide get-lastname)
(provide get-goals)
(provide get-number)
(provide get-assists)
(provide get-points)
(provide remove-last)
(provide string-find)
(provide instr)
(provide apply-generic)
(provide car-str)
(provide cdr-str)
(provide list-last)
(provide but-last)
(define (remove-last lst)
    (if (null? (cdr lst))
        '()
        (cons (car lst) (remove-last (cdr lst)))))
(define (apply-generic op arg) (arg op))


;This searches a string [s] (starting at n) and returns the first location where the character c appears.
;This is more or less redundant with my later instr, but I wrote this for the first segment, and it didn't apply to the second
(define (string-find s c n)
  (if (eq? (string-ref s n) c) n (if (= (+ n 1) (string-length s)) -1 (string-find s c (+ 1 n)))))

;You know, I'm sure these functions MUST exist, but I couldn't find them and it was just faster to write them
;instr searches the string for the substring starting at postion i. j should always be zero at start.
(define (instr string sub i j)
  (if (= i (string-length string)) -1
  (if (= j (string-length sub)) (- i j)
  (if (eq? (string-ref string i) (string-ref sub j)) (instr string sub (+ i 1) (+ j 1)) (instr string sub (+ i 1) 0)) 
      )))

(define (car-str str) (if (equal? str "") "" (substring str 0 1)))
(define (cdr-str str) (if (equal? str "") "" (substring str 1 (string-length str))))

(define (list-last lst) (if (null? lst) '() (car (reverse lst))))
(define (but-last lst) (if (null? lst) '()(reverse (cdr (reverse lst)))))
;I recently set up a piece of software (m.jdbjohnbrown.net) that used php _POST functions to retrieve data from a SQL table.
;I wanted to experiment and see if I could pull out the data using racket.
;The information held in the database is player stats and information for a hockey team I play for.
;I pull this information using a series of commands from net/url. Notably post-pure-port (pain in the butt figuring out how to use it. The internet was useless for once.)
;From here, I load this information into a "player object" with create-player, and access it with various selectors; as well as a display function
(define base "http://jdbjohnbrown.net/gameSync.php")
(define my-site (string->url base))
(define Header (list "Content-Type: application/x-www-form-urlencoded"))
(define target-post (string->bytes/utf-8 (format "f=tgn&id=1")))


;Code that directly contacts my website for the first segment.
;Seriously, try to figure out how to do this with no prior knowledge and the internet is NO help.
;Anyways, this uses post-pure-port to send data to a .php file which then appropriately access a SQL database.
;The information sent is "f=tgn" which tells it to return info from the FUNCTION that gets player info
;also sends "id=x" so that it knows which player to recieve. Anything over 12 will return an empty string
(define (get-player-string x)
  (define in
  (post-pure-port my-site (string->bytes/utf-8 (format (string-append "f=tgn&id=" (number->string x)))) Header) )
  (begin0
  (port->string in)
  (close-input-port in)))

;various selectors for the data held in the player objects. Used in display-player
(define (get-firstname x) (car x))
(define (get-lastname x) (cadr x))
(define (get-fullname x) (string-append (get-lastname x) ", " (get-firstname x) )) ;well actually i never use this one but i wrote it anyways
(define (get-number x) (car (third x)))
(define (get-position x) (cadr (third x)))
(define (get-goals x) (car (cadddr x)))
(define (get-assists x) (cadr (cadddr x)))
(define (get-points x) (caddr (cadddr x)))

;displays an object created with create-player
(define (display-player x)
  (define (print-player x)
    (displayln (string-append (get-firstname x) " " (get-lastname x)))
    (displayln (string-append "Number: " (get-number x)))
    (displayln (string-append "Position: " (get-position x)))
    (displayln (string-append "Goals: " (get-goals x)))
    (displayln (string-append "Assists: " (get-assists x)))
    (displayln (string-append "Points: " (get-points x)))
    )
  (if (null? x) (display "Invalid Player Object") (print-player x))
 
  )

;parses the data from get-player-string
;places the data into an list with the form (firstname lastname (number position) (goals assists points))
(define (create-player x)
  (define s (get-player-string x))
    (define (parse s)     
    (define a (string-find s #\, 0))
    (define f (substring s 0 a))
    (define l (substring s (+ a 1) (string-find s #\, (+ a 1))))
    (set! a (+(string-find s #\, (+ a 1)) 1))
    (set! a (+(string-find s #\, (+ a 1)) 1))
    (define num (substring s a  (string-find s #\, (+ a 1))))
    (set! a (+(string-find s #\, (+ a 1)) 1))
    (define pos (substring s a  (string-find s #\, (+ a 1))))
    (set! a (+(string-find s #\, (+ a 1)) 1))
    (define g (substring s a  (string-find s #\, (+ a 1))))
    (set! a (+(string-find s #\, (+ a 1)) 1))
    (define as (substring s a  (string-find s #\, (+ a 1))))
    (set! a (+(string-find s #\, (+ a 1)) 1))
    (define p (substring s a  (- (string-length s) 2)))
    (list f l (list num pos) (list g as p))
    )
  (if (equal? (substring s 0 7) ",,,,,,,") '() (parse s))
  )

;creates a list of all possible players while running create-player
(define (all-players)
  (define (loop x n)
    (define a (create-player n))
    (if (null? a) x (loop (append x (list a)) (+ n 1)))    )
(loop '() 1))

;strips the previous list of the stats provided
(define nostats (map (lambda (x) (remove-last x)) (all-players)))

;pings the website to return information about a game
(define (get-game-string x)
  (define in
  (post-pure-port my-site (string->bytes/utf-8 (format (string-append "f=ggi&id=" (number->string x)))) Header) )
  (begin0
  (port->string in)
  (close-input-port in)))

;"creates" the game by parsing the string provided by get-game-string
(define (create-game x)
  (define s (get-game-string x))
    (define (parse s)     
      (define a (string-find s #\* 0))
      (define d (substring s 0 a))
      (define opp (substring s (+ a 1) (string-find s #\* (+ a 1))))
      (set! a (+(string-find s #\* (+ a 1)) 1))
      (define for (substring s a  (string-find s #\* (+ a 1)))) 
      (set! a (+(string-find s #\* (+ a 1)) 1))      
      (define against (substring s a  (string-find s #\* (+ a 1))))     
      (set! a (+(string-find s #\* (+ a 1)) 1))
      (define goalie (substring s a  (string-find s #\* (+ a 1))))    
      (set! a (+(string-find s #\* (+ a 1)) 1))
      (define g (substring s a  (- (string-length s) 2)))
      (list d opp for against goalie g)     
    )
  (define (partial s)     
      (define a (string-find s #\* 0))
      (define d (substring s 0 a))
      (define opp (substring s (+ a 1) (string-find s #\* (+ a 1))))      
      (list d opp "0" "0" "0" "()")     
    ) 
  (if (equal? (substring s 0 5) "*****") '() (if (< (string-length (string-trim s)) 28) (partial s) (parse s)))
  )

(define (parse-game-str str)
  (define (check-c c r) (if (eq? (string->number (car-str r)) #f) c (string-append c (car-str r))))
  (define (check-r c r) (if (eq? (string->number (car-str r)) #f) r (cdr-str r)))
  (define (loop lst str state)
    (define c (car-str str))
    (define r (cdr-str str))
    (if (= (string-length str) 0) lst
        (cond
          [(not (eq? (string->number c) #f))
               (if (= state 0) (loop (append lst (list (list (string->number (check-c c r))))) (check-r c r) 1)
                   (loop (append (but-last lst) (list (append (list-last lst) (list (string->number (check-c c r))))))
                         (check-r c r) 1))]
          [(equal? c ";") (loop lst r 0)]
          [else (loop lst r state)])
    ))
  (if (equal? str "") '()
  (loop '() str 0)  )  
  
  )


;loops through create-game until it gets a null value, creating a list of all games
(define all-games ((lambda ()
  (define (loop x n)
    (define a (create-game n))
    (if (null? a) x (loop (append x  (list (append (but-last a) (list (parse-game-str (list-last a)))))) (+ n 1)))
    )
  (loop '() 1)
  )) )


;pulls goalie stats out of the all-games
;I'm actually pretty proud of this code, although it is sloppy.
;See, it iterates the entire lst once looking for the first goalie, and if the goalie doesn't match
;it places the game into an "other" list. So the original list is entirely consumed, then it moves
;to the next goalie in the other list.
(define Goalies
  ((lambda () 
     (define (getG G lst)
       
       (if (> (string->number(caddr lst))  (string->number(cadddr lst)))
           (if (= (string->number (cadddr lst)) 0)
           (list (car G) (+ (cadr G) 1) (caddr G) (cadddr G)
                 (+ (car(cddddr G)) (string->number (caddr lst)))
                 (+ (cadr (cddddr G)) (string->number (cadddr lst)))
                 (+ (caddr (cddddr G)) 1)
                 )    
           (list (car G) (+ (cadr G) 1) (caddr G) (cadddr G)
                 (+ (car(cddddr G)) (string->number (caddr lst)))
                 (+ (cadr (cddddr G)) (string->number (cadddr lst)))
                  (caddr (cddddr G))))
          (if (= (string->number (caddr lst))  (string->number (cadddr lst))) 
              (if (= (string->number (cadddr lst)) 0)
              (list (car G) (cadr G)  (caddr G) (+ (cadddr G) 1)
                 (+ (car(cddddr G)) (string->number (caddr lst)))
                 (+ (cadr (cddddr G)) (string->number (cadddr lst)))
                 (+ (caddr (cddddr G)) 1))
             (list (car G) (cadr G)  (caddr G) (+ (cadddr G) 1)
                 (+ (car(cddddr G)) (string->number (caddr lst)))
                 (+ (cadr (cddddr G)) (string->number (cadddr lst)))
                 (caddr (cddddr G)))
              )
           (list (car G) (cadr G)  (+ (caddr G) 1) (cadddr G)
                 (+ (car(cddddr G)) (string->number (caddr lst)))
                 (+ (cadr (cddddr G)) (string->number (cadddr lst)))
                  (caddr (cddddr G)))
              ))
       )
     (define (loop lst G other)
       (if (null? lst) (cons G other)
       (if (equal? (car G) (string->number(car (cddddr (car lst)))))
           (loop (cdr lst) (getG G (car lst)) other) 
           (loop (cdr lst) G (append other (list (car lst)))))
       ))1
     (define (driver lst Gs)
         (if (null? lst) Gs
             (let ([a (loop lst (list (string->number(car (cddddr (car lst)))) 0 0 0 0 0 0) '())])
             (driver (cdr a) (append Gs (list (car a))))

       )))
                 (driver all-games '())
     ))
  )




;Plainly a variable that holds the output of all-players
(define aP (all-players))






;Returns the list object of the player in aP with the most goals.
;   Ties are broken by selecting the player with the higher # of points overall
(define most_goals
  ((lambda ()
     (define (loop lst curr)
       (if (null? lst) curr
           (if (> (string->number (get-goals (car lst))) (string->number (get-goals curr)))
               (loop (cdr lst) (car lst))
               (if (= (string->number (get-goals (car lst))) (string->number (get-goals curr)))
                   (if (> (string->number (get-points (car lst))) (string->number (get-points curr)))
                       (loop (cdr lst) (car lst))
                       (loop (cdr lst) curr))
                   (loop (cdr lst) curr)))))
               
     (loop (cdr aP) (car aP))
     )))
;Returns the list object of the player in aP with the most points.
;     Ties are broken by selecting the player with the higher # of goals.
(define most_points
  ((lambda ()
     (define (loop lst curr)
       (if (null? lst) curr
           (if (> (string->number (get-points (car lst))) (string->number (get-points curr)))
               (loop (cdr lst) (car lst))
               (if (= (string->number (get-points (car lst))) (string->number (get-points curr)))
                   (if (> (string->number (get-goals (car lst))) (string->number (get-goals curr)))
                       (loop (cdr lst) (car lst))
                       (loop (cdr lst) curr))
                   (loop (cdr lst) curr)))))
               
     (loop (cdr aP) (car aP))
     )))

;Returns the list object of the player in aP with the most assists.
;Ties are broken by whichever player has the most points.
(define most_assists
  ((lambda ()
     (define (loop lst curr)
       (if (null? lst) curr
           (if (> (string->number (get-assists (car lst))) (string->number (get-assists curr)))
               (loop (cdr lst) (car lst))
               (if (= (string->number (get-assists (car lst))) (string->number (get-assists curr)))
                   (if (> (string->number (get-points (car lst))) (string->number (get-points curr)))
                       (loop (cdr lst) (car lst))
                       (loop (cdr lst) curr))
                   (loop (cdr lst) curr)))))
               
     (loop (cdr aP) (car aP))
     )))

;Parses the Goalies object to decide which goalie (not-including Goalie ID: 0 which is unplayed games)
;                                          has the most logged wins.
;                                          Ties are broken by which goalie has, more ties.
(define most_wins
  ((lambda ()
     (define (loop lst curr)
       (if (null? lst) curr
           (if (= (caar lst) 0) (loop (cdr lst) curr)
           (if (> (cadar lst) (cadr curr))
               (loop (cdr lst) (car lst))
               (if (= (cadar lst) (cadr curr))
                   (if (> (caddar lst) (caddr curr))
                       (loop (cdr lst) (car lst))
                       (loop (cdr lst) curr))
                   (loop (cdr lst) curr))))))
               
     (loop (cdr Goalies) (car Goalies))
     )))

;This code has two different ways to run, but returns the same output.
;You either pass it a player's id number (1-12) or Lastname, Firstname and it will
;return the player object out of aP
(define (get-player x)
  (define (str x lst)
    (if (equal? x (get-fullname (car lst))) (car lst) (str x (cdr lst))))
  (define (num n lst)
    (if (null? lst)
        '()
    (if (= n 0) (car lst) (num (- n 1) (cdr lst)))))
  (if (number? x) (num (- x 1) aP)  (str x aP))
  )

  (define (update-game x)
  (define in
  (post-pure-port my-site (string->bytes/utf-8 (format  "f=sgi&id=30&d=2016-04-11 07:29:00&opp=0&gf=&ga=&gd=%20&gid=12" )) Header) )
  (begin0
  (port->string in)
  (close-input-port in)))

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

;(define games-image
;(above
;(overlay/offset
;(overlay (text "Dragons" 42 "gold")
;         (rectangle 500 80 "solid" "forestgreen"))
;0 60
;(overlay (text "Game" 16 "gold")
;         (rectangle 500 40 "solid" "black")))
;0 100
;(rectangle 500 100 "solid" "black")))


 

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
                   ;(if (and (> x-mouse 375) (< y-mouse 20))
                    ;   (set! draw_img games-image)
                       0))
           
           ))0)
 )
(big-bang '(75 . 75) (to-draw create-scene) (on-mouse change))




; TODO
; Use http://m.jdbjohnbrown.net/ as reference
; Uses arrows keys up/down to rotate through games
; create a 3rd button labeled "games" add functionality to also iterate through past games and eventual games