#lang racket
;tools.rkt is a collection of frequently used pieces of code found throughout
;"WebsiteCode" "Images" and "MainCode"

(provide remove-last)
(provide string-find)
(provide instr)
(provide apply-generic)
(provide car-str)
(provide cdr-str)
(provide list-last)
(provide but-last)
(provide get-num-list)
(provide edit-num-list)
(provide get-team-name)

;Removes the last element from a list
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

;gets the first character of a string
(define (car-str str) (if (equal? str "") "" (substring str 0 1)))
;gets all but the first character of a string
(define (cdr-str str) (if (equal? str "") "" (substring str 1 (string-length str))))

;returns the last element of a list
(define (list-last lst) (if (null? lst) '() (car (reverse lst))))
;removes the last element of a list
(define (but-last lst) (if (null? lst) '()(reverse (cdr (reverse lst)))))

;returns the xth element of a list
(define (get-num-list x lst)
  (define (loop n x lst)
    (if (null? lst) '()
        (if (= n x) (car lst)
            (loop (+ n 1) x (cdr lst)))))
  (loop 1 x lst)
  )

;replaces the xth element of a list lst with the value: edit
(define (edit-num-list x lst edit)
  (define (loop n x lst new edit)
    (if (null? lst) new
         (if (= n x)  (loop (+ n 1) x (cdr lst) (append new (list edit)) edit)
             (loop (+ n 1) x (cdr lst) (append new (list (car lst))) edit))))
  (loop 1 x lst '() edit)
  )

; To save the amount of data being communicated, the team names are not saved on a server, and are instead
; represented by a number. Here that number is translated into a name.
(define (get-team-name x)
(cond  [(= x 1) "Dragons"]
[(= x 2) "Chiefs"]
[(= x 3) "Toews"]
[(= x 4) "Purple Haze"]
[(= x 5) "Tanner Factory"]
[(= x 6) "Sudden Death"]
[(= x 7) "Dark Knights"]
[(= x 8) "Bullets"]
[(= x 9) "Crew"]
[(= x 10) "Hackers"]
[(= x 11) "Hackers"]
[(= x 12) "Misfits"]
[else "N/A"]))


