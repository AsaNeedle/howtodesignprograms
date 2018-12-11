;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 145-159 HtDP|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)
 ; List-of-temperatures -> Number
    ; computes the average temperature 
    (define (average alot)
      (/ (sum alot)
         (if (> (how-many alot) 0) (how-many alot) (error message))))
     
   (define message "YOU CAN'T DO THAT!")

    ; Wish
    ; List-of-temperatures -> Number 
    ; adds up the temperatures on the given list 
   (define (sum alot)
  (cond
    [(empty? alot) 0]
    [else (+ (first alot) (sum (rest alot)))]))

    ; Wish
    ; List-of-temperatures -> Number 
    ; counts the temperatures on the given list
    (check-expect (how-many '()) 0)
    (check-expect (how-many (cons "a" '())) 1)
    (check-expect
      (how-many (cons "b" (cons "a" '()))) 2)
    

    (define (how-many alot)
      (cond
        [(empty? alot) 0]
        [(cons? alot) (+ (how-many (rest alot)) 1)]))
    ; Exercise 145 (DONE)
    ; NEList-of-tempertures -> Boolean
    ; consumes a NEList-of-tempertures nel and produces #true if the list is in strict descending order,
    ; otherwise #false 
   
    (check-expect (sorted>? (cons 5 '())) #true)
    (check-expect (sorted>? (cons 4 (cons 4 '()))) #false)
    (check-expect (sorted>? (cons 1 (cons 0 '()))) #true)
    (check-expect (sorted>? (cons 6 (cons 4 (cons 3 '())))) #true)
    (check-expect (sorted>? (cons 1 (cons 10 '()))) #false)
    (define (sorted>? nel)
      (cond
      [(empty? (rest nel)) #true ]
      [(<=  (first nel) (first (rest nel))) #false]
      [else (sorted>? (rest nel))]))

    ; Exercise 147
    ; NEList-of-Booleans is one of
    ; - (cons Boolean '())
    ; - (cons Boolean NEList-of-Booleans)
    
    ; NEList-of-Booleans -> Boolean
    ; (all-true nel)
    ; It determines whether any Boolean in the list is #false, and if so, produces #false.
    ; Otherwise, #true

  (check-expect (all-true (cons #false (cons #false '()))) #false)
  (check-expect (all-true (cons #true (cons #true (cons #true '())))) #true)

    (define (all-true nel)
      (cond
        [(empty? (rest nel)) #true]
        [(equal? (first nel) #false) #false]
        [else (all-true (rest nel))]))

    ; NEList-of-Booleans -> Booleans
    ; (one-true nel)
    ; Determines if any Boolean in the list is #true, and is so, produces #true, otherwise #false

    (check-expect (one-true (cons #true (cons #false (cons #false '())))) #true)
    (check-expect (one-true (cons #true (cons #true (cons #true '())))) #true)
    (check-expect (one-true (cons #false (cons #false '()))) #false)

    (define (one-true nel)
      (cond
        [(empty? (rest nel)) #false]
        [(equal? (first nel) #true) #true]
        [else (one-true (rest nel))]))
    
    
 

    (define (to-zero x) (if (= (sub1 x) 0) 0 (to-zero (sub1 x))))
    (define (to-infinity x) (if (= (sub1 x) 0) 0 (to-infinity (add1 x))))
; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers
    
    ; N, String -> List-of-strings
    ; creates a list of N copies of S
(check-expect (copier 0 "hello") '())
(check-expect (copier 2 "hello")
              (cons "hello" (cons "hello" '())))
(check-expect (copier 0 #true) '())
(check-expect (copier 3 #false) (cons #false (cons #false (cons #false '()))))
 
(define (copier n s)
  (cond
    [(zero? n) '()]
    [else (cons s (copier (sub1 n) s))]))

; Exercise 150
; N -> Number
; computes (+ n pi) without using +

(check-within (add-to-pi 3) (+ pi 3) 0.001)
(define (add-to-pi n)
  (cond
    ((zero? n) pi)
    (else (add1 (add-to-pi (sub1 n))))))
; Exercise 151
; N, Number -> Number
; consumes a natural number n and multiplies it by a number x without using *
(check-expect (multiply 0.5 10) 5)
(check-expect (multiply 3 7) 21)

(define (multiply x n)
  (cond
    [(zero? n) 0]
    [else (+ x (multiply x (sub1 n)))]))

; N, Number -> Number
;consumes a natural number n and adds it to Number x without using +
(check-expect (add 0.5 10) 10.5)
(check-expect (add 1 2) 3)
 
(define (add x n)
  (cond
    ((zero? n) x)
    (else (add1 (add x (sub1 n))))))
  

; EXERCISE 152

; (define (col n img) img)
; Number, Image -> Image
;produces column of n number of Image img, stacked on top of each other
(define (col n img)
  (cond
    [(zero? (sub1 n)) img]
    [else (beside img (col (sub1 n) img))]))

; (define (row n img) img)
; Number, Image -> Image
; produces row of n number of Image img, laid side by side
(define (row n img)
  (cond
    [(zero? (sub1 n)) img]
    [else (above img (row (sub1 n) img))]))

(check-expect (row 2 (circle 10 "solid" "red")) (above (circle 10  "solid" "red" )(circle 10  "solid" "red")))

; EXERCISE 153
; (define (grid n1 n2 img) img)
; Number, Number, Image -> Image
; produces grid of Image img, n1 wide and n2 high

(define (grid n1 n2 img) (col n1 (row n2 img)))


(define lecture-hall (overlay (grid 8 18 (square 10 "outline" "black")) (empty-scene 80 180)))

; Wish: add-balloons
; List-of-Posn-lh -> img
; consumes a List-of-Posn-lh and produces an img with a red dot at each posn in the list
; on a grid of 8 by 18 squares, each length 10
; A Posn-lh is a structure (make-posn x y) where
; - (<= 0 x 80),
; - (modulo x 10) = 0,
; - (modulo y 10 = 0, and
; - (<= 0 y 180)

; A List-of-Posn-lh is one of
; - '()
; - (cons Posn-lh List-of-Posn-lh)
(check-expect (add-balloons '()) lecture-hall)
(check-expect (add-balloons (cons (make-posn 5 4) '())) (place-image (circle 2 "solid" "red") 5 4 lecture-hall))

(define (add-balloons plh)
  (cond
    [(empty? plh) lecture-hall]
    [else (place-image
           (circle 2 "solid" "red")
           (posn-x (first plh))
           (posn-y (first plh))
           (add-balloons (rest plh)))]))

; NEW SECTION 9.4 Russian Dolls
    (define-struct layer [color doll])
    ; An RD (short for Russian doll) is one of: 
    ; – String 
    ; – (make-layer String RD)
    ; ex. (make-layer "yellow" (make-layer "green" "red"))
    ; "yellow"

; RD -> Number
; how many dolls are part of an-rd
    (check-expect (depth "yellow") 1)
    (check-expect (depth (make-layer "pink" (make-layer "black" "white"))) 3)
    
    (define (depth an-rd)
      (cond
        [(string? an-rd) 1]
        [else
         (+ (depth (layer-doll an-rd)) 1)]))
    
; Exercise 154
; Wish: colors
; RD -> String
; consumes an RD and produces string of all colors, separated by comma and space
(check-expect (colors "yellow") "yellow")
(check-expect (colors (make-layer "pink" (make-layer "black" "white"))) "pink, black, white")
(check-expect (colors (make-layer "yellow" (make-layer "green" "red"))) "yellow, green, red")
(define (colors an-rd)
  (cond
    [(string? an-rd) an-rd]
    [else (string-append
           (layer-color an-rd) ", "
           (colors (layer-doll an-rd)))]))
; Exercise 155
; RD -> String
; consumes RD and produces color of the innermost doll
(check-expect (inner "yellow") "yellow")
(check-expect (inner (make-layer "pink" (make-layer "black" "white"))) "white")
(check-expect (inner (make-layer "yellow" (make-layer "green" "red"))) "red")

(define (inner an-rd)
  (cond
    [(string? an-rd) an-rd]
    [else (inner (layer-doll an-rd))]))

; NEW SECTION 9.5
; A List-of-shots is one of: 
; – '()
; – (cons Shot List-of-shots)
; interpretation the collection of shots fired
; A Shot is a Number.
; interpretation represents the shot's y-coordinate
; A ShotWorld is List-of-numbers. 
; interpretation each number on such a list
; represents the y-coordinate of a shot
    (define HEIGHT 80) ; distances in terms of pixels 
    (define WIDTH 100)
    (define XSHOTS (/ WIDTH 2))
     
    ; graphical constants 
    (define BACKGROUND (empty-scene WIDTH HEIGHT))
    (define SHOT (triangle 3 "solid" "red"))

; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tock1]
    [on-key keyh]
    [to-draw to-image]))
 
; ShotWorld -> ShotWorld 
; moves each shot up by one pixel 
(define (tock w)
  (cond
    [(empty? w) '()]
    [else (cons (sub1 (first w)) (tock (rest w)))]))

; ShotWorld -> ShotWorld
; moves each shot up by one pixel and deletes them once they are above the canvas
 
; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world if the space bar is hit 
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))
 
; ShotWorld -> Image 
; adds each shot y on w at (XSHOTS,y} to BACKGROUND
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))


; EXERCISE 158
; ShotWorld -> ShotWorld
; moves each shot up by one pixel and deletes them once they are above the canvas
 (define (tock1 w)
  (cond
    [(empty? w) '()]
    [(< (first w) 0) (tock (rest w))]
    [else (cons (sub1 (first w)) (tock (rest w)))]))


; EXERCISE 159
(define-struct pair [n lob])
; A Pair is a structure (make-pair N List-of-posns)
; A List-of-posns is one of: 
; – '()
; – (cons Posn List-of-posns)
; interpretation (make-pair n lob) means n balloons 
; must yet be thrown and added to lob

; BalloonWorld -> BalloonWorld
; A BalloonWorld is a Pair
 (define (riot bw)
   (big-bang bw
     [to-draw render-b]
     [on-tick tock-b 1]))
 ; Wish: render-b
 ; BalloonWorld -> Image
 ; consumes a (make-pair n lob) ws and produces a grid of 10x10 black squares,
 ; and places a red dot at each posnlh of lob

 (check-expect (render-b (make-pair 5 (cons (make-posn 20 30) '())))
               (place-image (circle 2 "solid" "red")
                            20 30
                            lecture-hall))
 (check-expect (render-b (make-pair 5 (cons (make-posn 60 60) (cons (make-posn 20 30) '()))))
               (place-image (circle 2 "solid" "red")
                            60 60
                            (place-image (circle 2 "solid" "red")
                            20 30
                            lecture-hall)))
 
 (check-expect (render-b (make-pair 2 '())) lecture-hall)
                            
                            
 (define (render-b bw)
  (cond
    [(empty? (pair-lob bw)) lecture-hall]
    [else (place-image (circle 2 "solid" "red")
          (posn-x (first (pair-lob bw)))
          (posn-y (first (pair-lob bw)))
          (render-b (make-pair (pair-n bw)
                               (rest (pair-lob bw)))))]))
 

 ; Wish tock-b
 ; BalloonWorld -> BalloonWorld
 ; Consumes a (make-pair n lob) ws and on-tick subtracts 1 from n
 ; and adds a random Posn to lob
(define (tock-b bw)
  (cond
    [(zero? (pair-n bw)) bw]
    [else (make-pair
           (sub1 (pair-n bw))
           (cons (random-seat 8 18) (pair-lob bw)))]))
; Number, Number -> Posn
; (random-seat x y) 
; consumes two Numbers, and produces a Posn (make-posn a b) where
; - (<= 0 a x),
; - (modulo a 10) = 0,
; - (<= 0 b y),
; - (modulo b 10 = 0

(define (random-seat x y) (make-posn (* 10 (random (+ 1 x))) (* 10 (random (+ 1 y)))))

 ; NEW SECTION 9.6 a note on lists and sets
; List-of-string String -> N
; determines how often s occurs in los
(check-expect (count '() "hello") 0)
(check-expect (count (cons "y" (cons "y" '())) "y") 2)
(check-expect (count (cons "no" (cons "yes" '())) "yes") 1)

(define (count los s)
  (cond
    [(empty? los) 0]
    [(equal? (first los) s) (+ 1 (count (rest los) s))]
    [else (count (rest los) s)]))

 
 
     
   
 
 


      
    
