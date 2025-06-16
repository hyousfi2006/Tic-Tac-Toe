;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hogan-yousfi-tictactoe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) ; you might want this

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the SA will mess with the SIZE constant, be sure it is used
;; throughout your program where needed
(define SIZE 300) ;; can range from [300,900]

(define MTS (empty-scene SIZE SIZE "black"))

(define BANNER (rectangle (/ SIZE 1.15) (/ SIZE 4) 200 "white"))

;; may use the following "pen" when you draw the vertical and horizontal lines on the board
(define PEN (make-pen "White"
                      (round (/ SIZE 40)) "solid" "round" "round"))

(define X 1)  ;; Val
(define O 0)  ;; Val
(define B 2)  ;; for Blank square
(define D -1) ;; for draw
(define E -2) ;; for end game

(define FONT-SIZE (/ SIZE 3))

(define X-COLOR "purple")
(define O-COLOR "green")

(define X-PEN (make-pen X-COLOR
                        (round (/ SIZE 45)) "solid" "round" "round"))
(define O-PEN (make-pen O-COLOR
                        (round (/ SIZE 45)) "solid" "round" "round"))
;; empty board
(define listOfBoard (list 2 2 2
                          2 2 2
                          2 2 2))
;; no one wins
(define LOB1 (list 2 1 0
                   2 0 0
                   1 2 2))

;; X diagonal top left
(define LOB2 (list 1 1 0
                   0 1 0
                   1 0 1))

;; X wins top horizontal & middle vertical
(define LOB3 (list 1 1 1
                   0 1 0
                   0 1 0))

;; Draw
(define LOB4 (list 1 0 1
                   0 1 0
                   0 1 0))

(define BOARD
  (place-image (line (* 1.60 (/ SIZE 2)) 0 PEN) (/ SIZE 2) (* 2 (/ SIZE 3))
               (place-image (line (* 1.60 (/ SIZE 2)) 0 PEN) (/ SIZE 2) (/ SIZE 3)
                            (place-image (line 0 (* 1.60 (/ SIZE 2)) PEN) (* 2 (/ SIZE 3)) (/ SIZE 2)
                                         (place-image (line 0 (* 1.60 (/ SIZE 2)) PEN) (/ SIZE 3) (/ SIZE 2)
                                                      (place-image (text "Difficulty:" (/ SIZE 15) "white") (/ SIZE 3) (+ (/ SIZE 25) 0) MTS))))))

(define X-IMAGE (place-image (line (/ SIZE 6) (/ SIZE 6) X-PEN) (/ SIZE 12) (/ SIZE 12) (line (* -1 (/ SIZE 6)) (/ SIZE 6) X-PEN)))

(define O-IMAGE (ellipse (/ SIZE 6) (/ SIZE 6) "outline" O-PEN))


;; world-state -> player
;; takes in a world-state and checks for all three types of win (across, down, diagonal)
;; (define (test-win ws) 0)
(define (test-win ws)
  (if (or (horiz (world-state-listOfBoard ws) X)
          (ver (world-state-listOfBoard ws) X)
          (diag (world-state-listOfBoard ws) X))
      X
      (if (or (horiz (world-state-listOfBoard ws) O)
              (ver (world-state-listOfBoard ws) O)
              (diag (world-state-listOfBoard ws) O))
          O
          (if (test-full ws)
              D
              B))))

(check-expect (test-win WS1) B)
(check-expect (test-win WS3) X)
(check-expect (test-win WS5) D)

;; world-state -> boolean
;; checks if the board has a draw
(define (test-full ws)
  (local [(define (test lob acc)
            (cond [(empty? lob) true]
                  [else
                   (if (= (first lob) B)
                       false
                       (test (rest lob) (+ 1 acc)))]))]
    (test (world-state-listOfBoard ws) 0)))

(check-expect (test-full WS1) false)
(check-expect (test-full WS2) false)
(check-expect (test-full WS3) true)

;; listOfBoard Natural -> listOfBoard
;; takes a list and an index and returns the next three values (horizontal on a tic tac toe board)
(define (make-horizontal lob i)
  (local [(define (build lob z acc)
            (cond [(empty? lob) empty]
                  [else
                   (if (and (>= z acc) (>= acc i))
                       (cons (first lob) (limit acc z lob))
                       (limit acc z lob))]))

          (define (limit acc z lob)
            (if (<= z acc)
                empty
                (build (rest lob) z (+ 1 acc))))]      
    (build lob (+ i 2) 0)))

(check-expect (make-horizontal LOB1 0) (list 2 1 0))
(check-expect (make-horizontal LOB1 3) (list 2 0 0))
(check-expect (make-horizontal LOB1 6) (list 1 2 2))


;; listOfBoard natural -> listOfBoard
;; (define make-vertical)
(define (make-vertical lob i)
  (local [(define (build lob z acc)
            (cond [(empty? lob) empty]
                  [else
                   (if (and (and (>= z acc) (>= acc i)) (or (or (= acc (- z 3))(= acc (- z 6))) (= acc (- z 0))))
                       (cons (first lob) (limit acc z lob))
                       (limit acc z lob))]))

          (define (limit acc z lob)
            (if (<= z acc)
                empty
                (build (rest lob) z (+ 1 acc))))]      
    (build lob (+ i 6) 0)))

(check-expect (make-vertical LOB1 0) (list 2 2 1))
(check-expect (make-vertical LOB1 1) (list 1 0 2))
(check-expect (make-vertical LOB1 2) (list 0 0 2))

;; listOfBoard natural natural -> listOfBoard
;; (define make-diagonal)
(define (make-diagonal lob i dir)
  (local [(define (build lob z acc)
            (cond [(empty? lob) empty]
                  [else
                   (if (and (even? z) (= (+ i (* dir acc)) z) (< acc 3))
                       (cons (first lob) (build (rest lob) (+ 1 z) (+ 1 acc)))
                       (build (rest lob) (+ 1 z) acc))]))]      
    (build lob 0 0)))

(check-expect (make-diagonal LOB1 0 4) (list 2 0 2)) ;; top left
(check-expect (make-diagonal LOB1 2 2) (list 0 0 1)) ;; top right

;; (define check-horizontal lob p i)
;; listOfBoard player natural -> boolean
;; takes in a listOfBoard, a player, and an index and checks if there is a horizontal win for said player
(define (check-horizontal lob p i)
  (local [(define (search lob player i acc)
            (cond [(empty? lob) false]
                  [else
                   (if (and (and (or (or (= i 2) (= i 5)) (= i 8)) (= (first lob) player)) (= 2 acc))
                       true
                       (if (= (first lob) player)
                           (search (rest lob) player (+ 1 i) (+ 1 acc))
                           (search (rest lob) player (+ 1 i) acc)))]))]
    (search lob p i 0)))

;; listOfBoard Player -> Boolean
;; Calls check-horizontal for all possible wins
(define (horiz lob p)
  (or (check-horizontal (make-horizontal lob 0) p 0)
      (or (check-horizontal (make-horizontal lob 3) p 3)
          (check-horizontal (make-horizontal lob 6) p 6))))

(check-expect (horiz LOB1 X) false)
(check-expect (horiz LOB3 X) true)
(check-expect (horiz LOB3 O) false)


;; (define check-vertical)
;; listOfBoard Player Natural -> boolean
;; takes in a listOfBoard, a player, and an index and checks if there is a vertical win for said player
(define (check-vertical lob p i)
  (local [(define (search lob player i acc)
            (cond [(empty? lob) false]
                  [else
                   (if (and (and (or (or (= i 6) (= i 7)) (= i 8)) (= (first lob) player)) (= 2 acc))
                       true
                       (if (= (first lob) player)
                           (search (rest lob) player (+ 3 i) (+ 1 acc))
                           (search (rest lob) player (+ 3 i) acc)))]))]
    (search lob p i 0)))

;; listOfBoard Player -> Boolean
;; Calls check-vertical for all possible wins
(define (ver lob p)
  (or (check-vertical (make-vertical lob 0) p 0)
      (or (check-vertical (make-vertical lob 1) p 1)
          (check-vertical (make-vertical lob 2) p 2))))

(check-expect (ver LOB1 X) false)
(check-expect (ver LOB3 X) true)
(check-expect (ver LOB3 O) false)

;; (define check-diagonal)
;; ListOfBoard Player Natural -> boolean
;; takes in a listOfBoard, a player, and an index and checks if there is a diagonal win for said player
(define (check-diagonal lob p i)
  (local [(define (search lob player i acc)
            (cond [(empty? lob) false]
                  [else
                   (if (and (and (or (or (= i 6) (= i 7)) (= i 8)) (= (first lob) player)) (= 2 acc))
                       true
                       (if (= (first lob) player)
                           (search (rest lob) player (+ 3 i) (+ 1 acc))
                           (search (rest lob) player (+ 3 i) acc)))]))]
    (search lob p i 0)))

;; listOfBoard Player -> Boolean
;; Calls check-diagonal for all possible wins
(define (diag lob p)
  (or (check-diagonal (make-diagonal lob 0 4) p 0)
      (check-diagonal (make-diagonal lob 2 2) p 2)))

(check-expect (diag LOB1 X) false)
(check-expect (diag LOB2 X) true)


;; a world-state is a (make-world-state listOfBoard Natural Natural)
(define-struct world-state (listOfBoard turn diff))
;; interp:
;;  - listOfBoard is a list of the current state of the board
;;  - turn is the current player
;;  - diff i the current game difficulty

(define WS1 (make-world-state listOfBoard 1 0))
(define WS2 (make-world-state LOB1 1 0))
(define WS3 (make-world-state LOB2 1 0))
(define WS4 (make-world-state LOB2 -1 0))
(define WS5 (make-world-state LOB4 -1 0))

;;world-state -> world-state
;; takes in a world-state and returns a world-state, calls our functions render and win
;; (define (to-rend ws) ws)
(define (to-render ws)
  (render-win ws (render ws)))

;; world-state -> image
;; draw the current board
(define (render ws)
  (local
    [(define (place lob x y)
       (cond [(empty? lob) (place-image (text (number->string (world-state-diff ws)) (/ SIZE 15) "white") (/ SIZE 2) (/ SIZE 25) BOARD)]
             [(= (first lob) B)
              (place (rest lob) (test-x x) (test-y x y))]
             [(= (first lob) X)
              (place-image X-IMAGE (* (/ SIZE 6) (+ 1 x)) (* (/ SIZE 6) (+ 1 y))
                           (place (rest lob) (test-x x) (test-y x y)))]
             [else
              (place-image O-IMAGE (* (/ SIZE 6) (+ 1 x)) (* (/ SIZE 6) (+ 1 y))
                           (place (rest lob) (test-x x) (test-y x y)))]
             ))
     (define (test-y x y)
       (if (> x 3)
           (+ 2 y)
           y))
     (define (test-x x)
       (if (= x 4)
           0
           (+ x 2)))
     ]
    (place (world-state-listOfBoard ws) 0 0)
    ))

; (check-expect (render START) BOARD)
#; (check-expect (render WS2) (place-image X-IMAGE (* 3 (/ SIZE 6)) (/ SIZE 6)
                                           (place-image O-IMAGE (* 5 (/ SIZE 6)) (/ SIZE 6)
                                                        (place-image O-IMAGE (/ (* 3 SIZE) 6) (* 3 (/ SIZE 6))
                                                                     (place-image O-IMAGE (* 5 (/ SIZE 6)) (* 3 (/ SIZE 6))
                                                                                  (place-image X-IMAGE (/ SIZE 6) (* 5 (/ SIZE 6)) BOARD))))))

;; world-state -> image
;; creates an image based on who has currently won
(define (render-win ws currBoard)
  (cond [(= (test-win ws) B)
         currBoard]
        [(= (test-win ws) X)
         (place-image
          (text "X Wins" (/ SIZE 4) X-COLOR) (/ SIZE 2) (/ SIZE 2)
          (place-image BANNER (/ SIZE 2) (/ SIZE 2) currBoard))]
        [(= (test-win ws) O) 
         (place-image
          (text "O Wins" (/ SIZE 4) O-COLOR) (/ SIZE 2) (/ SIZE 2)
          (place-image BANNER (/ SIZE 2) (/ SIZE 2) currBoard))]
        [(= (test-win ws) D)
         (place-image
          (text "Draw" (/ SIZE 4) "red") (/ SIZE 2) (/ SIZE 2)
          (place-image BANNER (/ SIZE 2) (/ SIZE 2) currBoard))]))

;; ListOfBoard Player -> ListOfNumbers
;; takes in the current state of listOfBoard and Player and returns the index of all the spots taken by a certain player on the board.
(define (list-index lob player)
  (local [(define (search lob acc player)
            (cond [(empty? lob) empty]
                  [else
                   (if (= (first lob) player)
                       (cons acc (search (rest lob) (+ 1 acc) player))
                       (search (rest lob) (+ 1 acc) player))]))]
    (search lob 0 player)))

(check-expect (list-index LOB1 0) (list 2 4 5))

(define START (make-world-state (list 2 2 2 2 2 2 2 2 2) X 0))

(define (main world-state)
  (big-bang world-state
    (to-draw to-render)
    (on-tick tick)
    (on-mouse player-x-turn)
    (on-key keyhandler)
    ))

;;world-state -> world-state
;; takes in a world-state and returns a world-state, calls our functions random-O and test-win
;; (define (tick ws) ws)
(define (tick ws)
  (if (= (world-state-turn ws) O)
      (cond [(=(world-state-diff ws) 0)
             (random-O ws)]
            [(or (= (world-state-diff ws) 1)
                 (= (world-state-diff ws) 2))
             (win-move ws)]
            [(= (test-win ws) B)
             (minimax ws)]
            [else
             ws]) ;MINIMAX
      (make-world-state (world-state-listOfBoard ws) (if (or (= (test-win ws) X)
                                                             (= (test-win ws) O)
                                                             (= (test-win ws) D)
                                                             (test-full ws))
                                                         E
                                                         (world-state-turn ws))
                        (world-state-diff ws))))

; World-State -> World-State
;; takes in a World-State and places O on a random unoccupied spot on the board.
(define (random-O ws)
  (local
    [(define (check-spot lob i acc)
       (cond [(empty? lob) (check-spot (world-state-listOfBoard ws) (random 9) 0)]
             [else
              (if (and (= i acc) (= (first lob) B))
                  (place (world-state-listOfBoard ws) i 0)
                  (check-spot (rest lob) i (+ 1 acc)))])) 
     (define (place lob i acc)
       (cond [(empty? lob) empty]
             [else
              (if (= i acc)
                  (cons O (place (rest lob) i (+ 1 acc)))
                  (cons (first lob) (place (rest lob) i (+ 1 acc))))]))]
    (make-world-state  (if (or (= (test-win ws) X)
                               (= (test-win ws) O)
                               (= (test-win ws) D)
                               (test-full ws)) 
                           (world-state-listOfBoard ws)
                           (check-spot (world-state-listOfBoard ws) (random 9) 0))
                       (if (or (= (test-win ws) X)
                               (= (test-win ws) O)
                               (= (test-win ws) D)
                               (test-full ws)) 
                           E
                           X)(world-state-diff ws))))

;; (check-expect (random-O WS1) listOfBoard)
;; (check-expect (random-O WS2) LOB1)

;; World-State, Mouse Event, natural, natural -> World-state 
;; to determine where player x wants to play
(define (player-x-turn ws mouse-x mouse-y me)
  (cond
    [(and (mouse=? me "button-down")
          (= (world-state-turn ws) X)
          (spot-taken ws mouse-x mouse-y))
     (make-world-state
      (make-new-lob-x ws mouse-x mouse-y X)
      (if (or (= (test-win ws) X)
              (= (test-win ws) O)
              (= (test-win ws) D)
              (test-full ws)) 
          E
          O)(world-state-diff ws))]
    [else
     ws]
    )) 

(check-expect (player-x-turn WS1 (/ SIZE 6) (/ SIZE 6) "button-down") (make-world-state (list 1 2 2 2 2 2 2 2 2) 0 (world-state-diff WS1)))
(check-expect (player-x-turn WS2 (/ SIZE 6) (/ SIZE 6) "button-down") (make-world-state (list 1 1 0
                                                                                              2 0 0
                                                                                              1 2 2) 0 (world-state-diff WS2)))

;; world-state natural natural player -> listOfBoard
;; creates a new listOfBoard based off of most recent play by player
(define (make-new-lob-x ws x y p)
  (local
    [(define (location x y)
       (cond [(< y (/ SIZE 3))
              (cond [(< x (/ SIZE 3)) 0]
                    [(< x (* 2 (/ SIZE 3))) 1]
                    [else 2])]
             [(< y (* 2 (/ SIZE 3)))
              (cond [(< x (/ SIZE 3)) 3]
                    [(< x (* 2 (/ SIZE 3))) 4]
                    [else 5])]
             [else
              (cond [(< x (/ SIZE 3)) 6]
                    [(< x (* 2 (/ SIZE 3))) 7]
                    [else 8])]))
     (define (place lob acc i)
       (cond [(empty? lob) empty]
             [else
              (if (= acc i)
                  (cons p (place (rest lob) (+ 1 acc) i))
                  (cons (first lob) (place (rest lob) (+ 1 acc) i)))]))]
    (place (world-state-listOfBoard ws) 0 (location x y))))


(check-expect (make-new-lob-x WS1 (/ SIZE 6) (/ SIZE 6) X) (list 1 2 2 2 2 2 2 2 2))
(check-expect (make-new-lob-x WS1 (* 5 (/ SIZE 6)) (/ SIZE 6) X) (list 2 2 1 2 2 2 2 2 2))
(check-expect (make-new-lob-x WS1 (* 3 (/ SIZE 6)) (* 3 (/ SIZE 6)) X) (list 2 2 2 2 1 2 2 2 2))
(check-expect (make-new-lob-x WS1 (* 1 (/ SIZE 6)) (* 5 (/ SIZE 6)) X) (list 2 2 2 2 2 2 1 2 2))


;; world-state, natural, natural -> boolean
;; to determine if the spot clicked on by the player is available
(define (spot-taken ws x y)
  (cond [(< y (/ SIZE 3))
         (cond [(< x (/ SIZE 3))
                (= (first(world-state-listOfBoard ws)) B)]
               [(< x (* 2 (/ SIZE 3)))
                (= (first(rest(world-state-listOfBoard ws))) B)]
               [else
                (= (first(rest (rest (world-state-listOfBoard ws)))) B)])]
        [(< y (* 2 (/ SIZE 3)))
         (cond [(< x (/ SIZE 3))
                (= (first(make-horizontal (world-state-listOfBoard ws) 3)) B)]
               [(< x (* 2 (/ SIZE 3)))
                (= (first(rest (make-horizontal (world-state-listOfBoard ws) 3))) B)]
               [else
                (= (first(rest (rest (make-horizontal (world-state-listOfBoard ws) 3)))) B)])]
        [else
         (cond [(< x (/ SIZE 3))
                (= (first(make-horizontal (world-state-listOfBoard ws) 6)) B)]
               [(< x (* 2 (/ SIZE 3)))
                (= (first(rest (make-horizontal (world-state-listOfBoard ws) 6))) B)]
               [else
                (= (first(rest (rest (make-horizontal (world-state-listOfBoard ws) 6)))) B)])]))

(check-expect (spot-taken WS1 (/ SIZE 2) (/ SIZE 5)) true)
(check-expect (spot-taken WS2 (* 5 (/ SIZE 6)) (/ SIZE 2)) false)

;;world-state key-handler -> world-state
;; takes in the current world state and the value of the key pressed and returns a world state
;; with a difficulty of the key pressed. 
(define (keyhandler ws key)
  (cond
    [(key=? key "0")
     (make-world-state (world-state-listOfBoard ws) (world-state-turn ws) 0)]
    [(key=? key "1")
     (make-world-state (world-state-listOfBoard ws) (world-state-turn ws) 1)]
    [(key=? key "2")
     (make-world-state (world-state-listOfBoard ws) (world-state-turn ws) 2)]
    [(key=? key "3")
     (make-world-state (world-state-listOfBoard ws) (world-state-turn ws) 3)]
    [(key=? key "4")
     (make-world-state (world-state-listOfBoard ws) (world-state-turn ws) 4)]
    [(key=? key "5")
     (make-world-state (world-state-listOfBoard ws) (world-state-turn ws) 5)]
    [(key=? key "6")
     (make-world-state (world-state-listOfBoard ws) (world-state-turn ws) 6)]
    [(key=? key "7")
     (make-world-state (world-state-listOfBoard ws) (world-state-turn ws) 7)]
    [(key=? key "8")
     (make-world-state (world-state-listOfBoard ws) (world-state-turn ws) 8)]
    [(key=? key "9")
     (make-world-state (world-state-listOfBoard ws) (world-state-turn ws) 9)]
    [else
     ws]))
         

;; world-state -> world-state
;; checks if there is an immediate winning move and places it if so, if not checks to see if it should check for a block or go to random based on difficulty
(define (win-move ws)
  (local [(define (check lob acc)
            (cond [(= acc 8)
                   (if (= (world-state-diff ws) 2)
                       (block-move ws)
                       (random-O ws))
                   ]
                  [else
                   (if (= (test-win
                           (make-world-state (place (world-state-listOfBoard ws) 0 acc) (world-state-turn ws) (world-state-diff ws)))
                          O )
                       (make-world-state (new-lob acc 0 (world-state-listOfBoard ws)) X (world-state-diff ws))
                       (check lob (+ 1 acc)))]))

          (define (place lob acc i)
            (cond [(empty? lob) empty]
                  [else
                   (if (and (= (first lob) B)
                            (= acc i))
                       (cons O (place (rest lob) (+ 1 acc) i))
                       (cons (first lob) (place (rest lob) (+ 1 acc) i)))]))
          
          (define (new-lob i acc lob)
            (cond [(empty? lob) empty]
                  [else
                   (if (= i acc)
                       (cons O (new-lob i (+ 1 acc) (rest lob)))
                       (cons (first lob) (new-lob i (+ 1 acc) (rest lob))))]))]
    
    (check (world-state-listOfBoard ws) 0)))

(check-expect (win-move (make-world-state (list 0 1 0
                                                2 1 1
                                                0 0 1) O 1)) (make-world-state (list 0 1 0
                                                                                     0 1 1
                                                                                     0 0 1) X 1))
;; world-state -> world-state
;; checks if there is an immediate winning move for X and blocks it if so, if not goes to random
(define (block-move ws)
  (local [(define (place lob acc i)
            (cond [(empty? lob) empty]
                  [else
                   (if (and (= (first lob) B)
                            (= acc i))
                       (cons X (place (rest lob) (+ 1 acc) i))
                       (cons (first lob) (place (rest lob) (+ 1 acc) i)))]))
          (define (check lob acc)
            (cond [(= acc 9)
                   (random-O ws)]
                  [else
                   (if (= (test-win (make-world-state (place (world-state-listOfBoard ws) 0 acc) (world-state-turn ws) (world-state-diff ws))) X)
                       (make-world-state (new-lob acc 0 (world-state-listOfBoard ws)) X (world-state-diff ws))
                       (check lob (+ 1 acc)))]))
          (define (new-lob i acc lob)
            (cond [(empty? lob) empty]
                  [else
                   (if (= i acc)
                       (cons O (new-lob i (+ 1 acc) (rest lob)))
                       (cons (first lob) (new-lob i (+ 1 acc) (rest lob))))]))]
    (check (world-state-listOfBoard ws) 0)))


(check-expect (block-move (make-world-state (list 1 0 1
                                                  2 0 1
                                                  1 1 0) O 2)) (make-world-state (list 1 0 1
                                                                                       0 0 1
                                                                                       1 1 0) X 2))
;; world-state -> world-state
;; determines the next best move to make

(define (minimax ws)
  (local
    [(define (search ws depth)
       (cond [(or (= X (test-win ws))
                  (= O (test-win ws))
                  (= D (test-win ws))
                  (= depth 1))
              (utility ws)]
             [else
              (if (= X (world-state-turn ws))
                  (apply min (map
                              (λ(ws) (search ws (- depth 1)))
                              (gen-nextmoves (next-moves (world-state-listOfBoard ws) 0) ws))) ;; gen list function
                  
                  (apply max (map
                              (λ(ws) (search ws (- depth 1)))
                              (gen-nextmoves (next-moves (world-state-listOfBoard ws) 0) ws))))]))
     ;; check to make sure the turn cannot be anything other than x or o
                         
     ;; makes a list of empty spaces in board
     (define (next-moves lob acc)
       (cond [(empty? lob) empty] 
             [else
              (if (= B (first lob))
                  (cons acc (next-moves (rest lob) (+ 1 acc)))
                  (next-moves (rest lob) (+ 1 acc)))])) 
     
     (define (utility ws) 
       (if (= (test-win ws) X) 
           -1
           (if (= (test-win ws) O)
               1
               0)))

     (define (new-lob i acc lob ws)
       (cond [(empty? lob) empty]
             [else
              (if (= i acc)
                  (cons (world-state-turn ws) (new-lob i (+ 1 acc) (rest lob) ws)) ;; should be X or O depending on turn 
                  (cons (first lob) (new-lob i (+ 1 acc) (rest lob) ws)))])) 
     
     (define (gen-nextmoves lob ws)
       [cond [(empty? lob) empty]
             [else
              (if (= X (world-state-turn ws))
                  (cons (make-world-state (new-lob (first lob) 0 (world-state-listOfBoard ws) ws) O (world-state-diff ws))
                        (gen-nextmoves (rest lob) ws))
                  (cons (make-world-state (new-lob (first lob) 0 (world-state-listOfBoard ws) ws) X (world-state-diff ws))
                        (gen-nextmoves (rest lob) ws)))]]) 
     ]
    
    (argmax
     (λ(s) (search s (world-state-diff s)))
     (gen-nextmoves (next-moves (world-state-listOfBoard ws) 0) ws)) ;;gen list
    ))  

;; functions needed:
;; 1. go through the list and call to-win
;; 2. calculate util function did O or X win and return the number corresponding with it
;; 3. check if d-acc is larger than the diff

;;next-moves, search, (apply min (list)).