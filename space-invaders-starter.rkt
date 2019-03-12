;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:
;;============

(define WIDTH 300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick 
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 3)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 2)
(define MAX-INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

;; Data Definitions:
;;===================

(define-struct game (invaders missiles tank))
;; Game is (make-game  Invaders Missiles Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I0 (make-invader (/ WIDTH 2) 0 INVADER-X-SPEED))
(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader 170 120 INVADER-X-SPEED)); not landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M0 (make-missile (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2)))
(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
(define M4 (make-missile (invader-x I4) (+ (invader-y I4) 10)))  ; exactly hit U4
(define M5 (make-missile (+ (invader-x I4) 10) (+ (invader-y I4) 10))); exactly hit U4

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; Invaders is one of:
;; - empty
;; - (cons Invader Invaders)
;; represents the current list of invaders

(define LOI0 empty)
(define LOI2 (list I0 I1 I2))

#;
(define (fn-for-invaders loi)
  (cond[(empty? loi) (...)]
       [(else
         (...(fn-for-invader (first loi))
             (fn-for-invaders (rest loi))))]))           

;; template rules used:
;; one of:
;; atomic distinct: empty
;; compound data: (cons Invader Invaders)
;; reference: (first loi) is Invader
;; self-reference: (rest loi) is Invaders

;; Missiles is one of:
;; - empty
;; - (cons Missile Missiles)
;; represents the current list of missiles 

(define LOM0 empty)
(define LOM2 (list M0 M1 M2))

#;
(define (fn-for-missiles lom)
  (cond[(empty? lom) (...)]
       [(else
         (...(fn-for-missile (first loi))
             (fn-for-missiles (rest loi))))]))           

;; template rules used:
;; one of:
;; atomic distinct: empty
;; compound data: (cons Missile Missiles)
;; reference: (first loi) is Missile
;; self-reference: (rest loi) is Missiles


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; Functions
;;=============

;; Game -> Game
;; start the world with (main G0)

;; no examples for main

(define (main s)
  (big-bang s
    (on-tick next-game)      ; Game -> Game
    (to-draw render-game)    ; Game -> Image
    (on-key handle-key)      ; Game KeyEvent -> Game
    (stop-when end-game?)))  ; Game -> Boolean


;; Game -> Game
;; produces the next game state which includes:
;; the next Missiles, the next Invaders and the next Tank

; (define (next-game s) G0) ; stub

(define (next-game s)
  (make-game (next-invaders (destroy-invaders (game-invaders s) (game-missiles s)))
             (next-missiles (destroy-missiles (remove-top (game-missiles s)) (game-invaders s)))
             (next-tank (game-tank s))))

;; took template from Game

;; Invader-control Functions
;;===========================

;; Invaders -> Invaders
;; produces list of invaders after the next tick 

;; No examples since the addition of invaders is based on probability and it is quite random

; (define (next-invaders loi) empty); stub

#;
(define (next-invaders loi lom)
  (cond[(empty? loi) (...)]
       [(empty? lom) (...)]
       [ else
         (fn-for-invader (first loi))
         (fn-for-missile (first lom))
         (next-invaders (rest loi) (rest lom))]))

;; template rules used:
;; - one of:
;; - atomic-distinct: empty
;; - one of:
;; - atomic-distince: empty
;; - compound data: (cons Invader Invaders)
;; - compound data: (cons Missile Missiles)
;; - reference: (first loi) is Invader
;; - reference: (first lom) is Missile
;; - self-reference: (rest loi) is Invaders
;; - self-reference: (rest loi) is Missiles

(define (next-invaders loi)
  (cond[(empty? loi)
        (if (add-new? INVADE-RATE)
            (add-invader loi)
            empty)]
       [else
        (cons (advance-invader (first loi)) (next-invaders (rest loi)))]))


;; Invaders -> Invaders
;; adds a new invader at a random x-position and y=0 with x speed = INVADER-X-SPEED at the beginning of given list

(check-random (add-invader empty) (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) empty))
(check-random (add-invader (list (make-invader 150 50 INVADER-X-SPEED)))
              (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) (list (make-invader 150 50 INVADER-X-SPEED))))
(check-random (add-invader (cons (make-invader 150 50 INVADER-X-SPEED) (cons (make-invader 120 70 INVADER-X-SPEED) empty)))
              (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED)
                    (cons (make-invader 150 50 INVADER-X-SPEED) (cons (make-invader 120 70 INVADER-X-SPEED) empty))))

; (define (add-invader loi) loi); stub

(define (add-invader loi)
  (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi))


;; Natural -> Boolean
;; produces #true or #false based on the given invade rate (n), the higher the invade rate, the greater the chance of #true

(check-random (add-new? 1) (> (random MAX-INVADE-RATE) (- MAX-INVADE-RATE 1)))

; (define (add-new? n) #true); stub

(define (add-new? n)
  (> (random MAX-INVADE-RATE) (- MAX-INVADE-RATE n)))


;; Invader -> Invader
;; advances the given invader's x-coordinate and y-coordinate by INVADER-X-SPEED and INVADER-Y-SPEED respectively
;; also bounces off invader by 45 degrees when it hits an edge

(check-expect (advance-invader (make-invader 100 50 INVADER-X-SPEED))
              (make-invader (+ 100 INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) INVADER-X-SPEED)); middle

(check-expect (advance-invader (make-invader (- WIDTH 1) 50 INVADER-X-SPEED))
              (make-invader WIDTH (+ 50 INVADER-Y-SPEED) (- INVADER-X-SPEED)))

(check-expect (advance-invader (make-invader 1 50 (- INVADER-X-SPEED)))
              (make-invader 0 (+ 50 INVADER-Y-SPEED) INVADER-X-SPEED))

; (define (advance-invader i) i); stub

(define (advance-invader i)
  (cond[(> (+ (invader-x i) (invader-dx i)) WIDTH) (make-invader WIDTH (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
       [(< (+ (invader-x i) (invader-dx i)) 0) (make-invader 0 (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
       [ else (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))]))

;; took template from Invader


;; Invaders Missiles-> Invaders
;; produces a new list of invaders after removing invaders which are in the HIT-RANGE of any missile

(check-expect (destroy-invaders empty (list M0 M2 M3)) empty)
(check-expect (destroy-invaders (list I0 I2 I3) empty) (list I0 I2 I3))
(check-expect (destroy-invaders (list I0 I2 I3) (list M0 M2 M3)) (list I0 I2 I3))
(check-expect (destroy-invaders (list I0 I1 I2 I3) (list M0 M1 M2 M3)) (list I0 I2 I3))
(check-expect (destroy-invaders (list I0 I1 I2 I3 I4) (list M0 M1 M2 M3 M4)) (list I0 I2 I3))
(check-expect (destroy-invaders (list I4) (list M5)) empty)

; (define (destroy-invaders loi) loi); stub

(define (destroy-invaders loi lom)
  (cond[(empty? loi) empty]
       [(empty? lom) loi]
       [ else
         (if (ishit? (first loi) lom)
             (destroy-invaders (rest loi) lom)           
             (cons (first loi) (destroy-invaders (rest loi) lom)))]))

;; took template from the function next-invaders

;; Invader Missiles -> Boolean
;; produces true if the given invader is in the HIT-RANGE of a missile out of all the missiles given to it, false otherwise

(check-expect (ishit? I0 (list M0 M2 M3)) #false)
(check-expect (ishit? I1 (list M2)) #true)
(check-expect (ishit? I1 empty) #false)

; (define (ishit? i lom) #false); stub

(define (ishit? i lom)
  (cond[(empty? lom) #false]
       [else
        (if (mhiti? i (first lom))
            #true
            (ishit? i (rest lom)))]))

;; constructed template from Invader and Missiles


;; Invader Missile -> Boolean
;; produces true if the given invader in the HIT-RANGE of the given missile, false otherwise

(check-expect (mhiti? I0 M0) #false)
(check-expect (mhiti? I1 M2) #true)
(check-expect (mhiti? I1 M3) #true)

; (define mhiti? i m) #false); stub

(define (mhiti? i m)
  (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
       (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))


;; took template from Invader and Missile


;; Missile-control Functions
;;=============================

;; Missiles Invaders -> Missiles
;; produces list of missiles after the next tick 

(check-expect (next-missiles empty) empty)
(check-expect (next-missiles (list M0 M1))
              (list (make-missile (/ WIDTH 2) (- (- HEIGHT TANK-HEIGHT/2) MISSILE-SPEED))
                    (make-missile 150 (- 300 MISSILE-SPEED))))
; (define (next-missiles lom) empty); stub

(define (next-missiles lom)
  (cond[(empty? lom) empty]
       [ else
         (cons (advance-missile (first lom)) (next-missiles (rest lom)))]))


;; Missile -> Missile
;; reduces y-coordinate of missile by MISSILE-SPEED

(check-expect (advance-missile (make-missile (/ WIDTH 2) 200)) (make-missile (/ WIDTH 2) (- 200 MISSILE-SPEED)))

; (define (advance-missile m) m); stub

(define (advance-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; took template from Missile


;; Missiles -> Missiles 
;; given a list of missiles with those that are off the top edge already removed
;; produces a new list of missiles after removing missiles which HIT any invader 

(check-expect (destroy-missiles empty (list I0 I2)) empty)
(check-expect (destroy-missiles (list M0 M2 M3) empty) (list M0 M2 M3))
(check-expect (destroy-missiles (list M0 M2 M3) (list I0 I2 I3)) (list M0 M2 M3))
(check-expect (destroy-missiles (list M0 M1 M2 M3) (list I0 I1 I2 I3)) (list M0 M1))
(check-expect (destroy-missiles (list M0 M1 M2 M3 M4) (list I0 I1 I2 I3 I4)) (list M0 M1))
(check-expect (destroy-missiles (list M5) (list I4)) empty)

;(define (destroy-missiles lom loi) empty); stub

(define (destroy-missiles lom loi)
  (cond[(empty? lom) empty]
       [(empty? loi) lom]
       [ else
         (if (hashit? (first lom) loi)
             (destroy-missiles (rest lom) loi)           
             (cons (first lom) (destroy-missiles (rest lom) loi)))]))

;; took template from the function destroy-invaders which is similar to this one


;; Missile ListOfInvaders -> Boolean
;; produces true if the given missile has hit an invader, false otherwise

(check-expect (hashit? M2 (list I0 I1)) #true)
(check-expect (hashit? M3 (list I1)) #true)
(check-expect (hashit? M2 (list I0)) #false)
(check-expect (ishit? M1 empty) #false)

; (define (hashit? m loi) #false); stub

(define (hashit? m loi)
  (cond[(empty? loi) #false]
       [else
        (if (mhiti? (first loi) m)
            #true
            (hashit? m (rest loi)))]))

;; took template from Missile and Invaders


;; Missiles -> Missiles
;; takes a list of missiles and produces a new list of missiles with all those missiles whose y-coordinates are 0
;; removed from the new list

(check-expect (remove-top empty) empty)
(check-expect (remove-top (list M0 M1 M2)) (list M0 M1 M2))
(check-expect (remove-top (list M0 M1 M2 (make-missile 150 0))) (list M0 M1 M2))
(check-expect (remove-top (list M0 M1 M2 (make-missile 150 -23))) (list M0 M1 M2))

; (define (remove-top lom) lom); stub

(define (remove-top lom)
  (cond[(empty? lom) empty]
       [else
        (if (<= (missile-y (first lom)) 0)
            (remove-top (rest lom))
            (cons (first lom) (remove-top (rest lom))))]))

;; template rules used:
;; - one of:
;; - atomic distinct: empty
;; - compound data: (cons Missile Missiles)
;; - self-reference: (rest lom) is Missiles
;; - reference: (first lom) is Missile


;; Tank-control Functions
;;==========================

;; Tank -> Tank
;; produces tank after the next tick

(check-expect (next-tank (make-tank (/ WIDTH 2) 1)) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (next-tank (make-tank (/ WIDTH 2) -1)) (make-tank (+ (/ WIDTH 2) (- TANK-SPEED)) -1))
(check-expect (next-tank (make-tank (- WIDTH TANK-WIDTH/2) 1)) (make-tank (- WIDTH TANK-WIDTH/2) 1))
(check-expect (next-tank (make-tank (- WIDTH TANK-WIDTH/2) -1)) (make-tank (- WIDTH TANK-WIDTH/2 TANK-SPEED) -1))
(check-expect (next-tank (make-tank (+ 0 TANK-WIDTH/2) 1)) (make-tank (+ 0 TANK-WIDTH/2 TANK-SPEED) 1))
(check-expect (next-tank (make-tank (+ 0 TANK-WIDTH/2 TANK-SPEED) -1)) (make-tank (+ 0 TANK-WIDTH/2) -1))
(check-expect (next-tank (make-tank (+ 0 TANK-WIDTH/2) -1)) (make-tank (+ 0 TANK-WIDTH/2) -1))

; (define (next-tank t) T0); stub

(define (next-tank t)
  (cond[(and (> (tank-dir t) 0) (>= (tank-x t) (- WIDTH TANK-WIDTH/2)))
        (make-tank (- WIDTH TANK-WIDTH/2) (tank-dir t))]
       [(and (< (tank-dir t) 0) (<= (tank-x t) TANK-WIDTH/2))
        (make-tank TANK-WIDTH/2 (tank-dir t))]
       [else
        (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))
     

;; Game KeyEvent -> Game
;; moves the tank in the direction of left or right arrow keys when they are pressed and fires a missile when space is pressed

(check-expect (handle-key (make-game (list I0 I1 I2) (list M1 M2) T0) " ") (make-game (list I0 I1 I2) (list M0 M1 M2) T0))
(check-expect (handle-key (make-game (list I0 I1 I2) (list M1 M2) T0) "left")
              (make-game (list I0 I1 I2) (list M1 M2) (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-key (make-game (list I0 I1 I2) (list M1 M2) T0) "right")
              (make-game (list I0 I1 I2) (list M1 M2) T0))
(check-expect (handle-key (make-game (list I0 I1 I2) (list M1 M2) (make-tank (/ WIDTH 2) -1)) "right")
              (make-game (list I0 I1 I2) (list M1 M2) (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-key (make-game (list I0 I1 I2) (list M1 M2) (make-tank (/ WIDTH 2) -1)) "left")
              (make-game (list I0 I1 I2) (list M1 M2) (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-key (make-game (list I0 I1 I2) (list M1 M2) (make-tank (/ WIDTH 2) -1)) "a")
              (make-game (list I0 I1 I2) (list M1 M2) (make-tank (/ WIDTH 2) -1)))
                          
; (define (handle-key s ke) ; stub

(define (handle-key s ke)
  (cond [(key=? ke "left") (make-game (game-invaders s) (game-missiles s) (make-tank (tank-x (game-tank s)) -1))]
        [(key=? ke "right") (make-game (game-invaders s) (game-missiles s) (make-tank (tank-x (game-tank s)) 1))]
        [(key=? ke " ") (make-game (game-invaders s)
                                   (cons (make-missile (tank-x (game-tank s)) (- HEIGHT TANK-HEIGHT/2))(game-missiles s))
                                   (game-tank s))]
        [else s]))

;; used the typical key handler template and took template from Game


;; Rendering Functions
;;=====================

;; Game -> Image
;; produces image based on the current Game it is given

(check-expect (render-game (make-game empty empty (make-tank (/ WIDTH 2) 1)))
              (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

(check-expect (render-game (make-game (list I0 I1) (list M1) (make-tank (/ WIDTH 2) 1)))
              (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2)
                           (place-image INVADER (invader-x I0) (invader-y I0)
                                        (place-image INVADER (invader-x I1) (invader-y I1)
                                                     (place-image MISSILE (missile-x M1) (missile-y M1)
                                                                  BACKGROUND)))))

; (define (render-game s) BACKGROUND) ; stub

(define (render-game s)
  (place-image TANK (tank-x (game-tank s)) (- HEIGHT TANK-HEIGHT/2) (render-invaders (game-invaders s) (game-missiles s))))

;; took template from Game


;; Invaders Missiles -> Image
;; produces image based on the current positions of invaders and missiles

(check-expect (render-invaders empty empty) BACKGROUND)
(check-expect (render-invaders empty (list M1)) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))
(check-expect (render-invaders (list I0 I1) (list M1)) (place-image INVADER (invader-x I0) (invader-y I0)
                                                                    (place-image INVADER (invader-x I1) (invader-y I1)
                                                                                 (place-image MISSILE (missile-x M1) (missile-y M1)
                                                                                              BACKGROUND))))
; (define (render-invaders loi lom) BACKGROUND); stub

(define (render-invaders loi lom)
  (cond[(empty? loi) (render-missiles lom)]
       [ else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi))
                      (render-invaders (rest loi) lom))]))


;; Missiles -> Image
;; produces an image based on the current positions of the missiles given

(check-expect (render-missiles empty) BACKGROUND)
(check-expect (render-missiles (list M1 M2)) (place-image MISSILE (missile-x M1) (missile-y M1)
                                                          (place-image MISSILE (missile-x M2) (missile-y M2) BACKGROUND)))
                                                                       
; (define (render-missiles lom) BACKGROUND); stub

(define (render-missiles lom)
  (cond [(empty? lom) BACKGROUND]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom)) (render-missiles (rest lom)))]))

;; took template from the function next-invaders


;; End Of Game Functions:
;;========================

;; Game -> Boolean
;; returns false if endgame2? returns false, returns true if endgame2? returns true - basically the same function except that it
;; takes a game as input and produces Boolean

; (define (end-game? s) #false); stub

(define (end-game? s)
  (end-game2? (game-invaders s)))

;; Invaders -> Boolean
;; returns false if none of the invaders has reached a y-coordinate of HEIGHT, returns true otherwise

(check-expect (end-game2? empty) #false)
(check-expect (end-game2? (list I0 I1 I2 I3)) #true)
(check-expect (end-game2? (list I0 I1)) #false)
                        
; (define (end-game2? loi) #false); stub

(define (end-game2? loi)
  (cond[(empty? loi) #false]
       [else
        (if (>= (invader-y (first loi)) HEIGHT)
            #true
            (end-game2? (rest loi)))]))

;; took template from Game
(main G0)