;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter_solved) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)
(define INVADE-DENOM 200); interp. Invader created every INVADE-RATE ouf of INVADE-DENOM

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
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
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

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
(define M4 (make-missile 50 TANK-Y))                      ; missile created at position 50 where T1 and T2 lie
(define M5 (make-missile (/ WIDTH 2) TANK-Y))             ; missile created at T0's position 

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

(define G4 (make-game empty empty T2))
(define G5 (make-game empty (list M4) T2))
(define G6 (make-game (list I1 I2) (list M4 M1 M2) T1))
(define G7 (make-game empty (list M5) T0))
(define G8 (make-game (list I1 I2) (list M1 M2) T2))

(define G9 (make-game (list I1 I3) (list M1 M2) T2)) ; one invader more than landed, with missiles
(define G10 (make-game (list I1 I3) empty T2))       ; one invader more than landed, no missiles


;; =================
;; Functions:

;; Game -> Game
;; Start space invaders by evaluating (main Game)

(define (main G0)
  (big-bang G0
    (on-key  key-update)               ; Game KeyEvent -> Game
    (on-tick  next-game)               ; Game -> Game
    (to-draw  render-game)             ; Game -> Image
    (stop-when game-over?)))           ; Game -> Boolean


;; ====DONE
;; key-update 

;; Game KeyEvent -> Game
;; Moves tank left/right if left/right keys pressed, fires missile if space bar pressed

(check-expect (key-update G1 "left") G4)
(check-expect (key-update G1 "right") G1)
(check-expect (key-update G4 "left") G4)
(check-expect (key-update G4 "right") G1)
(check-expect (key-update G3 "left") G8)
(check-expect (key-update G3 "right") G3)

(check-expect (key-update G4 "a") G4)
(check-expect (key-update G3 "b") G3)

(check-expect (key-update G4 " ") G5)
(check-expect (key-update G3 " ") G6)



;; <Teamplate from KeyEvent>

(define (key-update g ke)
  (cond [(key=? ke "left")  (update-left g)]
        [(key=? ke "right") (update-right g)]
        [(key=? ke " ") (fire-missile g)]
        [else g]))

;; ====DONE
;; update-left

;; Game -> Game
;; Update tank direction to left in a game
(check-expect (update-left G1) G4)
(check-expect (update-left G4) G4)
(check-expect (update-left G3) G8)

; (define (update-left g) g)  ;stub
;; <Template from Game>

(define (update-left g)
  (make-game (game-invaders g)
             (game-missiles g)
             (uleft (game-tank g))))

;; ====DONE
;; uleft

;; Tank -> Tank
;; Update tank direction to left
(check-expect (uleft T1) T2)
(check-expect (uleft T2) T2)

; (define (uleft t) t) ;stub
;; <Template from Tank>

(define (uleft t)
  (make-tank (tank-x t) -1))


;; ====DONE
;; update-right

;; Game -> Game
;; Update tank direction to left in a game
(check-expect (update-right G1) G1)
(check-expect (update-right G4) G1)
(check-expect (update-right G3) G3)

; (define (update-right g) g)  ;stub
;; <Template from Game>

(define (update-right g)
  (make-game (game-invaders g)
             (game-missiles g)
             (uright (game-tank g))))

;; ====DONE
;; uright

;; Tank -> Tank
;; Update tank direction to right
(check-expect (uright T1) T1)
(check-expect (uright T2) T1)

; (define (uright t) t) ;stub
;; <Template from Tank>

(define (uright t)
  (make-tank (tank-x t) 1))

;; ====DONE
;; fire-missile

;; Game -> Game
;; Fire missile from tank at position x of tank, and at height TANK-HEIGHT/2
(check-expect (fire-missile G4) G5)
(check-expect (fire-missile G3) G6) 


;(define (fire-missile g) g) ;stub
; <Template from Game>

(define (fire-missile g)
  (make-game (game-invaders g)
             (new-missile (game-missiles g) (game-tank g))
             (game-tank g)))

;; ====DONE
;; new-missile

;; ListOfMissile Tank -> ListOfMissile
;; Updates list of Missiles with missile at position x of tank, and at height TANK-HEIGHT/2
(check-expect (new-missile empty T1) (list (make-missile (tank-x T1) TANK-Y)))
(check-expect (new-missile (list M1 M2) T1) (list (make-missile (tank-x T1) TANK-Y) M1 M2))

;(define (new-missile lom t) lom) ;stub
; <Template from Missile with modification to include list structure and Tank >

(define (new-missile lom t)
  (cons (make-missile (tank-x t) TANK-Y) lom))

;; ====
;; ====
;; ====
;; NEXT-GAME-COMPOSITION

;; ====
;; next-game

;; Game -> Game
;; Updates state of a game:
;; - Every INVADER-RATE, creates an invader from the top of the box
;; - advances tank by TANKSPEED in the appropriate direction over the X axis
;; - advances invaders by INVADER-X-SPEED and INVADER-Y-SPEED in the appropriate direction
;; - advances missiles by MISSILE-SPEED over the Y axis
;; - destroys missiles and an invaders when they are at or less than HIT-RANGE distance
;; !!!
;; EXAMPLES NEED TO ONLY TEST COMPOSITION

(check-random (next-game G0)
               (if (< (random INVADE-DENOM) INVADE-RATE)
                   (make-game (n-invader (advance-invaders (game-invaders G0))) (advance-missiles (game-missiles G0)) (advance-tank (game-tank G0)))
                   (make-game (advance-invaders (game-invaders G0)) (advance-missiles (game-missiles G0)) (advance-tank (game-tank G0)))))               ;; Next-game for a new game, random invader creation

(check-random (next-game G2)
               (if (< (random INVADE-DENOM) INVADE-RATE)
                   (make-game (n-invader (advance-invaders (game-invaders G2))) (advance-missiles (game-missiles G2)) (advance-tank (game-tank G2)))
                   (make-game (advance-invaders (game-invaders G2)) (advance-missiles (game-missiles G2)) (advance-tank (game-tank G2)))))               ;; Next-game for game with one invader, one missile, no destruction, random invader creation

(check-random (next-game G3)
               (if (< (random INVADE-DENOM) INVADE-RATE)
                   (make-game (n-invader (advance-invaders (destroy-invaders (game-invaders G3) (game-missiles G3)))) (advance-missiles (destroy-missiles (game-invaders G3) (game-missiles G3))) (advance-tank (game-tank G3)))
                   (make-game (advance-invaders (destroy-invaders (game-invaders G3) (game-missiles G3))) (advance-missiles (destroy-missiles (game-invaders G3) (game-missiles G3))) (advance-tank (game-tank G3)))))               ;; Next-game for game with two invaders, two missiles, one destruction, random invader creation
              
#;(check-random (next-game (make-game empty empty T1))
              (make-game empty empty (make-tank
                                      (+ (tank-x T1) (* (tank-dir T1) TANK-SPEED))
                                      (tank-dir T1))))                                        ;; updating one tank after tick

#;(check-random (next-game (make-game empty empty T2))
              (make-game empty empty (make-tank
                                      (+ (tank-x T2) (* (tank-dir T2) TANK-SPEED))
                                      (tank-dir T2))))                                        ;; updating one tank after tick

#;(check-random (next-game (make-game empty empty T2))
              (make-game empty empty (make-tank
                                      (+ (tank-x T2) (* (tank-dir T2) TANK-SPEED))
                                      (tank-dir T2))))                                        ;; updating one tank after tick


;(define (next-game g) g) ;stub

; <Template for Composition>

(define (next-game g)
  (new-invader (advance-game (destroy g))))


;; ====DONE
;; destroy

;; Game -> Game
;; Destroys missiles and invaders when they are at or less than HIT-RANGE distance
(check-expect (destroy (make-game empty empty T1)) (make-game empty empty T1))
(check-expect (destroy (make-game (list I1) (list M1) T1)) (make-game (list I1) (list M1) T1)) ; missile not hitting invader
(check-expect (destroy (make-game (list I1) (list M2) T1)) (make-game empty empty T1)) ; missile exactly hitting invader
(check-expect (destroy (make-game (list I1) (list M3) T1)) (make-game empty empty T1)) ; missile more than hitting invader
(check-expect (destroy (make-game (list I1 I2) (list M2 M4) T1)) (make-game (list I2) (list M4) T1)) ; missile exactly hitting invader
(check-expect (destroy (make-game (list I1 I2) (list M3 M4) T1)) (make-game (list I2) (list M4) T1)) ; missile more than hitting invader

;(define (destroy g) g) ;stub

;<Template from Game>

(define (destroy g)
  (make-game (destroy-invaders (game-invaders g) (game-missiles g)) (destroy-missiles (game-invaders g) (game-missiles g)) (game-tank g)))

;; ====DONE
;; destroy-invaders

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Destroys invaders in list when they are at or less than HIT-RANGE distance
(check-expect (destroy-invaders empty empty) empty)
(check-expect (destroy-invaders (list I1) empty) (list I1))
(check-expect (destroy-invaders empty (list M1)) empty)
(check-expect (destroy-invaders (list I1) (list M1)) (list I1)) ; missile not hitting invader
(check-expect (destroy-invaders (list I1) (list M2)) empty) ; missile exactly hitting invader
(check-expect (destroy-invaders (list I1) (list M3)) empty) ; missile more than hitting invader
(check-expect (destroy-invaders (list I1 I2) (list M2 M4)) (list I2)) ; missile exactly hitting invader
(check-expect (destroy-invaders (list I1 I2) (list M3 M4)) (list I2)) ; missile more than hitting invader


;(define (destroy-invaders loi lom) loi) ;stub

;<Temaplte from List with inclusion of Missile and Invaders>
(define (destroy-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (find-invader? (first loi) lom)
             (destroy-invaders (rest loi) lom)
             (cons (first loi) (destroy-invaders (rest loi) lom)))]))

;; ====DONE
;; find-invader?

;; Invader ListOfMissiles -> Boolean
;; Returns True if the invader is hit by one of the missiles in list
(check-expect (find-invader? I1 (list M4 M5)) false)
(check-expect (find-invader? I1 (list M1)) false)
(check-expect (find-invader? I1 (list M2)) true)
(check-expect (find-invader? I1 (list M3)) true)
(check-expect (find-invader? I1 (list M3 M5)) true)

;(define (find-invader? i lom) true) ;stub

(define (find-invader? i lom)
  (cond [(empty? lom) false]
        [else
         (and ( and (<= (- (invader-x i) HIT-RANGE) (missile-x (first lom))) (>= (+ (invader-x i) HIT-RANGE) (missile-x (first lom))))
              ( and (<= (- (invader-y i) HIT-RANGE) (missile-y (first lom))) (>= (+ (invader-y i) HIT-RANGE) (missile-y (first lom)))))]))
             
  

;; ====DONE
;; destroy-missiles

;; ListOfInvaders ListOfMissiles -> ListOfMissiles
;; Destroys missiles in list when they are at or less than HIT-RANGE distance
(check-expect (destroy-missiles empty empty) empty)
(check-expect (destroy-missiles (list I1) empty) empty)
(check-expect (destroy-missiles empty (list M1)) (list M1))
(check-expect (destroy-missiles (list I1) (list M1)) (list M1)) ; missile not hitting invader
(check-expect (destroy-missiles (list I1) (list M2)) empty) ; missile exactly hitting invader
(check-expect (destroy-missiles (list I1) (list M3)) empty) ; missile more than hitting invader
(check-expect (destroy-missiles (list I1 I2) (list M2 M4)) (list M4)) ; missile exactly hitting invader
(check-expect (destroy-missiles (list I1 I2) (list M3 M4)) (list M4)) ; missile more than hitting invader


;(define (destroy-missiles loi lom) lom) ;stub

(define (destroy-missiles loi lom)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [else
         (if (find-missile? loi (first lom))
             (destroy-missiles loi (rest lom))
             (cons (first lom) (destroy-missiles loi (rest lom))))]))

;; ====DONE
;; find-missile?

;; ListOfInvader Missiles -> Boolean
;; Returns True if the missile hits one of the invaders in list
(check-expect (find-missile? empty M1) false)
(check-expect (find-missile? (list I1) M4) false)
(check-expect (find-missile? (list I1) M2) true)
(check-expect (find-missile? (list I1) M3) true)
(check-expect (find-missile? (list I1 I2) M2) true)
(check-expect (find-missile? (list I1 I2) M3) true)
(check-expect (find-missile? (list I1 I2) M4) false)

;(define (find-missile? loi m) true) ;stub

(define (find-missile? loi m)
  (cond [(empty? loi) false]
        [else
         (and ( and (<= (- (invader-x (first loi)) HIT-RANGE) (missile-x m)) (>= (+ (invader-x (first loi)) HIT-RANGE) (missile-x m)))
              ( and (<= (- (invader-y (first loi)) HIT-RANGE) (missile-y m)) (>= (+ (invader-y (first loi)) HIT-RANGE) (missile-y m))))]))





;; ====DONE
;; advance-game

;; Game -> Game
;; Advances missiles list, invaders list, and tank
(check-expect (advance-game (make-game empty empty (make-tank 40 -1))) (make-game empty empty (make-tank (+ 40 (* TANK-SPEED -1)) -1)))
(check-expect (advance-game (make-game
                             (list (make-invader 150 100 -1) (make-invader 300 50 1))
                             (list (make-missile 100 10) (make-missile 10 30))
                             (make-tank 40 -1)))
              (make-game (list (make-invader (+ 150 (* INVADER-X-SPEED -1)) (+ 100 INVADER-Y-SPEED) -1) (make-invader (+ 300 (* INVADER-X-SPEED -1)) (+ 50 INVADER-Y-SPEED) -1))
                         (list (make-missile 100 (- 10 MISSILE-SPEED)) (make-missile 10 (- 30 MISSILE-SPEED)))
                         (make-tank (+ 40 (* TANK-SPEED -1)) -1)))
                         

; (define (advance-game g) g) ;stub
;<Template from Game>
(define (advance-game g)
  (make-game (advance-invaders (game-invaders g))
             (advance-missiles (game-missiles g))
             (advance-tank     (game-tank g))))

;; ====DONE
;; advance-invaders

;; ListOfInvaders -> ListOfInvaders
;; Advances invaders list by INVADER-X-SPEED-X towards dx, and INVADER-Y-SPEED towards Y, changes invader direction when hit the wall 
(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (list (make-invader 150 100 -1))) (list (make-invader (+ 150 (* INVADER-X-SPEED -1)) (+ 100 INVADER-Y-SPEED) -1))) ; normal advancement, one invader
(check-expect (advance-invaders (list (make-invader 0 100 -1))) (list (make-invader (+ 0 (* INVADER-X-SPEED 1)) (+ 100 INVADER-Y-SPEED) 1))) ; one invader hits the left and changes direction
(check-expect (advance-invaders (list (make-invader 300 50 1))) (list (make-invader (+ 300 (* INVADER-X-SPEED -1)) (+ 50 INVADER-Y-SPEED) -1))) ; one invader hits the left and changes direction
(check-expect (advance-invaders (list (make-invader 150 100 -1) (make-invader 300 50 1))) (list (make-invader (+ 150 (* INVADER-X-SPEED -1)) (+ 100 INVADER-Y-SPEED) -1) (make-invader (+ 300 (* INVADER-X-SPEED -1)) (+ 50 INVADER-Y-SPEED) -1))) ; two invaders invader hits the left and changes direction
              

;(define (advance-invaders loi) loi) ;stub

;<Template from Invaders with modification to include list>

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader (first loi)) (advance-invaders (rest loi)))]))


;; ====DONE
;; advance-invader

;; Invader -> Invader
;; Advances invader by INVADER-X-SPEED towards dx, and INVADER-Y-SPEED towards Y, and changes direction if invader hits wall 
(check-expect (advance-invader (make-invader 150 100 -1)) (make-invader (+ 150 (* INVADER-X-SPEED -1)) (+ 100 INVADER-Y-SPEED) -1)) ; normal advancement, one invader
(check-expect (advance-invader (make-invader 0 100 -1))  (make-invader (+ 0 (* INVADER-X-SPEED 1)) (+ 100 INVADER-Y-SPEED) 1))      ; one invader hits the left and changes direction
(check-expect (advance-invader (make-invader WIDTH 50 1))  (make-invader (+ WIDTH (* INVADER-X-SPEED -1)) (+ 50 INVADER-Y-SPEED) -1))  ; one invader hits the right and changes direction

;(define (advance-invader i) i) ;stub

;<Template from invader>

(define (advance-invader i)
  (cond [(hit-wall? i) (make-invader (+ (invader-x i) (* INVADER-X-SPEED (- (invader-dx i)))) (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
        [else (make-invader (+ (invader-x i) (* INVADER-X-SPEED (invader-dx i))) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))]))

;; ====DONE
;; hit-wall?

;; Invader -> Boolean
;; Returns true if invader hits well, ie if invader-x is <=0 or >=WIDTH
(check-expect (hit-wall? (make-invader 150 100 -1)) false)
(check-expect (hit-wall? (make-invader 0 100 -1)) true)     
(check-expect (hit-wall? (make-invader 303 50 1)) true)

;(define (hit-wall? i) true) ;stub

(define (hit-wall? i)
  (or (and (>= (invader-x i) WIDTH) (> (invader-dx i) 0)) (and (<= (invader-x i) 0) (< (invader-dx i) 0))))



;; ====DONE
;; advance-missiles

;; ListOfMissiles -> ListOfMissiles
;; Advances missiles list by MISSILE-SPEED in Y axis towards top
(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list (make-missile 100 10))) (list (make-missile 100 (- 10 MISSILE-SPEED))))
(check-expect (advance-missiles (list (make-missile 100 10) (make-missile 10 30))) (list (make-missile 100 (- 10 MISSILE-SPEED)) (make-missile 10 (- 30 MISSILE-SPEED))))

;(define (advance-missiles lom) lom) ;stub

;<Template from self-reference list>

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (advance-missile (first lom)) (advance-missiles (rest lom)))]))

;; ====DONE
;; advance-missile

;; Missile -> Missile
;; Advances missile by MISSILE-SPEED in Y axis towards top
(check-expect (advance-missile (make-missile 100 10)) (make-missile 100 (- 10 MISSILE-SPEED)))
(check-expect (advance-missile (make-missile 50 20)) (make-missile 50 (- 20 MISSILE-SPEED)))

;(define (advance-missile m) m) ;stub

;<Template from Missile>

(define (advance-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ====DONE
;; advance-tank

;; Tank -> Tank
;; Advances tank by TANK-SPEED in tank direction in X axis
(check-expect (advance-tank (make-tank 10 -1)) (make-tank (+ 10 (* TANK-SPEED -1)) -1))
(check-expect (advance-tank (make-tank 100 1)) (make-tank (+ 100 (* TANK-SPEED 1)) 1))

;(define (advance-tank t) t) ;stub

;<Template from Tank>

(define (advance-tank t)
  (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t)))




;; ====DONE
;; new-invader

;; Game -> Game
;; Create a new invader from top of screen with random x, every INVADE-RATE
(check-random (new-invader (make-game empty empty T1))
              (if (< (random INVADE-DENOM) INVADE-RATE) (make-game (list (make-invader (random WIDTH) 0 INVADER-X-SPEED)) empty T1) (make-game empty empty T1)))
(check-random (new-invader (make-game (list I1) empty T1))
              (if (< (random INVADE-DENOM) INVADE-RATE) (make-game (list (make-invader (random WIDTH) 0 INVADER-X-SPEED) I1) empty T1) (make-game (list I1) empty T1)))
(check-random (new-invader (make-game (list I1 I2) (list M1 M2) T1))
              (if (< (random INVADE-DENOM) INVADE-RATE) (make-game (list (make-invader (random WIDTH) 0 INVADER-X-SPEED) I1 I2) (list M1 M2) T1) (make-game (list I1 I2) (list M1 M2) T1)))

;(define (new-invader g) g) ;stub

; <Template from Game>

(define (new-invader g)
  (cond
    [(< (random INVADE-DENOM) INVADE-RATE)
     (make-game (n-invader (game-invaders g))
                (game-missiles g)
                (game-tank     g))]
    [else g]
    ))


;; ====DONE
;; n-invader

;; ListOfInvaders -> ListOfInvaders
;; Adds to the list of invaders a new invader from top of screen, random x, every INVADE-RATE/INVADE-DENOM
(check-random (n-invader empty) (list (make-invader (random WIDTH) 0 INVADER-X-SPEED)))
(check-random (n-invader (list I1)) (list (make-invader (random WIDTH) 0 INVADER-X-SPEED) I1))
(check-random (n-invader (list I1 I2)) (list (make-invader (random WIDTH) 0 INVADER-X-SPEED) I1 I2))

;(define (n-invader loi) loi) ;stub

; <Template from Invader with modification to include list>

(define (n-invader loi)
  (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi)) 



;; ====DONE
;; ====
;; ====
;; render-game

;; Game -> Image
;; Renders game by drawing:
;; - tank in its position
;; - missiles in their positions
;; - invaders in their positions
(check-expect (render-game G0) (place-image TANK (tank-x T0) TANK-Y BACKGROUND))
(check-expect (render-game G2) (place-image TANK (tank-x T1) TANK-Y
                                            (place-image MISSILE (missile-x M1) (missile-y M1)
                                                         (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND))))
(check-expect (render-game G3) (place-image TANK (tank-x T1) TANK-Y
                                            (place-image MISSILE (missile-x M1) (missile-y M1)
                                                         (place-image MISSILE (missile-x M2) (missile-y M2)
                                                                      (place-image INVADER (invader-x I1) (invader-y I1)
                                                                                   (place-image INVADER (invader-x I2) (invader-y I2) BACKGROUND))))))

;(define (render-game g) BACKGROUND) ;stub

;; Template for Game + Composition 
(define (render-game g)
  (render-tank (game-tank g) (render-missiles (game-missiles g) (render-invaders (game-invaders g)))))

;; ====DONE
;; render-invaders

;; ListOfInvaders -> Image
;; Draws invaders on bakcground in their x,y positions
(check-expect (render-invaders empty) BACKGROUND)
(check-expect (render-invaders (list I1)) (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND))
(check-expect (render-invaders (list I1 I2)) (place-image INVADER (invader-x I1) (invader-y I1)
                                                          (place-image INVADER (invader-x I2) (invader-y I2) BACKGROUND)))

 
;(define (render-invaders loi) BACKGROUND) ;stub

; <Template from list with inclusion of Invader>

(define (render-invaders loi)
  (cond [(empty? loi) BACKGROUND]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi)) (render-invaders (rest loi)))]))

;; ====DONE
;; render-missiles

;; ListOfMissiles Image -> Image
;; Draws missiles in their x,y positions on top of input image
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list M1) BACKGROUND) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))
(check-expect (render-missiles (list M1 M2) BACKGROUND) (place-image MISSILE (missile-x M1) (missile-y M1)
                                                                     (place-image MISSILE (missile-x M2) (missile-y M2) BACKGROUND)))
(check-expect (render-missiles (list M1 M2) (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND)) (place-image MISSILE (missile-x M1) (missile-y M1)
                                                                     (place-image MISSILE (missile-x M2) (missile-y M2) (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND))))
 
;(define (render-missiles lom img) img) ;stub

; <Template from list with inclusion of Missile>

(define (render-missiles lom img)
   (cond [(empty? lom) img]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom)) (render-missiles (rest lom) img))]))

  

;; ====DONE
;; render-tank

;; Tank Image -> Image
;; Draws Tank on its x position and at height TANK-HEIGHT/2 on top of input image
(check-expect (render-tank T2 BACKGROUND) (place-image TANK (tank-x T2) TANK-Y BACKGROUND))
(check-expect (render-tank T1 (place-image MISSILE (missile-x M1) (missile-y M1)
                                                         (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND)))
              (place-image TANK (tank-x T1) TANK-Y (place-image MISSILE (missile-x M1) (missile-y M1)
                                                         (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND))))
 
;(define (render-tank tank img) img) ;stub

; <Template from Tank with modification>
(define (render-tank t img)
  (place-image TANK (tank-x t) TANK-Y img))

 
   



;; ====DONE
;; ====
;; ====
;; game-over?

;; Game -> Boolean
;; Returns true as soon as any invader lands (i.e. invader-x >= HEIGHT)

(check-expect (game-over? G0)  false)
(check-expect (game-over? G2)  false)
(check-expect (game-over? G3)  true)
(check-expect (game-over? G9)  true)
(check-expect (game-over? G10) true)

;(define (game-over? g) false) ;stub

; <Template from Game>

(define (game-over? g)
  (landed? (game-invaders g)))

;; ====DONE
;; landed?

;; ListOfInvaders -> Boolean
;; Returns true if at least of of the invaders has landed (i.e. invader-x >= HEIGHT)

(check-expect (landed? empty)  false)
(check-expect (landed? (list I1))  false)
(check-expect (landed? (list I2))  true)
(check-expect (landed? (list I3))  true)
(check-expect (landed? (list I2 I3))  true)
(check-expect (landed? (list I1 I3))  true)

;(define (landed? loi) false) ;stub

;<Template from ListOfInvaders>

(define (landed? loi)
  (cond [(empty? loi) false]
        [(landed-invader? (first loi)) true]
        [else (landed? (rest loi))]))

;; ====DONE
;; landed-invader?

;; Invaders -> Boolean
;; Returns true if at least of of the invaders has landed (i.e. invader-x >= HEIGHT)

(check-expect (landed-invader? I1)  false)
(check-expect (landed-invader? I2)  true)
(check-expect (landed-invader? I3)  true)

;(define (landed-invader? i) false) ;stub

;<Template from Invader>

(define (landed-invader? i)
  (>= (invader-y i) HEIGHT))







