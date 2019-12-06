#lang racket

(require 2htdp/image 2htdp/universe)


;;; MAIN

(define (start) 
  (big-bang (world 300 '() 'first-scene)
            (on-key input-router)
            (to-draw render)))

;;; STATE, ENTITIES, PLAYER, ETC.


(struct world (player obstacles current-scene) #:transparent #:mutable)

;;; CONSTANTS

(define INTRO-TEXT
  (text/font "
Hello and thanks for playing the office game
(Made with the Racket programming language)

Controls:

Left Arrow - move left
Right Arrow - move right

Press Space to Start!"
        14
        "indigo"
        #f
         'modern 'italic 'normal #f))

(define window-width 600)
(define window-height 400)

(define player-size 40)

(define first-scene 
  (place-image/align
   INTRO-TEXT
   (quotient (image-width INTRO-TEXT) 3) 200
   "left" "bottom"
   (empty-scene window-width window-height)))

(define game-scene
  (empty-scene window-width window-height))

;;; INPUT HANDLING AND STATE MANIPULATION

(define (offscreen? position block-size)
  (if (or (< (- position (/ window-width 2)) 0)
      (> (+ position (/ window-width 2)) window-width))
      #t
      #f))

(define (right-step current-position)
  (min (+ current-position player-size) (- window-width (/ player-size 2))))

(define (left-step current-position)
  (max (- current-position player-size) (/ player-size 2)))

(define (underline image)
  (define line-image (line (image-width image) 0 "black"))
  (overlay/offset image 0 15 line-image))

(define (move-left w)
  (set-world-player! w (left-step (world-player w)))
  w)

(define (move-right w)
  (set-world-player! w (right-step (world-player w)))
  w)

(define (input-router w key)
  (cond [(equal? (world-current-scene w) 'first-scene) (first-scene-router w key)]
        [else (game-router w key)]))

(define (first-scene-router w key)
  (if (key=? key " ")
      (let []
        (set-world-current-scene! w 'game-scene)
        w)
       w))

(define (game-router w key)
  (cond [(key=? key "left") (move-left w)]
        [(key=? key "right") (move-right w)]
        [else w]))

;;; RENDER

(define (render-game-scene w)
  (place-image
   (square player-size "solid" "blue")
   (world-player w)
   350
   game-scene))


(define (render w)
  (cond [(equal? (world-current-scene w) 'first-scene) first-scene]
         [else (render-game-scene w)]))
        
(start)
