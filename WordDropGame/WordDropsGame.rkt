;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 15 |) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define GRID-HEIGHT 40)
(define GRID-WIDTH 40)
(define CELL-HEIGHT 15)
(define CELL-WIDTH 15)
(define ACTIVE-COLOR "green")
(define TYPING-COLOR "purple")
(define STUCK-COLOR "red")
(define SCENE-HEIGHT (* GRID-HEIGHT CELL-HEIGHT))
(define SCENE-WIDTH (* GRID-WIDTH CELL-WIDTH))
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define PLAYGROUND (empty-scene 600 650))
;BACKGROUND is the full background of the game

(define frequency 0.5)

;A word is a [string pos]
(define-struct word[string position])
;string is the word string
;posn is the position of the word
(define word1 (make-word "Good"    (make-posn 10 10)))
(define word2 (make-word "Morning" (make-posn 11 11)))
(define word3 (make-word "Fundies" (make-posn 12 11)))
(define word4 (make-word "Midterm" (make-posn 13 11)))

#;(define(word-temp word)
    (...(word-string word)...
        (word-position word)...))


; A LOS(List of String) is one of:
; - empty
; - (cons String LOS)

(define LOS1(list "about" "after" "again" "air" "all"
                  "along""also""an""and""another""any""are""around""as""at"
                  "away"" back""be""because""been""before""below""between""both""dog""cat""rain""apple"
                  "beer""bar""cap""hap""ysl""muf""dior""channel""lauramercier""fancl""shiseido""marc""hermes"
                  "but"))

(define LOS2(list "about" "after" "again" "air" "all"
                  "along""also""an""and""another""any""are""around""as""at"
                  "away"" back""be""because""been""before""below""between""both"
                  "but"))


#;(define (LOS-temp LOS)
    (cond [(empty? LOS)...]
          [else ...(first LOS)...
                ...LOS-temp(rest LOS)...]))


; A LOW(List of Word) is one of:
; - empty
; - (cons word LOW)

(define LOW1 (cons word2 (cons word1 empty)))
(define LOW2 (cons word4 (cons word3 empty)))

#;(define (LOW-temp LOW)
    (cond [(empty? LOW)...]
          [else ...(first LOW)...
                ...LOW-temp(rest LOW)...]))

; A LOP is one of:
; - empty
; - (cons Posn LOP)

#;(define (LOP-temp LOP)
    (cond [(empty? LOP)...]
          [else ...(first LOP)...
                ...LOP-temp(rest LOP)...]))


;A world is a [LOW LOW stirng Number]
(define-struct world[falling-words bottom-words typing-words time])
;falling-words are the words that are falling
;bottom-words are the words that at the bottom of the screen
;typing-words is the word that player is typing
;time is the time tick of the game
(define WORLD0 (make-world empty empty "" 0))

(define world1 (make-world LOW1 LOW2 "Nice" 1.00))

#;(define (world-temp world)
    (...(LOW-temp(world-falling-words world))...
        (LOW-temp(world-bottom-words world))...
        (world-typing-words world)...
        (world-time world)...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;draw-world:World -> image
;draw out the LOW image
(check-expect (draw-world world1)
              (draw-falling-words (world-falling-words world1)
                                  (draw-bottom-words(world-bottom-words world1)
                                                    (draw-typing-words (world-typing-words world1) PLAYGROUND))))
(define (draw-world world)
  (draw-falling-words (world-falling-words world)
                      (draw-bottom-words(world-bottom-words world)
                                        (draw-typing-words (world-typing-words world) PLAYGROUND))))

;drawonewword: w i -> image
;drawout one word
(define (drawoneword w i)
  (placetheword (drawalphabet (explode(word-string w))) (word-position w) i))

;draw-falling-words: low i -> image
;draw the falling words

(define (draw-falling-words  low i)
  (foldr drawoneword i low))
                                                                                                   
;List of sting -> Image
;Draw out the list of string that was explode before
(define(drawalphabet los)
  (foldr (lambda(string image) (beside(text string 15 "green")image))
         empty-image
         los))

;placetheword: image posn-> image
(define (placetheword image posn i)
  (place-image image (* (posn-x posn) 15)
               (* (posn-y posn) 15 )
               i))

;draw-bottom-words: low i -> image
;draw the bottom words
(define (draw-bottom-words low i)
  (foldr drawoneword-2 i low))                                                                      


;drawonwword: w -> image
;drawout one word
(define (drawoneword-2 w i)
  (placetheword-2 (drawalphabet-2 (explode(word-string w))) (word-position w) i))

;List of sting -> Image
;Draw out the list of string that was explode before
(define(drawalphabet-2 los)
  (foldr (lambda(string image) (beside(text string 15 "red")image))
         empty-image
         los))

;placetheword: image posn-> image
(define (placetheword-2 image posn i)
  (place-image image (* (posn-x posn) 15)
               (* (posn-y posn) 15 )
               i))


;draw-words: String Image-> Image
;draw out a word that is typing
(define s1 "Nice")

(check-expect(draw-typing-words s1 PLAYGROUND)
             (place-image (text s1 15 TYPING-COLOR) 300 580 PLAYGROUND))

(define(draw-typing-words s PLAYGROUND)
  (place-image (text s 15 TYPING-COLOR)
               300
               580 PLAYGROUND))


;updateworld: World Frequency -> World
;Purpose: updates the world by frequency
(check-expect (world? (update-world WORLD0)) #true)

(define (update-world w)
  (make-world (dropfallingwords (cons (generate-new-moving-word/text
                                       (maybe-generate-new-moving-word w)
                                       (list-ref LOS1 (random 25)))
                                      (world-falling-words w))
                                (world-bottom-words w))
              (stackbottomwords (world-falling-words w) (world-bottom-words w))
              (world-typing-words w)
              (+ 1 (world-time w))))

; maybe-generate-new-moving-word : World -> Boolean
; Maybe generate a new moving word if it's the appropriate time
(check-expect (maybe-generate-new-moving-word WORLD0) #true)
(check-expect (maybe-generate-new-moving-word world1) #false)

(define (maybe-generate-new-moving-word w)
  (cond[(even? (world-time w)) #true]
       [else #false]))
 
; generate-new-moving-word/text : Boolean String -> Word
; Generate a new moving word with this text
(check-expect (word? (generate-new-moving-word/text #true "hello")) #true)
(check-expect (generate-new-moving-word/text #false "nice") (make-word "" (make-posn 0 0)))

(define(generate-new-moving-word/text boolean s)
  (cond [(boolean=? #true boolean)(random-word s)]
        [else (make-word "" (make-posn 0 0))]))

; random-word: String -> W
; Makes a new word in random location.
(define (random-word s)
  (make-word s (make-posn (random GRID-WIDTH) 0)))

;dropfallingwords: LOW LOW->LOW
;Purpose: makes falling words drop and removes the words that stuck
(check-expect(dropfallingwords  (list (make-word "byebye" (make-posn 30 40))
                                      (make-word "byebyebyebyebye" (make-posn 1 30)))
                                (list  (make-word "byebyebyebyebye" (make-posn 30 40))))
             (cons (make-word "byebyebyebyebye" (make-posn 1 31))'()))


(define (dropfallingwords l1 l2)
  (map (lambda (x) (changeposny x l2))
       (filter (lambda (x) (not (or (reachbottomornot? x) (meetornot? x l2)))) l1)))


;changeposny: Word LOW-> Word
;Purpose: changes the posn-y of a word
(check-expect (changeposny (make-word "hello" (make-posn 3 2))
                           (list (make-word "byebye" (make-posn 30 3))
                                 (make-word "byebyebyebyebye" (make-posn 1 3))))
              (make-word "hello" (make-posn 3 2)))
(check-expect (changeposny (make-word "hello" (make-posn 3 2))
                           (list  (make-word "byebyebyebyebye" (make-posn 30 3))))
              (make-word "hello" (make-posn 3 3)))

(define (changeposny w low)
  (make-word (word-string w) (if (meetornot? w low) (word-position w)
                                 (make-posn (posn-x (word-position w))
                                            (+ 1 (posn-y (word-position w)))))))

;meetornot?:Word LOW-> Boolean
;Purpose: to check if the fallingword meets with the stuckwords
(check-expect (meetornot? (make-word "hello" (make-posn 3 2)) empty) #false)
(check-expect (meetornot? (make-word "" (make-posn 3 2)) (list (make-word "" (make-posn 3 2))))
              #false)
(check-expect (meetornot?  (make-word "hello" (make-posn 3 2))
                           (list (make-word "byebye" (make-posn 30 3))
                                 (make-word "byebyebyebyebye" (make-posn 1 3)))) #true)

(define (meetornot? w l)
  (ormap (lambda (x) (meetornotsingle x w)) l))


;meetornotsingle: Word Word -> Boolean
;Purpose: to check if two words meet
(check-expect (meetornotsingle (make-word "hello" (make-posn 3 2))
                               (make-word "bye" (make-posn 4 3))) #true)
(check-expect (meetornotsingle (make-word "hello" (make-posn 6 2))
                               (make-word "bye" (make-posn 20 2))) #false)
(check-expect (meetornotsingle (make-word "hello" (make-posn 3 2))
                               (make-word "bye" (make-posn 2 3))) #true)
(check-expect (meetornotsingle (make-word "hello" (make-posn 3 2))
                               (make-word "byebyebyebye" (make-posn 4 3))) #true)
(check-expect (meetornotsingle (make-word "hello" (make-posn 3 2))
                               (make-word "byebyebyebyebye" (make-posn 1 3))) #true)
(check-expect (meetornotsingle (make-word "hello" (make-posn 3 2))
                               (make-word "byebye" (make-posn 30 3))) #false)
 
(define (meetornotsingle w1 w2) 
  (and (= 1 (abs(- (posn-y (word-position w1)) (posn-y (word-position w2)))))
       (cond[(and(>= (posn-x (word-position w1)) (posn-x (word-position w2)))
                 (<= (posn-x (word-position w1)) (getendpoint w2))) #true]
            [(and (>= (getendpoint w1) (posn-x (word-position w2)))
                  (<= (getendpoint w1) (getendpoint w2))) #true]
            [(and (<= (posn-x (word-position w1)) (posn-x (word-position w2)))
                  (>= (getendpoint w1) (getendpoint w2))) #true]
            [else #false])))

;getendpoint : Word-> Number
;purpose: get the end point of a word
(check-expect (getendpoint(make-word "hello" (make-posn 20 20))) 24)

(define(getendpoint w)
  (- (+ (posn-x (word-position w)) (string-length (word-string w))) 1)) 


;stackbottomwords: LOW LOW-> LOW
;Purpose: stack the bottom words
(check-expect (stackbottomwords  (list (make-word "byebye" (make-posn 30 3))
                                       (make-word "byebyebyebyebye" (make-posn 1 2))) empty)
              empty)
(check-expect (stackbottomwords  (list (make-word "byebye" (make-posn 30 40))
                                       (make-word "byebyebyebyebye" (make-posn 1 40))) empty)
              (list (make-word "byebye" (make-posn 30 40))
                    (make-word "byebyebyebyebye" (make-posn 1 40))))


(define (stackbottomwords LOW1 LOW2)
    (append LOW2 (filter (lambda (x) (or (meetornot? x LOW2) (reachbottomornot? x))) LOW1)))
  

;reachbottomornot: Word ->Boolean
;Purpose: check if the word reach the bottom
(check-expect (reachbottomornot? (make-word "nice" (make-posn 20 20))) #false)
(check-expect (reachbottomornot? (make-word "nice" (make-posn 20 40))) #true)

(define (reachbottomornot? w)
  (if(= (posn-y (word-position w)) 40)
     #true
     #false))

;input-word: World Key -> World
;Purpose: input the words we are typing

(check-expect (input-word WORLD0 "ab") (make-world empty empty "ab" 0))
(check-expect (input-word world1 "\b") (make-world LOW1 LOW2 "Nic" 1))
(check-expect (input-word world1 "\r") (make-world(cons(make-word "Morning" (make-posn 11 11))
                                                       (cons(make-word "Good" (make-posn 10 10))'()))
                                                  (cons(make-word "Midterm"(make-posn 13 11))
                                                       (cons(make-word "Fundies"(make-posn 12 11))'()))""1))
(check-expect (input-word WORLD0 "1") WORLD0)

(define (input-word w k)
  [cond [(string-alphabetic? k)
         (make-world (world-falling-words w)
                     (world-bottom-words w) (string-append (world-typing-words w) k) (world-time w))]
        [(key=? "\b" k) (make-world (world-falling-words w)
                                    (world-bottom-words w)
                                    (implode (remove
                                              (string-ith (world-typing-words w)
                                                          (- (string-length (world-typing-words w)) 1))
                                              (explode (world-typing-words w))))
                                    (world-time w))]
        [(key=? "\r" k) (make-world (clearwordornot (world-typing-words w) (world-falling-words w))
                                    (world-bottom-words w)
                                    "" (world-time w))]
        [else w]])


;clearwordornot: String LOW-> LOW
;Purpose: checks if the input word equals one of the falling words

(check-expect (clearwordornot "byebye" (list (make-word "byebye" (make-posn 30 3))
                                             (make-word "byebyebyebyebye" (make-posn 1 2))))
              (cons (make-word "byebyebyebyebye" (make-posn 1 2)) empty))
(check-expect (clearwordornot "hello" (list (make-word "byebye" (make-posn 30 3))
                                            (make-word "byebyebyebyebye" (make-posn 1 2))))
              (list (make-word "byebye" (make-posn 30 3))
                    (make-word "byebyebyebyebye" (make-posn 1 2))))

(define (clearwordornot s low)
    (filter (lambda (x)(not (string=? s (word-string x)))) low))


; World Number-> Image
; Show the final scene of the game

(check-expect (show-score world1)
              (overlay (text (number->string (total-score frequency world1)) 80 'red) SCENE))

(define (show-score w)
  (overlay (text(number->string (total-score frequency w))  80 'red) SCENE))

;Number World -> Number
;calculate score of the game

(check-expect (total-score 0.2 world1) 5)

(define (total-score frequency w)
  (* (/ 1 frequency)  (world-time w)))

;game-over? World -> Boolean
;Is the game over?

(define (game-over2? world)
  (ormap touchtop (world-bottom-words world)))

;touch-top? Word -> Boolean
;Is a stacked word at the top of the screen
(define (touchtop word)
  (>= 5 (posn-y (word-position word)) ))


(define (main f)
  (big-bang WORLD0
            [to-draw draw-world]
            [on-tick update-world f]
            [on-key input-word]
            [stop-when game-over2? show-score]))

