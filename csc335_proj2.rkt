
"CSC335 project --- stock analysis"
"Alan Lau, Rupesh Basnet, Bohan Chen"


; random number generator
; reference: https://stackoverflow.com/questions/14674165/scheme-generate-random

(define random
  (let ((a 69069) (c 1) (m (expt 2 32)) (seed 19380110))
    (lambda new-seed
      (if (pair? new-seed)
          (set! seed (car new-seed))
          (set! seed (* 1.0 (modulo (+ (* seed a) c) m))))
      (/ seed m))))

(define (randint . args)
  (cond ((= (length args) 1)
          (floor (* (random) (car args))))
        ((= (length args) 2)
          (+ (car args) (floor (* (random) (- (cadr args) (car args))))))
        (else (error 'randint "usage: (randint [lo] hi)"))))


; reference: materials from prof Trouger

;(define (cons-stream a b)
;  (cons a (delay b)))
;
;(define (stream-car stream)
;  (car stream))
;
;(define (stream-cdr stream)
;  (force (cdr stream)))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (force delayed-object) (delayed-object))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())

(define stream-null? null?)
; -------------------------------------------------------------------------------
;rounding function

(define (my-round number precision)
  (let ((a (round (* number (expt 10 precision)))))
    (* a (expt 10 (* -1 precision)))))
;---------------------------------------------------

; All the variables needed
; a = stock A
; b = stock B
; c = stock C
; count = to find the time
;(define a (cons-stream 10 '()))
;(define b (cons-stream 10 '()))
;(define c (cons-stream 7 '()))

(define count 0)

; some of the probabilities, defined outside for easy modification the probabilities
;(define p-for-a-to-drop 6)
(define p-a-drop1% 5)
;(define p-a-drop2% 3)
(define p-a-drop3% 2)
(define p-for-a-to-rise 4)
(define p-a-rise1% 5)
;(define p-a-rise2% 3)
(define p-a-rise3% 2)

;(define p-for-b-to-drop 4)
(define p-b-drop1% 5)
;(define p-b-drop2% 3)
(define p-b-drop3% 2)
(define p-for-b-to-rise 6)
(define p-b-rise1% 5)
;(define p-b-rise2% 3)
(define p-b-rise3% 2)

;(define p-for-c-to-drop 5)
(define p-c-drop1% 5)
;(define p-c-drop2% 3)
(define p-c-drop3% 2)
(define p-for-c-to-rise 8)
(define p-c-rise1% 5)
;(define p-c-rise2% 3)
(define p-c-rise3% 2)
; ------------------------------------------------------------------------------------
;(define temp 0)
;
;(define s-rising
;  (lambda (stock)
;    (cond ((eq? stock 'a) (begin
;                           (set! temp (stream-car a))
;                           (cond ((< (randint 0 10) p-a-rise3%) (cons-stream (* temp 1.03) (cons-stream temp a)))
;                                 ((>= (randint 0 10) p-a-rise1%) (cons-stream (* temp 1.01) (cons-stream temp a)))
;                                 (else (cons-stream (* temp 1.02) (cons-stream temp a))))))
;          ((eq? stock 'b) (begin
;                            (set! temp (stream-car b))
;                            (cond ((< (randint 0 10) p-b-rise3%) (cons-stream (* temp 1.03) (cons-stream temp b)))
;                                  ((>= (randint 0 10) p-b-rise1%) (cons-stream (* temp 1.01) (cons-stream temp b)))
;                                  (else (cons-stream (* temp 1.02) (cons-stream temp b))))))
;          (else (begin
;                  (set! temp (stream-car c))
;                  (cond ((< (randint 0 10) p-c-rise3%) (cons-stream (* temp 1.03) (cons-stream temp c)))
;                        ((>= (randint 0 10) p-c-rise1%) (cons-stream (* temp 1.01) (cons-stream temp c)))
;                        (else (cons-stream (* temp 1.02) (cons-stream temp b)))))))))
;
;(define s-dropping
;  (lambda (stock)
;    (cond ((eq? stock 'a) (begin
;                           (set! temp (stream-car a))
;                           (cond ((< (randint 0 10) p-a-drop3%) (cons-stream (* temp 0.97) (cons-stream temp a)))
;                                 ((>= (randint 0 10) p-a-drop1%) (cons-stream (* temp 0.99) (cons-stream temp a)))
;                                 (else (cons-stream (* temp 0.98) (cons-stream temp a))))))
;          ((eq? stock 'b) (begin
;                            (set! temp (stream-car b))
;                            (cond ((< (randint 0 10) p-b-drop3%) (cons-stream (* temp 0.97) (cons-stream temp b)))
;                                  ((>= (randint 0 10) p-b-drop1%) (cons-stream (* temp 0.99) (cons-stream temp b)))
;                                  (else (cons-stream (* temp 0.98) (cons-stream temp b))))))
;          (else (begin
;                  (set! temp (stream-car c))
;                  (cond ((< (randint 0 10) p-c-drop3%) (cons-stream (* temp 0.97) (cons-stream temp c)))
;                        ((>= (randint 0 10) p-c-drop1%) (cons-stream (* temp 0.99) (cons-stream temp c)))
;                        (else (cons-stream (* temp 0.98) (cons-stream temp c)))))))))
;
;    
;
;(define reading-data
;  (lambda arg
;    (begin
;      (set! count (+ count 1)) ;; each call will increment the count by 1
;      (set! a (if (< (randint 0 10) p-for-a-to-rise)
;                  ; if a is rising, determine how much it will rise
;                  (s-rising 'a)
;                  (s-dropping 'a)))
;      (set! b (if (<  (randint 0 10) p-for-b-to-rise)
;                  (s-rising 'b)
;                  (s-dropping 'b)))
;      (set! c (if (<  (randint 0 10) p-for-c-to-rise)
;                  (s-rising 'c)
;                  (s-dropping 'c)))
;      )))
;
;
;(define (run n) ; this will show the price of stocks
;  (define (iter n)
;    (if (= n 0) (display "stop")
;        (begin
;          (reading-data)
;          (display count)
;          (newline)
;          (display (my-round (stream-car a) 2))
;          (display "                ")
;          (display (my-round (stream-car b) 2))
;          (display "                ")
;          (display (my-round (stream-car c) 2))
;          (newline)
;          (iter (- n 1)))))
;  (iter n))
;;(run 100)
;
;(define find-event
;  (lambda arg
;    (if (< (stream-car a) (stream-car b) (stream-car c))
;        count
;        (begin
;          (reading-data)
;          (find-event)))))
;
;;(find-event) ; if this return the count in the unit of hour we can simply divide it by 6.5 we will get the day
;(newline)
;(display "this event occur at: ")
;(display (my-round (/ (find-event) 6.5) 2))
;(display " days ago")

; ---------------------------------------------------------
; modified code

(define s-rising
  (lambda (stock price)
    (cond ((eq? stock 'a) (cond ((< (randint 0 10) p-a-rise3%) (* price 1.03)) ; return the new price based on the previous price
                                ((>= (randint 0 10) p-a-rise1%) (* price 1.01))
                                (else (* price 1.02))))
          ((eq? stock 'b) (cond ((< (randint 0 10) p-b-rise3%) (* price 1.03))
                                ((>= (randint 0 10) p-b-rise1%) (* price 1.01))
                                (else (* price 1.02))))
          (else (cond ((< (randint 0 10) p-c-rise3%) (* price 1.03))
                      ((>= (randint 0 10) p-c-rise1%) (* price 1.01))
                      (else (* price 1.02)))))))

(define s-dropping
  (lambda (stock price)
    (cond ((eq? stock 'a) (cond ((< (randint 0 10) p-a-drop3%) (* price 0.97)) ; return the new price based on the previous price
                                ((>= (randint 0 10) p-a-drop1%) (* price 0.99))
                                (else (* price 0.98))))
          ((eq? stock 'b) (cond ((< (randint 0 10) p-b-drop3%) (* price 0.97))
                                ((>= (randint 0 10) p-b-drop1%) (* price 0.99))
                                (else (* price 0.98))))
          (else (cond ((< (randint 0 10) p-c-drop3%) (* price 0.97))
                        ((>= (randint 0 10) p-c-drop1%) (* price 0.99))
                        (else (* price 0.98)))))))


(define reading-data
  (lambda (name price)
    (cond ((eq? name 'a) (if (< (randint 0 10) p-for-a-to-rise)
                  ; if a is rising, determine how much it will rise
                             (s-rising 'a price)
                             (s-dropping 'a price)))
          ((eq? name 'b) (if (<  (randint 0 10) p-for-b-to-rise)
                             (s-rising 'b price)
                             (s-dropping 'b price)))
          ((eq? name 'c) (if (<  (randint 0 10) p-for-c-to-rise)
                             (s-rising 'c price)
                             (s-dropping 'c price)))
          )))

(define find-event
  (lambda (a b c)
    (set! count (+ count 1))
    (if (< (stream-car a) (stream-car b) (stream-car c))
        count
        (find-event (stream-cdr a) (stream-cdr b) (stream-cdr c)))))
; count is a 'timer' in hour


(define (stock name initial_price)
  (cons-stream initial_price (stock name (reading-data name initial_price))))

;now we need to redefine our a b c

(define a (stock 'a 10)) ; a will be stock named 'a with initial price 10
(define b (stock 'b 10))
(define c (stock 'c 7))

(newline)
(display "this event occur at: ")
(display (my-round (/ (find-event a b c) 6.5) 2))
(display " days ago")

(#%require plot)

; SaveToList for testing and for plot
(define (saveToList stream numOfIntervals)
  (define (aux stream result num)
    (cond ((= num 0) result)
        (else (begin (aux (stream-cdr stream) (cons (stream-car stream) result) (- num 1))))))
  (aux stream '() numOfIntervals))

; Enumerator for the time interval
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define timeVal (enumerate-interval 1 100))
(newline)

(plot (lines (map vector timeVal (reverse (saveToList a 100)))))
(plot (lines (map vector timeVal (reverse (saveToList b 100)))))
(plot (lines (map vector timeVal (reverse (saveToList c 100)))))






