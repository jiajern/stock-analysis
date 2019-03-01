; csc335 project --- stock analysis

(#%require plot)

; random number generator
; reference: https://stackoverflow.com/questions/14674165/scheme-generate-random

(define random
  (let ((a 69069) (c 1) (m (expt 2.0 32)) (seed 19380110))
    (lambda new-seed
      (if (pair? new-seed)
          (set! seed (car new-seed))
          (set! seed (* 1 (modulo (+ (* seed a) c) m))))
      (/ seed m))))

;(random)
;(random 5339)

(define (randint . args)
  (cond ((= (length args) 1)
          (floor (* (random) (car args))))
        ((= (length args) 2)
          (+ (car args) (floor (* (random) (- (cadr args) (car args))))))
        (else (error 'randint "usage: (randint [lo] hi)"))))

;(randint 0 10)

; reference: textbook

(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))
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
(define a (cons-stream 10 '()))
(define b (cons-stream 10 '()))
(define c (cons-stream 7 '()))
; All volume variables
(define vol-a (cons-stream 10000 '()))
(define vol-b (cons-stream 10000 '()))
(define vol-c (cons-stream 7000 '()))

(define count 0)
(define vol-count 0)


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
(define temp 0)

(define s-rising
  (lambda (stock)
    (cond ((eq? stock 'a) (begin
                           (set! temp (stream-car a))
                           (cond ((< (randint 0 10) p-a-rise3%) (cons-stream (* temp 1.03) (cons-stream temp a)))
                                 ((>= (randint 0 10) p-a-rise1%) (cons-stream (* temp 1.01) (cons-stream temp a)))
                                 (else (cons-stream (* temp 1.02) (cons-stream temp a))))))
          ((eq? stock 'b) (begin
                            (set! temp (stream-car b))
                            (cond ((< (randint 0 10) p-b-rise3%) (cons-stream (* temp 1.03) (cons-stream temp b)))
                                  ((>= (randint 0 10) p-b-rise1%) (cons-stream (* temp 1.01) (cons-stream temp b)))
                                  (else (cons-stream (* temp 1.02) (cons-stream temp b))))))
          (else (begin
                  (set! temp (stream-car c))
                  (cond ((< (randint 0 10) p-c-rise3%) (cons-stream (* temp 1.03) (cons-stream temp c)))
                        ((>= (randint 0 10) p-c-rise1%) (cons-stream (* temp 1.01) (cons-stream temp c)))
                        (else (cons-stream (* temp 1.02) (cons-stream temp c)))))))))

(define s-dropping
  (lambda (stock)
    (cond ((eq? stock 'a) (begin
                           (set! temp (stream-car a))
                           (cond ((< (randint 0 10) p-a-drop3%) (cons-stream (* temp 0.97) (cons-stream temp a)))
                                 ((>= (randint 0 10) p-a-drop1%) (cons-stream (* temp 0.99) (cons-stream temp a)))
                                 (else (cons-stream (* temp 0.98) (cons-stream temp a))))))
          ((eq? stock 'b) (begin
                            (set! temp (stream-car b))
                            (cond ((< (randint 0 10) p-b-drop3%) (cons-stream (* temp 0.97) (cons-stream temp b)))
                                  ((>= (randint 0 10) p-b-drop1%) (cons-stream (* temp 0.99) (cons-stream temp b)))
                                  (else (cons-stream (* temp 0.98) (cons-stream temp b))))))
          (else (begin
                  (set! temp (stream-car c))
                  (cond ((< (randint 0 10) p-c-drop3%) (cons-stream (* temp 0.97) (cons-stream temp c)))
                        ((>= (randint 0 10) p-c-drop1%) (cons-stream (* temp 0.99) (cons-stream temp c)))
                        (else (cons-stream (* temp 0.98) (cons-stream temp c)))))))))

(define vol-rising
  (lambda (volume)
    (cond ((eq? volume 'vol-a) (begin
                           (set! temp (stream-car vol-a))
                           (cond ((< (randint 0 10) p-a-rise3%) (cons-stream (* temp 1.03) (cons-stream temp vol-a)))
                                 ((>= (randint 0 10) p-a-rise1%) (cons-stream (* temp 1.01) (cons-stream temp vol-a)))
                                 (else (cons-stream (* temp 1.02) (cons-stream temp vol-a))))))
          ((eq? volume 'vol-b) (begin
                            (set! temp (stream-car vol-b))
                            (cond ((< (randint 0 10) p-b-rise3%) (cons-stream (* temp 1.03) (cons-stream temp vol-b)))
                                  ((>= (randint 0 10) p-b-rise1%) (cons-stream (* temp 1.01) (cons-stream temp vol-b)))
                                  (else (cons-stream (* temp 1.02) (cons-stream temp vol-b))))))
          (else (begin
                  (set! temp (stream-car vol-c))
                  (cond ((< (randint 0 10) p-c-rise3%) (cons-stream (* temp 1.03) (cons-stream temp vol-c)))
                        ((>= (randint 0 10) p-c-rise1%) (cons-stream (* temp 1.01) (cons-stream temp vol-c)))
                        (else (cons-stream (* temp 1.02) (cons-stream temp vol-c)))))))))

(define vol-dropping
  (lambda (volume)
    (cond ((eq? volume 'vol-a) (begin
                           (set! temp (stream-car vol-a))
                           (cond ((< (randint 0 10) p-a-drop3%) (cons-stream (* temp 0.97) (cons-stream temp vol-a)))
                                 ((>= (randint 0 10) p-a-drop1%) (cons-stream (* temp 0.99) (cons-stream temp vol-a)))
                                 (else (cons-stream (* temp 0.98) (cons-stream temp vol-a))))))
          ((eq? volume 'vol-b) (begin
                            (set! temp (stream-car vol-b))
                            (cond ((< (randint 0 10) p-b-drop3%) (cons-stream (* temp 0.97) (cons-stream temp vol-b)))
                                  ((>= (randint 0 10) p-b-drop1%) (cons-stream (* temp 0.99) (cons-stream temp vol-b)))
                                  (else (cons-stream (* temp 0.98) (cons-stream temp vol-b))))))
          (else (begin
                  (set! temp (stream-car vol-c))
                  (cond ((< (randint 0 10) p-c-drop3%) (cons-stream (* temp 0.97) (cons-stream temp vol-c)))
                        ((>= (randint 0 10) p-c-drop1%) (cons-stream (* temp 0.99) (cons-stream temp vol-c)))
                        (else (cons-stream (* temp 0.98) (cons-stream temp vol-c)))))))))

(define reading-data
  (lambda arg
    (begin
      (set! count (+ count 1)) ;; each call will increment the count by 1
      (set! a (if (< (randint 0 10) p-for-a-to-rise)
                  ; if a is rising, determine how much it will rise
                  (s-rising 'a)
                  (s-dropping 'a)))
      (set! b (if (< (randint 0 10) p-for-b-to-rise)
                  (s-rising 'b)
                  (s-dropping 'b)))
      (set! c (if (< (randint 0 10) p-for-c-to-rise)
                  (s-rising 'c)
                  (s-dropping 'c)))
      )))

(define reading-vol-data
  (lambda arg
    (begin
      (set! vol-count (+ count 1)) ;; each call will increment the count by 1
      (set! vol-a (if (< (randint 0 10) p-for-a-to-rise)
                  ; if a is rising, determine how much it will rise
                  (vol-rising 'vol-a)
                  (vol-dropping 'vol-a)))
      (set! vol-b (if (<  (randint 0 10) p-for-b-to-rise)
                  (vol-rising 'vol-b)
                  (vol-dropping 'vol-b)))
      (set! vol-c (if (<  (randint 0 10) p-for-c-to-rise)
                  (vol-rising 'vol-c)
                  (vol-dropping 'vol-c)))
      )))


(define (run n) ; this will show the price and the volume of stocks
  (define (iter n)
    (if (= n 0) (display "stop")
        (begin
          (reading-data)
          (reading-vol-data)
          (display count)
          (newline)
          (display (my-round (stream-car a) 2))
          (display "                ")
          (display (my-round (stream-car b) 2))
          (display "                ")
          (display (my-round (stream-car c) 2))
          (newline)
          (display (my-round (stream-car vol-a) 2))
          (display "                ")
          (display (my-round (stream-car vol-a) 2))
          (display "                ")
          (display (my-round (stream-car vol-a) 2))
          (newline)
          (iter (- n 1)))))
  (iter n))
(run 100)

(define find-event
  (lambda arg
    (if (< (stream-car a) (stream-car b) (stream-car c))
        count 
        (begin
          (reading-data)
          (find-event)))))

(find-event) ; if this return the count in the unit of hour we can simply divide it by 6.5 we will get the day
(newline)
(display "this event occur at: ")
(display (my-round (/ (find-event) 6.5) 2))
(display " days ago")
    

; testing the plot function
;(plot (function sin (- pi) pi #:label "y = sin(x)"))

; SaveToList for testing and for plot
(define (saveToList stream numOfIntervals)
  (define (aux stream result num)
    (cond ((= num 0) result)
        (else (begin (aux (stream-cdr stream) (cons (stream-car stream) result) (- num 1))))))
  (aux stream '() numOfIntervals))

; Limit printer stream
(define (print-stream-limit stream limit)
  (define (aux rest-stream limit)
    (cond ((stream-null? rest-stream) (display "]"))
          ((= limit 0) (display " continued ...]"))
          (else (display (stream-car rest-stream))
                (newline)
                (aux (stream-cdr rest-stream) (- limit 1)))))
  (display "[")
  (aux stream limit))

; Enumerator for the time interval
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

; stream mapping for one stream
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

; creating a pair of stock and volume
(define (stock-vol-pairs stock vol)
  (cons-stream (list (stream-car stock) (stream-car vol))
               (stock-vol-pairs (stream-cdr stock) (stream-cdr vol))))

; defining the empty stream
(define the-empty-stream '())

(define stream-null? null?)

; stream-map that can receive number of streams as a parameter
(define (stream-map proc . args)
  (if (stream-null? (car args))
      the-empty-stream
      (cons-stream (apply proc (map stream-car args))
                   (apply stream-map (cons proc (map stream-cdr args))))))


; for pairing the two streams here stock and volume as a new stream
(define (test-streammap stock vol)
  (stream-map cons stock vol))

; proc to get the cdr of the pair of the combined stream
(define (test-cdr-stream-map combinedStream)
  (stream-map (lambda (x) (cdr x)) combinedStream))

; proc to get the car of the pair of the combined stream
(define (test-car-stream-map combinedStream)
  (stream-map (lambda (x) (car x)) combinedStream))

; proc to multiply the car of the pair with the cdr of the pair of the stream
(define (test-mul-stream-pairs combinedStream)
  (stream-map * (test-car-stream-map combinedStream) (test-cdr-stream-map combinedStream)))
  
; for pairing the volume and the volumeXclose
(define (test-streammap-calc combinedStream)
  (stream-map cons
               (test-cdr-stream-map combinedStream)
               (test-mul-stream-pairs combinedStream)))

; accumulate for each 10 bins
; grab ten in a stream. accumulate that stream and put it on another stream
; increment on that stream and keep on accumulating
; boiler plate used was of moving-average-calculator from
; https://rosettacode.org/wiki/Averages/Simple_moving_average

(define (moving-sum-calculator binSize . nums)
  (lambda (num)
    (set! nums ; set! the value of num to
          (cons num
              (if (= (length nums) binSize) ; if length of the nums is equal to the binSize we return  
                  (reverse (cdr (reverse nums)))
                  nums)))
  (apply + nums))) ; apply the + to all the numbers 

;----------------------------------------------------------
; Creating the bins for each stream necessary for computation
(define sumStreamVolA_bins (moving-sum-calculator 10))
(define sumStreamVolXStockA_bins (moving-sum-calculator 10))

(define sumStreamVolB_bins (moving-sum-calculator 10))
(define sumStreamVolXStockB_bins (moving-sum-calculator 10))

(define sumStreamVolC_bins (moving-sum-calculator 10))
(define sumStreamVolXStockC_bins (moving-sum-calculator 10))

;----------------------------------------------------------
; Get all the individual streams from the combined streams
(define testVolA
  (test-car-stream-map (test-streammap-calc (test-streammap a vol-a))))

(define testVolXStockA
  (test-cdr-stream-map (test-streammap-calc (test-streammap a vol-a))))

(define testVolB
  (test-car-stream-map (test-streammap-calc (test-streammap b vol-b))))

(define testVolXStockB
  (test-cdr-stream-map (test-streammap-calc (test-streammap b vol-b))))

(define testVolC
  (test-car-stream-map (test-streammap-calc (test-streammap c vol-c))))

(define testVolXStockC
  (test-cdr-stream-map (test-streammap-calc (test-streammap c vol-c))))

;----------------------------------------------------------
; Use the running sum proc with bins and the stream to get the
; new stream of running sum of 10 intervals
(define sumStreamVolA
  (stream-map sumStreamVolA_bins testVolA))

(define sumStreamVolXStockA
  (stream-map sumStreamVolXStockA_bins testVolXStockA))

(define sumStreamVolB
  (stream-map sumStreamVolB_bins testVolB))

(define sumStreamVolXStockB
  (stream-map sumStreamVolXStockB_bins testVolXStockB))

(define sumStreamVolC
  (stream-map sumStreamVolC_bins testVolC))

(define sumStreamVolXStockC
  (stream-map sumStreamVolXStockC_bins testVolXStockC))

; -------------------------------------------------------
; Finally calculate the running avgerage for each stock stream

(define running-avg-a
  (stream-map / sumStreamVolXStockA sumStreamVolA))

(define running-avg-b
  (stream-map / sumStreamVolXStockB sumStreamVolB))

(define running-avg-c
  (stream-map / sumStreamVolXStockC sumStreamVolC))

;(test-streammap-calc (test-streammap a vol-a)) -- the car will have the vol and the cdr will have the vol X stock
;((7267.418777090884 . 63843.79278039775) . #<promise>)

(define timeVal (enumerate-interval 1 100))
(newline)


; --------------------------------------------------
; Plots
; Plotting the Stock Prices
(plot (lines (map vector timeVal (saveToList a 100))))
(plot (lines (map vector timeVal (saveToList b 100))))
(plot (lines (map vector timeVal (saveToList c 100))))

; Plotting the Running-avgs
(plot (lines (map vector timeVal (saveToList running-avg-a 100))))
(plot (lines (map vector timeVal (saveToList running-avg-b 100))))
(plot (lines (map vector timeVal (saveToList running-avg-c 100))))

; Plotting the Running-avgs with the stock price
(plot (lines-interval (map vector timeVal (saveToList running-avg-a 100)) (map vector timeVal (saveToList a 100))))
(plot (lines-interval (map vector timeVal (saveToList running-avg-b 100)) (map vector timeVal (saveToList b 100))))
(plot (lines-interval (map vector timeVal (saveToList running-avg-c 100)) (map vector timeVal (saveToList c 100))))

;-----------------------------------------------------------
; Bargain index

; Save a new stream the bargain-index of the bargains where the vwap > price

(define (bargain-index stock-stream running-avg-stream)
  (cond
    ((or (stream-null? stock-stream) (stream-null? running-avg-stream)) the-empty-stream)
    ((> (stream-car running-avg-stream) (stream-car stock-stream))
      (cons-stream (* (stream-car stock-stream) (exp (- (stream-car stock-stream) (stream-car running-avg-stream))))
                   (bargain-index (stream-cdr stock-stream) (stream-cdr running-avg-stream))))
    (else (cons-stream 0
                   (bargain-index (stream-cdr stock-stream) (stream-cdr running-avg-stream))))))


; ----------------------------------------------------------
; Plots for the bargainindex
(plot (discrete-histogram (map vector timeVal (saveToList (bargain-index a running-avg-a) 100))) #:width 600)
(plot (discrete-histogram (map vector timeVal (saveToList (bargain-index b running-avg-b) 100))) #:width 600)
(plot (discrete-histogram (map vector timeVal (saveToList (bargain-index c running-avg-c) 100))) #:width 600)













