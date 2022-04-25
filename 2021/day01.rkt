#lang racket

(define sample-input '(199 200 208 210 200 207 240 269 260 263))
(define input
  (port->string (open-input-file "2021/input/day01.txt") #:close? #t))

(define (part1 queue processed)
  (cond
    [(empty? queue) 0]
    [(empty? processed) (part1 (cdr queue) (list (car queue)))]
    [(> (car queue) (car processed)) (+ 1 (part1 (cdr queue) (cons (car queue) processed)))]
    [else (+ 0 (part1 (cdr queue) (cons (car queue) processed)))]))

(part1 sample-input '())
(part1 (map (lambda (n) (string->number n)) (string-split input "\n")) '())


;; could speed this up by storing the sums in Map
(define (part2 queue index)
  (cond
    [(>= (+ index 3) (vector-length queue))
     0]
    [(> (+ (vector-ref queue (+ index 1))
           (vector-ref queue (+ index 2))
           (vector-ref queue (+ index 3)))
        (+ (vector-ref queue index)
           (vector-ref queue (+ index 1))
           (vector-ref queue (+ index 2))))
     (+ 1 (part2 queue (+ 1 index)))]
    [else (+ 0 (part2 queue (+ 1 index)))]))

(part2 (list->vector sample-input) 0)
(part2
  (list->vector (map
                  (lambda (n) (string->number n))
                  (string-split input "\n")))
  0)
