#lang racket

(define sample-input '("forward 5" 
                       "down 5"
                       "forward 8"
                       "up 3"
                       "down 8"
                       "forward 2"))
;; this function can be abstracted to a shared file
(define input
  (port->string (open-input-file "2021/input/day02.txt") #:close? #t))

(define (line-to-pair line)
  (let* ([split (string-split line " ")]
         [int (string->number (car (cdr split)))])
    (cons (car split) int)))

(define (parse-input input)
  (map (lambda (l) (line-to-pair l)) input))

(define (part1 commands coords)
  (let ([horizontal (car coords)]
         [depth (cdr coords)])
    (cond
      [(empty? commands)
       coords]
      [(equal? (car (car commands)) "forward")
       (part1 
         (cdr commands)
         (cons (+ horizontal (cdr (car commands))) depth))]
      [(equal? (car (car commands)) "down") 
       (part1 
         (cdr commands)
         (cons horizontal (+ depth (cdr (car commands)))))]
      [(equal? (car (car commands)) "up") 
       (part1 
         (cdr commands)
         (cons horizontal (- depth (cdr (car commands)))))]
      [else (raise "Shouldn't get here")])))

(define sample-results (part1 (parse-input sample-input) '(0 . 0)))
(* (car sample-results) (cdr sample-results))

(define results (part1 (parse-input (string-split input "\n")) '(0 . 0)))
(* (car results) (cdr results))

