#lang racket

(define sample-input '("forward 5" 
                       "down 5"
                       "forward 8"
                       "up 3"
                       "down 8"
                       "forward 2"))

(define (line-to-pair line)
  (let* ([split (string-split line " ")]
         [int (string->number (car (cdr split)))])
    (cons (car split) int)))

(define (part1 commands coords)
  (let* ([command (car commands)]
         [direction (car command)]
         [value (cdr command)]
         [horizontal (car coords)]
         [depth (cdr coords)])
    (cond
      [(empty? commands)
       coords]
      [(equal? direction "forward")
       (part1 
         (cdr commands)
         (cons (+ horizontal value) depth))]
      [(equal? direction "down") 
       (part1 
         (cdr commands)
         (cons horizontal (- depth value)))]
      [(equal? direction "up") 
       (part1 
         (cdr commands)
         (cons horizontal (+ depth value)))]
      [else (raise "Shouldn't get here")])))

(define parsed-input 
  (map 
    (lambda (l) (line-to-pair l))
    sample-input))

(part1 parsed-input '(0 . 0))
