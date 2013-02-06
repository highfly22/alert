#lang racket

(provide (all-defined-out))

(define box-print-columns (make-parameter 68))

(define (box-print #:center? [center? #f] . strings)
  (define max-col (apply max (map string-length strings)))
  (define truncate? (> max-col (box-print-columns)))
  (define col (min max-col (box-print-columns)))

  (define (print-header)
    (displayln (make-string (+ col 4) #\-)))

  (define (print-line s)
    (define len (string-length s))
    (cond [center?
           (define format (if (even? len)
                              "| ~a~a~a |\n"
                              "| ~a~a~a  |\n"))
           (define padding (make-string (quotient (- col len)2) #\ ))
           (printf format padding s padding)]
          [else
           (define padding (make-string (- col len) #\ ))
           (printf "| ~a~a |\n" s padding)]))
  (define (string-tear-up str col)
    (define len (string-length str))
    (for/list ([i (in-range (ceiling (/ len col)))])
      (define start (* i col))
      (define end (min (+ start col) len))
      (substring str start end)))

  (print-header)
  (map (lambda (i)
         (cond
          [(and truncate?
                (> (string-length i) col))
           (for ([s (string-tear-up i col)])
             (print-line s))]
          [else (print-line i)]))
       strings)
  (print-header))

