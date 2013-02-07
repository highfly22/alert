#! /usr/bin/env racket

#lang racket

(require ;; "inotify.rkt"
         "alert.rkt"
         "box-print.rkt"
         racket/date
         racket/sandbox)

(provide (all-defined-out))

(define (diligent-tester base)
  (watch-rules (path) base
               [#px"\\.rkt$"
                   (box-print #:center? #t
                              (date->string (current-date))
                              (format "Notify: \"~a\"" path))
                   (parameterize ([current-directory base])
                     (system "racket tests.rkt"))]))

(module* main #f
    (define args (current-command-line-arguments))
    (cond [(= 1 (vector-length args))
           (define p (vector-ref args 0))
           (diligent-tester p)
           (semaphore-wait (make-semaphore))]
          [else
           (print "usage: this-script path")]))
