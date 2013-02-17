#! /usr/bin/env racket

#lang racket

(require ;; "inotify.rkt"
         "alert.rkt"
         "box-print.rkt"
         racket/date
         racket/sandbox)

(provide (all-defined-out))

(define args (current-command-line-arguments))
(module* main #f
  (cond [(= 1 (vector-length args))
         (define base (vector-ref args 0))
         (watch-rules (path mask) base
                      [#px"\\.rkt$"
                          (box-print #:center? #t
                                     (date->string (current-date))
                                     (format "Notify: \"~a\"" path))
                          (parameterize ([current-directory base])
                            (system "racket tests.rkt"))])
         (semaphore-wait (make-semaphore))]
        [else
         (print "usage: this-script path")]))
