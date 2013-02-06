#! /usr/bin/env racket

#lang racket

(require "inotify.rkt"
         "box-print.rkt"
         racket/date
         racket/sandbox)

(provide (all-defined-out))

(define (diligent-tester path)
  (unless (path-string? path)
    (raise-argument-error 'diligent-tester "path-string?" path))
  (new diretory-watcher%
       [path path]
       [recursive? #t]
       [callback (lambda (name mask)
                   (match (path->string name)
                     [(regexp #rx".rkt$")
                      (box-print #:center? #t
                       (date->string (current-date))
                       (format "~a \"~a\"" mask name))
                      (parameterize ([current-directory path])
                        (system "racket tests.rkt"))]
                     [else #t]))]))

(module* main #f
    (define args (current-command-line-arguments))
    (cond [(= 1 (vector-length args))
           (define p (vector-ref args 0))
           (diligent-tester p)
           (semaphore-wait (make-semaphore))]
          [else
           (print "usage: this-script path")]))
