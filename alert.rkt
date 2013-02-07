#lang racket

(require "inotify.rkt")

(provide (all-defined-out))

(begin-for-syntax
 (define (check-id stx id)
   (unless (identifier? id)
     (raise-syntax-error #f
                         "not an identifier"
                         stx
                         id))))

(define-syntax watch-rules
  (lambda (stx)
    (syntax-case stx ()
      [(_ (id) base (pattern body ...) ...)
       (check-id stx #'id)
       #'(begin
           (unless (path-string? base)
             (raise-argument-error 'watch-rules "base is not path-string?" base))
           (new diretory-watcher%
                [path base]
                [recursive? #t]
                [callback
                 (lambda (name mask)
                   (define id name)
                   (match (path->string name)
                     [(pregexp pattern)
                      body ...] ...
                     [else #t]))]))])))
                                    
