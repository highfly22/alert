#lang racket

(require "inotify.rkt")

(provide (all-defined-out))

(begin-for-syntax
 (define (check-ids stx forms)
   (for-each
    (lambda (form)
      (unless (identifier? form)
        (raise-syntax-error #f
                            "not an identifier"
                            stx
                            form)))
    (syntax->list forms))))

(define-syntax watch-rules
  (lambda (stx)
    (syntax-case stx ()
      [(_ (p m) base (pattern body ...) ...)
       (check-ids stx #'(p m))
       #'(begin
           (unless (path-string? base)
             (raise-argument-error 'watch-rules "base is not path-string?" base))
           (new diretory-watcher%
                [path base]
                [recursive? #t]
                [callback
                 (lambda (name mask)
                   (define p name)
                   (define m mask)
                   (match (path->string name)
                     [(pregexp pattern)
                      body ...] ...
                     [else #t]))]))]
      [(_ (p) base (pattern body ...) ...)
       #`(watch-rules (p ___) base (pattern body ...) ...)]
      [(_ base (pattern body ...) ...)
       #`(watch-rules (__ ___) base (pattern body ...) ...)])))
                                    
