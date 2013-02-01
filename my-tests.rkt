#lang racket

(require "inotify.rkt"
         rackunit)

(define i (_inotify_init))

(define f (_inotify_add_watch i "/home/haiwei/tmp/" (bitwise-ior #x1 #x2 #x20 #x100 #x200)))

(thread (thunk
         (system "touch /home/haiwei/tmp/a")
         (sleep 1)
         (system "touch /home/haiwei/tmp/a")))

(define t (thread (thunk
                     (displayln "reading events")
                     (define l (read-events i))
                     (check-equal? l (list #s(inotify-event 1 32 0 16 "a"))))))

(thread-wait t)







