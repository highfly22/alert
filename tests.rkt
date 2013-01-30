#lang racket

(require "inotify.rkt"
         tests/eli-tester)

(define i (_inotify_init))

(define f (_inotify_add_watch i "/home/haiwei/tmp" (bitwise-ior #x100 #x200)))

(define l (read-inotify-events i))

(displayln l)



