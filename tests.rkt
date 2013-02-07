#lang racket

(require "inotify.rkt"
         "box-print.rkt"
         "alert.rkt"
         racket/file
         rackunit
         rackunit/text-ui
         racket/sandbox)

(define (system/print str)
  (printf "System:) ~a\n" str)
  (system str))

(define watcher-tests
  (test-suite
   "inotify tests"
   (test-case
    "An simple touch test"
    (call-with-limits
     10 10
     (thunk
      (define dir (make-temporary-file "rkttmp~a" 'directory))
      (define dir2 (make-temporary-file "rkttmp~a" 'directory))
      (system/print (format "mkdir -p ~a/a/b/c/d" dir2))

      (define ch (make-channel))

      (define watcher (new diretory-watcher%
                           [path dir]
                           [callback (lambda (name mask)
                                       (print (list name mask)) (newline)
                                       (channel-put ch (list (path->string name) mask)))]
                           [recursive? #t]))

      (system/print (format "touch ~a/1" dir))
      (check-equal? (channel-get ch)
                    (list (format "~a/1" dir) '(IN_CREATE)))

      (system/print (format "mkdir ~a/d" dir))
      (check-equal? (channel-get ch)
                    (list (format "~a/d" dir) '(IN_CREATE IN_ISDIR)))

      (system/print (format "mkdir ~a/d/.git" dir))

      (system/print (format "touch ~a/d/1" dir))
      (check-equal? (channel-get ch)
                    (list (format "~a/d/1" dir) '(IN_CREATE)))

      (system/print (format "rm -rf ~a/d" dir))
      (check-equal? (channel-get ch)
                    (list (format "~a/d/1" dir) '(IN_DELETE)))

      (check-equal? (channel-get ch)
                    (list (format "~a/d" dir) '(IN_DELETE IN_ISDIR)))

      (system/print (format "mv ~a/a ~a" dir2 dir))
      (check-equal? (channel-get ch)
                    (list (format "~a/a" dir) '(IN_MOVED_TO IN_ISDIR)))

      (system/print (format "touch ~a/a/b/1" dir))
      (check-equal? (channel-get ch)
                    (list (format "~a/a/b/1" dir) '(IN_CREATE)))

      (system/print (format "mv ~a/a/b ~a" dir dir2))
      (check-equal? (channel-get ch)
                    (list (format "~a/a/b" dir) '(IN_MOVED_FROM IN_ISDIR)))

      (system/print (format "touch ~a/b/2" dir2))

      (system/print (format "rm -rf ~a/a/" dir))
      (check-equal? (channel-get ch)
                    (list (format "~a/a" dir) '(IN_DELETE IN_ISDIR)))
     
      (send watcher stop-and-close)

      (system/print (format "rm -rf ~a" dir))
      (system/print (format "rm -rf ~a" dir2)))))))

(define box-print-tests
  (test-suite
   "box print tests"
   (test-case
    "An simple test"
    (box-print "Hello World!")
    (box-print "Hello World!" #:center? #t))
   (test-case
    "An long string test"
    (box-print
     "Hello World!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!wowowowowow!!!!!!"
     "Hello World!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!bobobobobob!!!!!")
    (box-print #:center? #t
     "Hello World!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!wowowowowow!!!!!!"
     "Hello World!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!bobobobobob!!!!!"))))

(define alert-tests
  (test-suite
   "alert tests"
   (test-case
    "A normal test"
    (define watcher (watch-rules (path) "."
                                 [#px"\\.rkt$"
                                     (displayln path)]))
    (send watcher stop-and-close))))

(module* main #f
  (run-tests watcher-tests)
  (run-tests box-print-tests)
  (run-tests alert-tests))
