#lang racket

(require "inotify.rkt"
         racket/file
         rackunit
         rackunit/text-ui)

(define watcher-tests
  (test-suite
   "inotify tests"
   #:before (lambda () (display "Before\n"))
   #:after  (lambda () (display "After\n"))
   (test-case
    "An simple touch test"
    (define dir (make-temporary-file "rkttmp~a" 'directory))
    (define dir2 (make-temporary-file "rkttmp~a" 'directory))
    (system (format "mkdir -p ~a/a/b/c/d" dir2))

    (define ch (make-channel))

    (define watcher (new diretory-watcher%
                         [path dir]
                         [recursive? #t]))

    ;; (send inotify add-watch
    ;;       dir
    ;;       '(IN_MODIFY IN_CREATE IN_DELETE IN_DELETE_SELF IN_MOVED_FROM IN_MOVED_TO)
    ;;       (lambda (watch name mask)
    ;;         (print (list watch name mask)) (newline)
    ;;         ;; (channel-put ch (list watch name mask))
    ;;         )
    ;;       #t)
    ;; (send inotify start)

    (system (format "touch ~a/1" dir))
    ;; (check-equal? (channel-get ch) (list dir "1" '(IN_CREATE)))

    (system (format "mkdir ~a/d" dir))
    ;; (check-equal? (channel-get ch) (list dir "d" '(IN_CREATE IN_ISDIR)))

    (system (format "touch ~a/d/1" dir))
    ;; (check-equal? (channel-get ch) (list (build-path dir "d") "1" '(IN_CREATE)))

    (system (format "rm -rf ~a/d" dir))
    ;; (channel-get ch)
    ;; (channel-get ch)
    ;; (channel-get ch)
    ;; (channel-get ch)

    (system (format "mv ~a/a ~a" dir2 dir))
    ;; (check-equal? (channel-get ch) (list dir "a" '(IN_MOVED_TO IN_ISDIR)))

    (system (format "touch ~a/a/b/1" dir))
    ;; (check-equal? (channel-get ch) (list (build-path dir "a/b") "1" '(IN_CREATE)))

    (system (format "mv ~a/a/b ~a" dir dir2))
    
    (system (format "touch ~a/b/2" dir2))

    (system (format "rm -rf ~a/a/" dir))
    ;; (channel-get ch)
    
    (system (format "ls -R ~a" dir))
    (system (format "ls -R ~a" dir2))
    
    (send watcher stop-and-close)
    (system (format "rm -rf ~a" dir))
    (system (format "rm -rf ~a" dir2))
    )))

(module* main #f
  (run-tests watcher-tests))

