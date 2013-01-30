#lang racket

(require ffi/unsafe
         racket/place)

(provide inotify-event read-events _inotify_init _inotify_add_watch _inotify_rm_watch buffer-size read-worker)

(define libc (ffi-lib #f))

           ;; struct inotify_event {
           ;;     int      wd;       /* Watch descriptor */
           ;;     uint32_t mask;     /* Mask of events */
           ;;     uint32_t cookie;   /* Unique cookie associating related
           ;;                           events (for rename(2)) */
           ;;     uint32_t len;      /* Size of name field */
           ;;     char     name[];   /* Optional null-terminated name */
           ;; };

(define-struct inotify-event (wd
                              mask
                              cookie
                              len
                              name)
  #:prefab)


;; /* Supported events suitable for MASK parameter of INOTIFY_ADD_WATCH.  */
;; #define IN_ACCESS	 0x00000001	/* File was accessed.  */
;; #define IN_MODIFY	 0x00000002	/* File was modified.  */
;; #define IN_ATTRIB	 0x00000004	/* Metadata changed.  */
;; #define IN_CLOSE_WRITE	 0x00000008	/* Writtable file was closed.  */
;; #define IN_CLOSE_NOWRITE 0x00000010	/* Unwrittable file closed.  */
;; #define IN_CLOSE	 (IN_CLOSE_WRITE | IN_CLOSE_NOWRITE) /* Close.  */
;; #define IN_OPEN		 0x00000020	/* File was opened.  */
;; #define IN_MOVED_FROM	 0x00000040	/* File was moved from X.  */
;; #define IN_MOVED_TO      0x00000080	/* File was moved to Y.  */
;; #define IN_MOVE		 (IN_MOVED_FROM | IN_MOVED_TO) /* Moves.  */
;; #define IN_CREATE	 0x00000100	/* Subfile was created.  */
;; #define IN_DELETE	 0x00000200	/* Subfile was deleted.  */
;; #define IN_DELETE_SELF	 0x00000400	/* Self was deleted.  */
;; #define IN_MOVE_SELF	 0x00000800	/* Self was moved.  */

;; /* Events sent by the kernel.  */
;; #define IN_UNMOUNT	 0x00002000	/* Backing fs was unmounted.  */
;; #define IN_Q_OVERFLOW	 0x00004000	/* Event queued overflowed.  */
;; #define IN_IGNORED	 0x00008000	/* File was ignored.  */

;; /* Helper events.  */
;; #define IN_CLOSE	 (IN_CLOSE_WRITE | IN_CLOSE_NOWRITE)	/* Close.  */
;; #define IN_MOVE		 (IN_MOVED_FROM | IN_MOVED_TO)		/* Moves.  */

;; /* Special flags.  */
;; #define IN_ONLYDIR	 0x01000000	/* Only watch the path if it is a
;; 					   directory.  */
;; #define IN_DONT_FOLLOW	 0x02000000	/* Do not follow a sym link.  */
;; #define IN_MASK_ADD	 0x20000000	/* Add to the mask of an already
;; 					   existing watch.  */
;; #define IN_ISDIR	 0x40000000	/* Event occurred against dir.  */
;; #define IN_ONESHOT	 0x80000000	/* Only send event once.  */


;; int inotify_init(void);

(define _inotify_init (get-ffi-obj "inotify_init" libc
                                   (_fun -> _int32)))

;; int inotify_add_watch(int fd, const char *pathname, uint32_t mask);

(define _inotify_add_watch (get-ffi-obj "inotify_add_watch" libc
                                        (_fun _int32 _string _uint32 -> _int)))

;; int inotify_rm_watch(int fd, uint32_t wd);
(define _inotify_rm_watch (get-ffi-obj "inotify_rm_watch" libc
                                       (_fun _int32 _uint32 -> (r : _int))))

;; ssize_t read(int fd, void *buf, size_t count);
(define _read (get-ffi-obj "read" libc
                           (_fun #:async-apply (lambda (f) (displayln f))
                                 #:save-errno 'posix
                                 _int32
                                 (output : (_bytes o size))
                                 (size : _uint32)
                                 -> (r : _int32)
                                 -> (values r (saved-errno) output))))

(define buffer-size (make-parameter 256))

(define (read-events/freezing fd)
  (define-values (r e b) (_read fd (buffer-size)))
  (and (< r 0) (= e 4) (break-enabled) (break-thread (current-thread)))
  (define pos 0)
  (with-input-from-bytes
   b
   (thunk
    (for/list ([i (stop-before (in-naturals) (lambda (i) (>= pos r)))])
     (let* ([wd (integer-bytes->integer (read-bytes 4) #t)]
            [mask (integer-bytes->integer (read-bytes 4) #f)]
            [cookie (integer-bytes->integer (read-bytes 4) #f)]
            [len (integer-bytes->integer (read-bytes 4) #f)]
            [oname (read-bytes len)]
            [olen (for/last ([i (in-range len)]) #:break (zero? (bytes-ref oname i)) i)]
            [name (cond [olen (bytes->string/utf-8 (subbytes oname 0 (add1 olen)))]
                        [else #f])])
       (set! pos (+ pos 16 len))
       (make-inotify-event wd mask cookie len name))))))

(define (read-worker pch)
  (define fd (place-channel-get pch))
  (place-channel-put pch (read-events/freezing fd)))

(define (read-events fd)
  (define pch (dynamic-place "inotify.rkt" 'read-worker))
  (place-channel-put pch fd)
  (place-channel-get pch))
