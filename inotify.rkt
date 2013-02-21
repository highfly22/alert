#lang racket/base

;;;    *************************************    ;;;
;;;    ***   Inotify - Racket Bindings   ***    ;;;
;;;    *************************************    ;;;

;;; author (FFI): Laurent orseau <laurent orseau gmail com> - 2013-01-04

(require "errno-base.rkt"
         ffi/unsafe
         ffi/unsafe/define
         racket/class
         racket/port
         racket/dict
         racket/path)

(provide (all-defined-out))

#| 

** Resources **

Documentation of the C API:
http://linux.die.net/man/7/inotify

See also pyinotify: 
http://seb-m.github.com/pyinotify/pyinotify.WatchManager-class.html#add_watch

** Description **

The inotify API provides a mechanism for monitoring file system events. 
Inotify can be used to monitor individual files, or to monitor directories.
When a directory is monitored, inotify will return events for the directory 
itself, and for files inside the directory. 

** Limitations and caveats (from the C docs) **

Inotify monitoring of directories is not recursive: to monitor 
subdirectories under a directory, additional watches must be created. 
This can take a significant amount time for large directory trees.

The inotify API provides no information about the user or process that 
triggered the inotify event.

Note that the event queue can overflow. In this case, events are lost.
Robust applications should handle the possibility of lost events gracefully.

The inotify API identifies affected files by filename. However, by the time
an application processes an inotify event, the filename may already have
been deleted or renamed.

If monitoring an entire directory subtree, and a new subdirectory is created
in that tree, be aware that by the time you create a watch for the new 
subdirectory, new files may already have been created in the subdirectory.
Therefore, you may want to scan the contents of the subdirectory immediately
after adding the watch. 

|#

;=================;
;=== C API FFI ===;
;=================;

(define-ffi-definer define-inotify (ffi-lib #f))

(define _flags
  (_bitmask
   '(IN_CLOEXEC  = 02000000
     IN_NONBLOCK =    04000)))

(define _mask
  (_bitmask
   '(NONE              = #x00000000  ; For default value
     IN_ACCESS         = #x00000001  ; File was accessed.
     IN_MODIFY         = #x00000002  ; File was modified.
     IN_ATTRIB         = #x00000004  ; Metadata changed, e.g., permissions, timestamps, extended attributes, link count (since Linux 2.6.25), UID, GID, etc
     IN_CLOSE_WRITE    = #x00000008  ; Writtable file was closed.
     IN_CLOSE_NOWRITE  = #x00000010  ; Unwrittable file closed.
     IN_CLOSE          = #x00000018  ; (IN_CLOSE_WRITE | IN_CLOSE_NOWRITE) ; Close.
     IN_OPEN           = #x00000020  ; File was opened.
     IN_MOVED_FROM     = #x00000040  ; File was moved from X.
     IN_MOVED_TO       = #x00000080  ; File was moved to Y.
     IN_MOVE           = #x000000c0  ; (IN_MOVED_FROM | IN_MOVED_TO) ; Moves.
     IN_CREATE         = #x00000100  ; Subfile was created.
     IN_DELETE         = #x00000200  ; Subfile was deleted.
     IN_DELETE_SELF    = #x00000400  ; Self was deleted.
     IN_MOVE_SELF      = #x00000800  ; Self was moved.
     ; Events sent by the kernel
     IN_UNMOUNT        = #x00002000  ; Backing fs was unmounted.
     IN_Q_OVERFLOW     = #x00004000  ; Event queued overflowed.
     IN_IGNORED        = #x00008000  ; File was ignored.
     ; Special flags
     IN_ONLYDIR        = #x01000000  ; Only watch the path if it is a directory
     IN_DONT_FOLLOW    = #x02000000  ; Do not follow a sym link
     IN_EXCL_UNLINK    = #x04000000  ; Exclude events on unlinked objects
     IN_MASK_ADD       = #x20000000  ; Add to the mask of an already existing watch
     IN_ISDIR          = #x40000000  ; Event occurred against dir.
     IN_ONESHOT        = #x80000000  ; Only send event once.
     )
   _uint32))
     
; All events which a program can wait on
(define IN_ALL_EVENTS  
  '(IN_ACCESS IN_MODIFY  IN_ATTRIB  IN_CLOSE_WRITE  
              IN_CLOSE_NOWRITE  IN_OPEN  IN_MOVED_FROM	      
              IN_MOVED_TO  IN_CREATE  IN_DELETE		      
              IN_DELETE_SELF  IN_MOVE_SELF))
  
(define-cstruct _inotify_event
  ((wd      _int)      ; Watch descriptor
   (mask    _mask)     ; Mask of events
   (cookie  _uint32)   ; Unique cookie associating related events (for `rename')
                       ; allows the resulting pair of IN_MOVE_FROM and IN_MOVE_TO 
                       ; events to be connected by the application.
   (len     _uint32)   ; Size of the name field
   ; Don't add the name, it will be read separately!
   ;(name    _pointer)  ; (char*) Optional null-terminated name
   ))

(define (make-inotify-empty-event)
  (make-inotify_event 0 'NONE 0 0 #;#f))

(define (inotify-init-errno funsym)
  (error funsym
   (format-errsym
    '((EINVAL . "An invalid value was specified in flags")
      (EMFILE . "The user limit on the total number of inotify instances has been reached")
      (ENFILE . "The system limit on the total number of file descriptors has been reached")
      (ENOMEM . "Insufficient kernel memory is available")))))

;/* Create and initialize inotify instance.  */
;extern int inotify_init (void) __THROW;
;; On success, these system calls return a new file descriptor. 
;; On error, -1 is returned, and errno is set to indicate the error. 
(define-inotify inotify_init 
  (_fun -> (fd : _int)
        -> (if (= fd -1)
               (inotify-init-errno 'inotify_init)
               fd
               )))

;/* Create and initialize inotify instance.  */
;extern int inotify_init1 (int __flags) __THROW;
;; See inotify_init
(define-inotify inotify_init1 
  (_fun _flags 
        -> (fd : _int)
        -> (if (= fd -1)
               (inotify-init-errno 'inotify_init1)
               fd)))

;/* Add watch of object NAME to inotify instance FD.  Notify about events specified by MASK.  */
;extern int inotify_add_watch (int __fd, const char *__name, uint32_t __mask) __THROW;
;; On success, inotify_add_watch() returns a nonnegative watch descriptor. 
;; On error -1 is returned and errno is set appropriately. 
(define-inotify inotify_add_watch 
  (_fun _int _string _mask 
        -> (wd : _int)
        -> (if (= wd -1)
               (error 'inotify_add_watch
                      (format-errsym
                       '((EACCES . "Read access to the given file is not permitted")
                         (EBADF  . "The given file descriptor is not valid")
                         (EFAULT . "pathname points outside of the process's accessible address space")
                         (EINVAL . "The given event mask contains no valid events; or fd is not an inotify file descriptor")
                         (ENOENT . "A directory component in pathname does not exist or is a dangling symbolic link")
                         (ENOMEM . "Insufficient kernel memory was available")
                         (ENOSPC . "The user limit on the total number of inotify watches was reached or the kernel failed to allocate a needed resource"))))
               wd)))

;/* Remove the watch specified by WD from the inotify instance FD.  */
;extern int inotify_rm_watch (int __fd, int __wd) __THROW;
;; On success, inotify_rm_watch() returns zero, 
;; or -1 if an error occurred (in which case, errno is set appropriately). 
(define-inotify inotify_rm_watch 
  (_fun _int _int 
        -> (res : _int)
        -> (if (= res -1)
               (error 'inotify_rm_watch
                      (format-errsym
                       '((EBADF  . "fd is not a valid file descriptor")
                         (EINVAL . "The watch descriptor wd is not valid; or fd is not an inotify file descriptor"))))
               #t)))

(define open-fd-input-port
  (get-ffi-obj "scheme_make_fd_input_port" (ffi-lib #f)
               (_fun (fd : _int) (_scheme = "fd-port") (_int = 0) (_int = 0) -> _racket)))

;=================;
;=== Interface ===;
;=================;

(module+ test
  (require rackunit)
  (displayln "Tests"))

;; Converts the bytes to a string, like bytes->string/locale
;; but omits the null bytes at the end of the byte string.
;; bytes? -> string?
(define (null-terminated-bytes->string/locale b)
  (define len
    (or
     (for/or ([i (in-range (bytes-length b) 0 -1)])
       (and (not (= 0 (bytes-ref b (sub1 i))))
            i))
     0))
  (bytes->string/locale b #f 0 len))

(module+ test
  (let ([proc null-terminated-bytes->string/locale])
    (check-equal? "A" (proc (bytes 65 0 0)))
    (check-equal? "ABC" (proc (bytes 65 66 67)))
    (check-equal? "ABC" (proc (bytes 65 66 67 0)))
    (check-equal? "ABC" (proc (bytes 65 66 67 0 0)))
    (check-equal? "" (proc (bytes)))
    (check-equal? "" (proc (bytes 0)))
    ))

(define (link-overlap? base link)
  (define b (path->string (simple-form-path (resolve-path base))))
  (define l (path->string (simple-form-path (resolve-path link))))
  (and (<= (string-length b) (string-length l))
   (equal? b (substring l 0 (string-length b)))))

(module+ test
  (check-equal? #t (link-overlap? "." "."))
  (check-equal? #t (link-overlap? "." "inotify.rkt"))
  (check-equal? #f (link-overlap? "." ".."))
  (check-equal? #f (link-overlap? "." "../..")))

(define-struct collection (name wd children))

(define (collection-add parent child name)
  (and parent
       (hash-set! (collection-children parent) name child)))

(define (collection-remove parent child-name)
  (begin0
      (hash-ref (collection-children parent) child-name)
    (hash-remove! (collection-children parent) child-name)))

(define inotify-watcher%
  (class object%
    (super-new)
    (init-field path
                [callback #f]
                [ignore #rx"^.git$|^.svn$|.log$|^compiled$|^\\.#|^#.*#$"]
                [recursive? #f]
                [follow-links? #t]
                [finalize? #f])
    ;; callback: (path-string? (listof symbol?) . -> . any)
    (field [watches (make-hash)]
           [fd (inotify_init)]
           [in (open-fd-input-port fd)]
           [mask '(IN_MODIFY IN_CREATE IN_DELETE IN_MOVED_FROM IN_MOVED_TO)])
    
    (define (complete-name parent name)
      (if parent (build-path (collection-name parent) name) name))

    (define (add-watch-base parent cname name)
      (define wd (inotify_add_watch fd cname mask))
      (log-debug "add-watch-base ~a ~a ~a\n" cname mask wd)
      (define c (make-collection cname wd (make-hash)))
      (hash-set! watches wd c)
      (collection-add parent c name)
      c)

    (define (add-watch parent name)
      (define cname (complete-name parent name))
      (define c (add-watch-base parent cname name))
      (when (and recursive? (directory-exists? cname))
        (for ([d (directory-list cname)])
          (define cname (complete-name c d))
          (when (and (directory-exists? cname)
                     (or (not (link-exists? cname))
                         (and follow-links?
                              (link-overlap? path cname)))
                     (or (not ignore) (not (regexp-match ignore d))))
            (add-watch c (path->string d))))))
    
    (define (remove-watch child)
      (log-debug "remove-watch ~a\n" (collection-wd child))
      (define child-wd (collection-wd child))
      (hash-remove! watches child-wd)
      (with-handlers ([exn:fail? (λ(e) #t)])
        (inotify_rm_watch fd child-wd))
      (when recursive?
        (for [(i (in-hash-values (collection-children child)))]
          (remove-watch i))))

    (define (start)
      (thread
       (λ()
         (define (combine-path watch name)
           (define base (collection-name watch))
           (if name (build-path base name) base))

         (define ev (make-inotify-empty-event))
         (define ev-bytes (make-sized-byte-string ev (ctype-sizeof _inotify_event)))

         (let loop ()
           (sync/enable-break
            (handle-evt (read-bytes!-evt ev-bytes in)
                        (λ(_fd)
                          (define len (inotify_event-len ev))
                          (define name #f)
                          (when (> len 0)
                            (set! name (null-terminated-bytes->string/locale (read-bytes len in))))
                          (define wd (inotify_event-wd ev))
                          (define mask (inotify_event-mask ev))
                          
                          (log-debug "Event: ~a\n" (list wd name mask))

                          (define watch (hash-ref watches wd #f))
                          (when (and watch
                                     (not (and name ignore (regexp-match ignore name))))
                            (when (and recursive?
                                       (member 'IN_ISDIR mask)
                                       (or (member 'IN_MOVED_TO mask)
                                           (member 'IN_CREATE mask)))
                              (add-watch watch name))
                            (when (and (member 'IN_ISDIR mask)
                                       (or (member 'IN_DELETE mask)
                                           (member 'IN_MOVED_FROM mask)))
                              (define child (collection-remove watch name))
                              (remove-watch child))
                            (when (and (member 'IN_ISDIR mask)
                                       (member 'IN_DELETE_SELF mask))
                              (remove-watch watch))
                            (and callback
                                 (callback (combine-path watch name) mask)))
                          (loop)))
            (handle-evt (port-closed-evt in)
                        (λ(_a)(log-debug "Port closed.")))))
         (log-debug "Thread ended."))))

    (define (init)
      (unless (path-string? path)
        (raise-argument-error 'watch-directory% "path-string?" path))
      (add-watch #f path)
      (start))
    (init)

    (define/public (stop-and-close) 
      (when in
        ;; (remove-all-watches) ; no need: done automatically when closing the port, which closes the file descriptor
        ;; This also automatically ends the thread
        (close-input-port in)
        ;; (set! in #f)
        ))))

