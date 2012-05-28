(in-package #:dispatch)

(defcfun (make-semaphore "dispatch_semaphore_create") semaphore (value :long))

(defcfun (signal-semaphore "dispatch_semaphore_signal") :boolean
  (dsema semaphore))

(defcfun (wait-on-semaphore "dispatch_semaphore_wait") inverted-boolean
  (dsema semaphore) (timeout time))

(defmacro with-semaphore-held ((semaphore timeout) &body body)
  (let ((semvar (gensym)))
    `(let ((,semvar ,semaphore))
       (when (wait-on-semaphore ,semvar ,timeout)
         (unwind-protect (progn ,@body)
           (signal-semaphore ,semvar))))))
