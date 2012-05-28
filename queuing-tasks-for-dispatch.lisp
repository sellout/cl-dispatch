(in-package #:dispatch)

(defcfun "dispatch_async_f" :void
  (queue queue) (context :pointer) (work function))
(define-dispatch-function dispatch-async (queue work)
  "Submits an application-defined function for asynchronous execution on a
   dispatch queue and returns immediately.

   Parameters
*  queue: The queue on which to submit the function. This parameter cannot be
   NIL.
*  work: The application-defined function to invoke on the target queue. It can
   not take any parameters. This parameter cannot be NIL.

   Discussion
   This function is the fundamental mechanism for submitting application-defined
   functions to a dispatch queue. Calls to this function always return
   immediately after the function has been submitted and never wait for it to be
   invoked. The target queue determines whether the function is invoked serially
   or concurrently with respect to other tasks submitted to that same queue.
   Serial queues are processed concurrently with respect to each other.")

(defcfun "dispatch_sync_f" :void
  (queue queue) (context :pointer) (work function))
(define-dispatch-function dispatch-sync (queue work)
  "Submits an application-defined function for synchronous execution on a
   dispatch queue.

   Parameters
*  queue: The queue on which to submit the function. This parameter cannot be
   NIL.
*  work: The application-defined function to invoke on the target queue. It can
   not take any parameters. This parameter cannot be NIL.")

(defcfun "dispatch_after_f" :void
  (when time) (queue queue) (context :pointer) (work function))
(define-dispatch-function dispatch-after (when queue work)
  "Enqueues an application-defined function for execution at a specified time.

   Parameters
*  when: The temporal milestone returned by OFFSET-TIME or WALLTIME.
*  queue: The queue on which to submit the function. This parameter cannot be
   NIL.
*  work: The application-defined function to invoke on the target queue. It can
   not take any parameters. This parameter cannot be NIL.

   Discussion
   This function waits until the specified time and then asynchronously adds the
   work function to the specified queue.

   Passing +NOW+ as the WHEN parameter is supported, but is not as optimal as
   calling DISPATCH-ASYNC instead. Passing +FOREVER+ is undefined.")

(defcfun "dispatch_apply_f" :void
  (iterations size) (queue queue) (context :pointer) (work function))
(defcallback apply-callback :void ((context :pointer) (iteration :int))
  ;; We cannot throw out of here. If we do, libdispatch will get very
  ;; confused.
  (with-simple-restart (abort "Return from libdispatch callback")
    (funcall (id-map-free-object *dispatch-id-map* (pointer-address context))
             iteration)))
(defun dispatch-apply (iterations queue work)
  "Submits an application-defined function to a dispatch queue for multiple
   invocations.

   Parameters
*  iterations: The number of iterations to perform.
*  queue: The queue on which to submit the function. This parameter cannot be
   NIL.
*  work: The application-defined function to invoke on the target queue. The
   first parameter passed to this function is the current index of iteration.
   This parameter cannot be NIL.

   Discussion
   This function submits an application-defined function to a dispatch queue for
   multiple invocations and waits for all iterations of the function to complete
   before returning. If the target queue is a concurrent queue returned by
   GLOBAL-QUEUE, the function can be invoked concurrently, and it must therefore
   be reentrant-safe. Using this function with a concurrent queue can be useful
   as an efficient parallel for loop.

   The current index of iteration is passed to each invocation of the function."
  (dispatch-apply-f iterations
                    queue
                    (make-pointer (assign-id-map-id *dispatch-id-map* work))
                    (callback apply-callback)))
