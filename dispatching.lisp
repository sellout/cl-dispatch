(in-package #:dispatch)

(defvar *dispatch-id-map* (make-id-map))

(defcallback dispatch-callback :void ((context :pointer))
  ;; We cannot throw out of here. If we do, libdispatch will get very
  ;; confused.
  (with-simple-restart (abort "Return from libdispatch callback")
    (funcall (id-map-free-object *dispatch-id-map* (pointer-address context)))))

(defcallback apply-callback :void ((context :pointer) (iteration :int))
  ;; We cannot throw out of here. If we do, libdispatch will get very
  ;; confused.
  (with-simple-restart (abort "Return from libdispatch callback")
    (funcall (id-map-free-object *dispatch-id-map* (pointer-address context))
             iteration)))

(defmacro define-dispatch-function (name (&rest lambda-list) docstring)
  `(defun ,name (,@lambda-list)
     ,docstring
     (,(intern (format nil "~A-F" name))
       ,@(butlast lambda-list)
       (make-pointer (assign-id-map-id *dispatch-id-map* ,(car (last lambda-list))))
       (callback dispatch-callback))))

;;; Queuing Tasks for Dispatch

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

(define-dispatch-function dispatch-sync (queue work)
  "Submits an application-defined function for synchronous execution on a
   dispatch queue.

   Parameters
*  queue: The queue on which to submit the function. This parameter cannot be
   NIL.
*  work: The application-defined function to invoke on the target queue. It can
   not take any parameters. This parameter cannot be NIL.")

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

;;; Using Dispatch Groups

(define-dispatch-function dispatch-group-async (group queue work)
  "Submits an application-defined function to a dispatch queue and associates it
   with the specified dispatch group.

   Parameters
*  group: A dispatch group to associate the submitted function with. This
   parameter cannot be NIL.
*  queue: The dispatch queue to which the function is submitted for asynchronous
   invocation. This parameter cannot be NIL.
*  work: The application-defined function to invoke on the target queue. It can
   not take any parameters.

   Discussion
   Submits an application-defined function to a dispatch queue and associates it
   with the given dispatch group. The dispatch group can be used to wait for the
   completion of the application-defined functions it references.")

(define-dispatch-function dispatch-group-notify (group queue work)
  "Schedules an application-defined function to be submitted to a queue when a
   group of previously submitted block objects have completed.

   Parameters
*  group: The dispatch group to observe. This parameter cannot be NIL.
*  queue: The queue to which the supplied block is submitted when the group
   completes. This parameter cannot be NIL.
*  work: The application-defined function to invoke on the target queue. It can
   not take any parameters.

   Discussion
   This function schedules a notification block to be submitted to the specified
   queue when all blocks associated with the dispatch group have completed. If
   the group is empty (no block objects are associated with the dispatch group),
   the notification block object is submitted immediately.

   When the notification block is submitted, the group is empty. The group can
   either be released with RELEASE or be reused for additional blocks. See
   DISPATCH-GROUP-ASYNC for more information.")

;;; Using Barriers

(define-dispatch-function dispatch-barrier-async (queue work)
  "Submits a barrier function for asynchronous execution and returns
   immediately.

   Parameters
*  queue: The dispatch queue on which to execute the barrier function. This
   parameter cannot be NIL.
*  work: The application-defined barrier function to be executed. This parameter
   cannot be NIL.

   Discussion
   Calls to this function always return immediately after the barrier function
   has been submitted and never wait for that function to be invoked. When the
   barrier function reaches the front of a private concurrent queue, it is not
   executed immediately. Instead, the queue waits until its currently executing
   blocks finish executing. At that point, the queue executes the barrier
   function by itself. Any blocks submitted after the barrier function are not
   executed until the barrier function completes.

   The queue you specify should be a concurrent queue that you create yourself
   using the dispatch_queue_create function. If the queue you pass to this
   function is a serial queue or one of the global concurrent queues, this
   function behaves like the DISPATCH-ASYNC function.")

(define-dispatch-function dispatch-barrier-sync (queue work)
  "Submits a barrier function for execution and waits until that function
   completes.

   Parameters
*  queue: The dispatch queue on which to execute the barrier function. This
   parameter cannot be NIL.
*  work: The application-defined barrier function to be executed. This parameter
   cannot be NIL.

   Discussion
   Submits a barrier function to a dispatch queue for synchronous execution.
   Unlike DISPATCH-BARRIER-ASYNC, this function does not return until the
   barrier function has finished. Calling this function and targeting the
   current queue results in deadlock.

   When the barrier function reaches the front of a private concurrent queue, it
   is not executed immediately. Instead, the queue waits until its currently
   executing blocks finish executing. At that point, the queue executes the
   barrier function by itself. Any blocks submitted after the barrier function
   are not executed until the barrier function completes.

   The queue you specify should be a concurrent queue that you create yourself
   using the dispatch_queue_create function. If the queue you pass to this
   function is a serial queue or one of the global concurrent queues, this
   function behaves like the DISPATCH-ASYNC function.

   As an optimization, this function invokes the barrier function on the current
   thread when possible.")
