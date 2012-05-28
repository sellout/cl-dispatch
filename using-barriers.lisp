(in-package #:dispatch)

(defcfun "dispatch_barrier_async_f" :void
  (queue queue) (context :pointer) (work function))
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

(defcfun "dispatch_barrier_sync_f" :void
  (queue queue) (context :pointer) (work function))
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
