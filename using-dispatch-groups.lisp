(in-package #:dispatch)

(defcfun "dispatch_group_async_f" :void
  (group group) (queue queue) (context :pointer) (work function))
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

(defcfun (make-group "dispatch_group_create") group)

(defcfun (enter "dispatch_group_enter") :void (group group))

(defcfun (leave "dispatch_group_leave") :void (group group))

(defcfun "dispatch_group_notify_f" :void
  (group group) (queue queue) (context :pointer) (work function))

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

(defcfun (wait-on-group "dispatch_group_wait") inverted-boolean
  (group group) (timeout time))
