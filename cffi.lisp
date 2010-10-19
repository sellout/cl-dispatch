(in-package :dispatch)

(define-foreign-library libdispatch
                        (:darwin (:default "libSystem"))
                        (t (:default "libdispatch")))
(use-foreign-library libdispatch)

;;; Queuing Tasks for Dispatch

(defcfun (dispatch-after "dispatch_after_f") :void
  (when time) (queue queue) (context :pointer) (work function))
(defcfun (dispatch-apply "dispatch_apply_f") :void
  (iterations :int) ; FIXME: size_t
  (queue queue)
  (context :pointer)
  (work :pointer)) ; void (*work)(void *, size)
(defcfun (dispatch-async "dispatch_async_f") :void
  (queue queue) (context :pointer) (work function))
(defcfun (current-queue "dispatch_get_current_queue") queue)
(defcfun (global-queue "dispatch_get_global_queue") queue
  (priority queue-priority) (flags :ulong))
(defcfun (main-queue "dispatch_get_main_queue") queue)
(defcfun (event-loop "dispatch_main") :void)
(defcfun "dispatch_queue_create" queue (label :string) (attr queue-attribute))
(defmethod make-instance
           ((type (eql 'queue))
            &key (label (error "Can't create a queue without a label.")) attribute)
  (dispatch-queue-create label attribute))
(defcfun (label "dispatch_queue_get_label") :string (queue queue))
(defcfun "dispatch_set_target_queue" :void (object object) (queue queue))
(defun (setf target-queue) (queue object)
  (dispatch-set-target-queue object queue)
  queue)
(defcfun (dispatch-sync "dispatch_sync_f") :void
  (queue queue) (context :pointer) (work function))

;;; Using Dispatch Groups

(defcfun (dispatch-group-async "dispatch_group_async_f") :void
  (group group) (queue queue) (context :pointer) (work function))
(defcfun "dispatch_group_create" group)
(defmethod make-instance ((type (eql 'group)) &key)
  (dispatch-group-create))
(defcfun (enter "dispatch_group_enter") :void (group group))
(defcfun (leave "dispatch_group_leave") :void (group group))
(defcfun (dispatch-group-notify "dispatch_group_notify_f") :void
  (group group) (queue queue) (context :pointer) (work function))
(defcfun (wait-on-group "dispatch_group_wait") :long (group group) (timeout time))

;;; Managing Dispatch Objects

(defcfun (debug "dispatch_debug") :void (object object) (message :string))
(defcfun (context "dispatch_get_context") :pointer (object object))
(defcfun (release "dispatch_release") :void (object object))
(defcfun (resume "dispatch_resume") :void (object object))
(defcfun (retain "dispatch_retain") :void (object object))
(defcfun "dispatch_set_context" :void (object object) (context :pointer))
(defun (setf context) (context object)
  (dispatch-set-context object context)
  context)
(defcfun "dispatch_set_finalizer_f" :void (object object) (finalizer function))
(defun (setf finalizer) (finalizer object)
  (dispatch-set-finalizer-f object finalizer)
  finalizer)
(defcfun (suspend "dispatch_suspend") :void (object object))

;;; Using Semaphores

(defcfun "dispatch_semaphore_create" semaphore (value :long))
(defmethod make-instance ((type (eql 'semaphore)) &key (value 0))
  (dispatch-semaphore-create value))
(defcfun (signal-semaphore "dispatch_semaphore_signal") :long (dsema semaphore))
(defcfun (wait-on-semaphore "dispatch_semaphore_wait") :long
  (dsema semaphore) (timeout time))

;;; Handling Events

(defcfun (cancel "dispatch_source_cancel") :void (source source))
(defcfun "dispatch_source_create" source
  (type source-type) (handle uintptr) (mask :ulong) (queue queue))
(defmethod make-instance ((type (eql 'source)) &key type handle mask queue)
  (dispatch-source-create type handle mask queue))
(defcfun (data "dispatch_source_get_data") :ulong (source source))
(defcfun (handle "dispatch_source_get_handle") :uintptr (source source))
(defcfun (mask "dispatch_source_get_mask") :ulong (source source))
(defcfun (merge-data "dispatch_source_merge_data") :void
  (source source) (value :ulong))
(defcfun "dispatch_source_set_cancel_handler_f" :void
  (source source) (cancel-handler function))
(defun (setf cancel-handler) (handler source)
  (dispatch-source-set-cancel-handler-f source handler)
  handler)
(defcfun "dispatch_source_set_event_handler_f" :void
  (source source) (handler function))
(defun (setf event-handler) (handler source)
  (dispatch-source-set-event-handler-f source handler)
  handler)
(defcfun (set-timer "dispatch_source_set_timer") :void
  (source source) (start time) (interval :uint64) (leeway :uint64))
(defcfun "dispatch_source_testcancel" :long (source source))
(defun canceledp (source)
  (/= 0 (dispatch-source-testcancel source)))

;;; Managing Time

(defcfun (offset-time "dispatch_time") time (when time) (delta :int64))
;; FIXME: is walltime useful?
;;(defcfun "dispatch_walltime" time (when timespec) (delta :int64))

;;; convenience functions

(defmacro with-dispatch-object ((var creator) &body body)
  `(let ((,var ,creator))
     (unwind-protect
         (progn
           (retain ,var)
           ,@body)
       (release ,var))))