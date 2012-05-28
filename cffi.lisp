(in-package #:dispatch)

(define-foreign-library libdispatch
                        (:darwin (:default "libSystem"))
                        (t (:default "libdispatch")))
(use-foreign-library libdispatch)

;;; Creating and Managing Queues

(defvar *serial-queue* (make-pointer +serial-queue-address+))
(defvar *concurrent-queue* (make-pointer +concurrent-queue-address+))

(defcfun (global-queue "dispatch_get_global_queue") queue
  (priority queue-priority) (flags :ulong))
(defcfun (main-queue "dispatch_get_main_queue") queue)
(defcfun "dispatch_queue_create" queue (label :string) (attr queue-attribute))
(defun make-queue (attr &optional (label (null-pointer)))
  (dispatch-queue-create label
                         (ecase attr
                           (:concurrent *concurrent-queue*)
                           (:serial *serial-queue*))))
(defcfun (current-queue "dispatch_get_current_queue") queue)
(defcfun (label "dispatch_queue_get_label") :string (queue queue))
(defcfun "dispatch_set_target_queue" :void (object object) (queue queue))
(defun (setf target-queue) (queue object)
  (dispatch-set-target-queue object queue)
  queue)
(defcfun "dispatch_main" :void)

;;; Queuing Tasks for Dispatch

(defcfun "dispatch_async_f" :void
  (queue queue) (context :pointer) (work function))
(defcfun "dispatch_sync_f" :void
  (queue queue) (context :pointer) (work function))
(defcfun "dispatch_after_f" :void
  (when time) (queue queue) (context :pointer) (work function))
(defcfun "dispatch_apply_f" :void
  (iterations :int) ; FIXME: size_t
  (queue queue)
  (context :pointer)
  (work function)) ; void (*work)(void *, size)

;;; Using Dispatch Groups

(defcfun "dispatch_group_async_f" :void
  (group group) (queue queue) (context :pointer) (work function))
(defcfun (make-group "dispatch_group_create") group)
(defcfun (enter "dispatch_group_enter") :void (group group))
(defcfun (leave "dispatch_group_leave") :void (group group))
(defcfun "dispatch_group_notify_f" :void
  (group group) (queue queue) (context :pointer) (work function))
(defcfun (wait-on-group "dispatch_group_wait") inverted-boolean
  (group group) (timeout time))

;;; Managing Dispatch Objects

(defcfun (log-debug-info "dispatch_debug") :void (object object) (message :string))
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

(defcfun (make-semaphore "dispatch_semaphore_create") semaphore (value :long))
(defcfun (signal-semaphore "dispatch_semaphore_signal") :boolean
  (dsema semaphore))
(defcfun (wait-on-semaphore "dispatch_semaphore_wait") inverted-boolean
  (dsema semaphore) (timeout time))

;;; Using Barriers

(defcfun "dispatch_barrier_async_f" :void
  (queue queue) (context :pointer) (work function))
(defcfun "dispatch_barrier_sync_f" :void
  (queue queue) (context :pointer) (work function))

;;; Managing Dispatch Sources

(defcfun (cancel "dispatch_source_cancel") :void (source source))
(defcfun "dispatch_source_create" source
  (type source-type) (handle uintptr) (mask :ulong) (queue queue))
(defun make-source (type queue &key (handle (null-pointer)) (mask 0))
  (dispatch-source-create type handle mask queue))
(defun make-data-add-source (queue)
  (make-source :data-add-source queue))
(defun make-data-or-source (mask queue)
  (make-source :data-or-source queue :mask mask))
(defun make-mach-receive-source (mach-port queue)
  (make-source :mach-receive-source queue :handle mach-port))
(defun make-mach-send-source (mach-port desired-events queue)
  (make-source :mach-send-source queue :handle mach-port :mask desired-events))
(defun make-process-source (process-id desired-events queue)
  (make-source :process-source queue :handle process-id :mask desired-events))
(defun make-read-source (file-descriptor queue)
  (make-source :read-source queue :handle (make-pointer file-descriptor)))
(defun make-signal-source (signal-number queue)
  (make-source :signal-source queue :handle (make-pointer signal-number)))
(defun make-timer-source (queue)
  (make-source :timer-source queue))
(defun make-vnode-source (file-descriptor desired-events queue)
  (make-source :vnode-source queue
               :handle (make-pointer file-descriptor) :mask desired-events))
(defun make-write-source (file-descriptor queue)
  (make-source :write-source queue :handle (make-pointer file-descriptor)))
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
(defcfun (canceled-p "dispatch_source_testcancel") :boolean
  (source source))

;;; FIXME: These constants should be groveled, but it's not working
(defconstant dead #x0)

(defconstant exit #x80000000)
(defconstant fork #x40000000)
(defconstant exec #x20000000)
(defconstant signal #x08000000)

(defconstant delete #x1)
(defconstant write #x2)
(defconstant extend #x4)
(defconstant attribute #x8)
(defconstant link-count #x10)
(defconstant rename #x20)
(defconstant revoke #x40)

;;; Managing Time

(defcfun (offset-time "dispatch_time") time (when time) (delta :int64))
;; FIXME: is walltime useful?
;;(defcfun "dispatch_walltime" time (when timespec) (delta :int64))

;;; convenience functions

(defmacro with-object ((var creator) &body body)
  `(let ((,var ,creator))
     (retain ,var)
     (unwind-protect (progn ,@body)
       (release ,var))))

(defmacro with-semaphore-held ((semaphore timeout) &body body)
  (let ((semvar (gensym)))
    `(let ((,semvar ,semaphore))
       (when (wait-on-semaphore ,semvar ,timeout)
         (unwind-protect (progn ,@body)
           (signal-semaphore ,semvar))))))

(defmacro with-object-suspended ((object) &body body)
  (let ((objvar (gensym)))
    `(let ((,objvar ,object))
       (suspend ,objvar)
       (unwind-protect (progn ,@body)
         (resume ,objvar)))))
