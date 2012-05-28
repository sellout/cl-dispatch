(in-package #:dispatch)

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

;; mach-send-source flags
(defconstant dead #x0
  "The receive right corresponding to the given send right was destroyed.")

;; process-source flags
(defconstant exit #x80000000
  "The process has exited (perhaps cleanly, perhaps not).")
(defconstant fork #x40000000
  "The process has created one or more child processes.")
(defconstant exec #x20000000
  "The process has become another executable image via an exec or posix_spawn
   function family call.")
(defconstant signal #x08000000
  "A Unix signal was delivered to the process.")

;; vnode-source flags
(defconstant delete #x1
  "The file-system object was deleted from the namespace.")
(defconstant write #x2
  "The file-system object data changed.")
(defconstant extend #x4
  "The file-system object changed in size.")
(defconstant attribute #x8
  "The file-system object metadata changed.")
(defconstant link-count #x10
  "The file-system object link count changed.")
(defconstant rename #x20
  "The file-system object was renamed in the namespace.")
(defconstant revoke #x40
  "The file-system object was revoked.")
