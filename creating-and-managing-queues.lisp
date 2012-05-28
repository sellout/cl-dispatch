(in-package #:dispatch)

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
