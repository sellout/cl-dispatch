(in-package #:dispatch)

(defcfun "dispatch_queue_set_specific" :void
  (queue queue) (key :pointer) (context :pointer) (destructor function))
(defun (setf queue-specific-context)
    (context queue key &optional (destructor (null-pointer)))
  (dispatch-queue-set-specific queue key context destructor))

(defcfun "dispatch_queue_get_specific" :pointer (queue queue) (key :pointer))
(defun queue-specific-context (queue key)
  (let ((context-data (dispatch-queue-get-specific queue key)))
    (unless (null-pointer-p context-data)
      context-data)))

(defcfun "dispatch_get_specific" :pointer (key :pointer))
(defun current-queue-specific-context (key)
  (let ((context-data (dispatch-get-specific key)))
    (unless (null-pointer-p context-data)
      context-data)))
