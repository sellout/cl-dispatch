(in-package #:dispatch)

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

(defmacro with-object ((var creator) &body body)
  `(let ((,var ,creator))
     (retain ,var)
     (unwind-protect (progn ,@body)
       (release ,var))))

(defmacro with-object-suspended ((object) &body body)
  (let ((objvar (gensym)))
    `(let ((,objvar ,object))
       (suspend ,objvar)
       (unwind-protect (progn ,@body)
         (resume ,objvar)))))
