(in-package #:dispatch)

(defvar *dispatch-id-map* (make-id-map))

(defcallback dispatch-callback :void ((context :pointer))
  ;; We cannot throw out of here. If we do, libdispatch will get very
  ;; confused.
  (with-simple-restart (abort "Return from libdispatch callback")
    (funcall (id-map-free-object *dispatch-id-map* (pointer-address context)))))

(defmacro define-dispatch-function (name (&rest lambda-list) docstring)
  `(defun ,name (,@lambda-list)
     ,docstring
     (,(intern (format nil "~A-F" name))
       ,@(butlast lambda-list)
       (make-pointer (assign-id-map-id *dispatch-id-map* ,(car (last lambda-list))))
       (callback dispatch-callback))))
