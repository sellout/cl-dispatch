(in-package :dispatch)

(defctype queue :pointer) ; "dispatch_queue_t"
(defctype queue-attribute :pointer) ; "dispatch_queue_attribute_t"
(defctype function :pointer) ; "dispatch_function_t"
(defctype group :pointer) ; "dispatch_group_t"
(defctype semaphore :pointer) ; "dispatch_semaphore_t"
(defctype source :pointer) ; "dispatch_source_t"
(defctype uintptr :pointer) ; "uintptr_t"

(defctype data :pointer) ; "dispatch_data_t"

(defctype io :pointer) ; "dispatch_io_t"

(define-foreign-type inverted-boolean ()
  ()
  (:actual-type :long)
  (:simple-parser inverted-boolean))

(defmethod expand-to-foreign (value (type inverted-boolean))
  `(if ,value 0 1))

(defmethod expand-from-foreign (value (type inverted-boolean))
  `(zerop ,value))
