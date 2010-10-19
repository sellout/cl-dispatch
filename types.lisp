(in-package :dispatch)

(defctype queue :pointer) ; "dispatch_queue_t"
(defctype queue-attribute :pointer) ; "dispatch_queue_attribute_t"
(defctype time :pointer) ; "dispatch_time_t"
(defctype function :pointer) ; "dispatch_function_t"
(defctype group :pointer) ; "dispatch_group_t"
(defctype semaphore :pointer) ; "dispatch_semaphore_t"
(defctype source :pointer) ; "dispatch_source_t"
(defctype source-type :pointer) ; "dispatch_source_type_t"
(defctype uintptr :pointer) ; "uintptr_t"
