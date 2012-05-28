(in-package #:dispatch)

(defcfun (make-data "dispatch_data_create") data
  (buffer :pointer) (size size) (queue queue) (destructor :pointer))

(defcfun (size "dispatch_data_get_size") size (data data))

(defcfun "dispatch_data_create_map" data
  (data data) (buffer :pointer) (size :pointer))
(defun map (data)
  (with-foreign-objects ((buffer :pointer)
                         (size size))
    (values (dispatch-data-create-map data buffer size)
            (mem-ref buffer :pointer)
            (mem-ref size size))))

(defcfun (concatenate "dispatch_data_create_concat") data
  (data1 data) (data2 data))

(defcfun (subrange "dispatch_data_create_subrange") data
  (data data) (offset size) (size size))

(defcfun "dispatch_data_copy_region" data
  (data data) (location size) (offset :pointer))
(defun copy-region (data location)
  (with-foreign-object (offset size)
    (values (dispatch-data-copy-region data location offset)
            (mem-ref offset size))))
