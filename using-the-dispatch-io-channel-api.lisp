(in-package :dispatch)

(defcfun (close "dispatch_io_close") :void (channel io) (flags io-close-flags))

(defcfun "dispatch_io_set_high_water" :void (channel io) (high-water size))
(defun (setf high-water) (high-water channel)
  (dispatch-io-set-high-water channel high-water)
  high-water)

(defcfun "dispatch_io_set_low_water" :void (channel io) (low-water size))
(defun (setf low-water) (low-water channel)
  (dispatch-io-set-low-water channel low-water)
  low-water)

(defcfun "dispatch_io_set_interval" :void
  (channel io) (interval :uint64) (flags io-interval-flags))
(defun (setf interval) (interval channel &optional (flags 0))
  (dispatch-io-set-interval channel interval flags)
  interval)
