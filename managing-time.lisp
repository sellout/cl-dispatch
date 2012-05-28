(in-package #:dispatch)

(defcfun (offset-time "dispatch_time") time (when time) (delta :int64))

;; FIXME: is walltime useful?
;;(defcfun "dispatch_walltime" time (when timespec) (delta :int64))
