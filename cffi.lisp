(in-package #:dispatch)

(define-foreign-library libdispatch
                        (:darwin (:default "libSystem"))
                        (t (:default "libdispatch")))
(use-foreign-library libdispatch)
