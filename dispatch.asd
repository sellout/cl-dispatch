(defpackage dispatch-system
  (:use #:cl #:asdf))

(in-package :dispatch-system)

;;; NOTE: before this will work, you need to have libdispatch installed

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc #+quicklisp #'ql:quickload #-quicklisp #'asdf:load-system
        '(cffi-grovel)))

(defsystem dispatch
  :description "CFFI bindings to Grand Central Dispatch."
  :long-description "Grand Central Dispatch (GCD) is an approach to multicore
                     computing that is used throughout Mac OS X. GCD combines an
                     easy-to-use programming model with highly-efficient system
                     services to simplify the code needed to make best use of
                     multiple processors."
  :license "undecided"
  :author "Greg Pfeil <greg@technomadic.org>"
  :depends-on (cffi cffi-grovel)
  :components ((:file "package")
               (:file "types" :depends-on ("package"))
               (cffi-grovel:grovel-file "grovel" :depends-on ("types"))
               (:file "cffi" :depends-on ("grovel"))
               (:file "id-map" :depends-on ("cffi"))
               (:file "dispatching" :depends-on ("id-map"))))
