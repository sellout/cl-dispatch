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
  :license "MIT"
  :author "Greg Pfeil <greg@technomadic.org>"
  :depends-on (cffi cffi-grovel)
  :components
  ((:file "package")
   (:file "cffi" :depends-on ("package"))
   (:file "id-map" :depends-on ("using-semaphores"))
   (:file "dispatching" :depends-on ("id-map"))
   (:file "types" :depends-on ("package"))
   (cffi-grovel:grovel-file "grovel" :depends-on ("types"))
   (:file "creating-and-managing-queues" :depends-on ("types"))
   (:file "queuing-tasks-for-dispatch" :depends-on ("dispatching"))
   (:file "using-dispatch-groups" :depends-on ("dispatching"))
   (:file "managing-dispatch-objects" :depends-on ("types"))
   (:file "using-semaphores" :depends-on ("grovel"))
   (:file "using-barriers" :depends-on ("dispatching"))
   (:file "managing-dispatch-sources" :depends-on ("types"))
   (:file "using-the-dispatch-io-channel-api" :depends-on ("grovel"))
   (:file "managing-dispatch-data-objects" :depends-on ("grovel"))
   (:file "managing-time" :depends-on ("types"))
   (:file "managing-queue-specific-context-data" :depends-on ("types"))))
