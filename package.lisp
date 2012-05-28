(defpackage dispatch
  (:use #:cl #:cffi)
  (:shadow #:function #:time)
  (:export ;; Creating and Managing Queues
           #:global-queue
           #:main-queue
           #:queue
           #:current-queue
           #:label
           #:target-queue
           #:dispatch-main
           ;; Queuing Tasks for Dispatch
           #:dispatch-async
           #:dispatch-sync
           #:dispatch-after
           #:dispatch-apply
           ;; Using Dispatch Groups
           #:dispatch-group-async
           #:group
           #:enter #:leave
           #:dispatch-group-notify
           #:wait-on-group
           ;; Managing Dispatch Objects
           #:debug
           #:context
           #:release #:retain
           #:resume #:suspend
           #:finalizer
           #:with-object #:with-object-suspended
           ;; Using Semaphores
           #:semaphore
           #:signal-semaphore #:wait-on-semaphore
           #:with-semaphore-held
           ;; Using Barriers
           #:dispatch-barrier-async
           #:dispatch-barrier-sync
           ;; Managing Dispatch Sources
           #:cancel
           #:make-source
           #:data
           #:handle
           #:mask
           #:merge-data
           #:registration-handler #:cancel-handler #:event-handler
           #:set-timer
           #:canceled-p
           #:make-data-add-source
           #:make-data-or-source
           #:make-mach-receive-source
           #:make-mach-send-source
           #:make-process-source
           #:make-read-source
           #:make-signal-source
           #:make-timer-source
           #:make-vnode-source
           #:make-write-source
           #:dead
           #:exit
           #:fork
           #:signal
           #:delete
           #:write
           #:extend
           #:attribute
           #:link-count
           #:rename
           #:revoke
           ;; Managing Time
           #:offset-time
           ;; constants
           #:+now+ #:+forever+))
