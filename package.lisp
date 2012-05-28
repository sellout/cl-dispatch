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
           ;; Using Semaphores
           #:semaphore
           #:signal-semaphore #:wait-on-semaphore
           ;; Using Barriers
           #:dispatch-barrier-async
           #:dispatch-barrier-sync
           ;; Managing Dispatch Sources
           #:cancel
           #:source
           #:data
           #:handle
           #:mask
           #:merge-data
           #:registration-handler #:cancel-handler #:event-handler
           #:set-timer
           #:not-canceled-p
           #:data-add-source
           #:data-or-source
           #:mach-receive-source
           #:mach-send-source
           #:process-source
           #:read-source
           #:signal-source
           #:timer-source
           #:vnode-source
           #:write-source
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
           #:+now+ #:+forever+
           #:data-add #:data-or #:mach-receive #:mach-send
           #:proc #:read #:signal #:timer #:vnode #:write
           ;; convenience functions
           #:with-object #:with-semaphore-held))
