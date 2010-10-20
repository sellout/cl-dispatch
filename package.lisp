(defpackage dispatch
  (:use #:cl #:cffi)
  (:shadow #:function #:time)
  (:export ;; Queuing Tasks for Dispatch
           #:dispatch-after
           #:dispatch-apply
           #:dispatch-async
           #:current-queue
           #:global-queue
           #:main-queue
           #:event-loop ; aka "dispatch_main"
           #:queue
           #:label
           #:target-queue
           #:dispatch-sync
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
           ;; Handling Events
           #:cancel
           #:source
           #:data
           #:handle
           #:mask
           #:merge-data
           #:cancel-handler #:event-handler
           #:set-timer
           #:not-canceled-p
           ;; Managing Time
           #:offset-time
           ;; constants
           #:now #:forever
           #:data-add #:data-or #:mach-receive #:mach-send
           #:proc #:read #:signal #:timer #:vnode #:write
           ;; convenience functions
           #:with-object #:with-semaphore-held))
