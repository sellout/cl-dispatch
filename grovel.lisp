(in-package :dispatch)

(include "dispatch/dispatch.h")

(cunion object "dispatch_object_t"
        (do "_do" :type :pointer)
        (dc "_dc" :type :pointer)
        (dq "_dq" :type queue)
        (dqa "_dqa" :type queue-attribute)
        (dg "_dg" :type group)
        (ds "_ds" :type source)
        (dsa "_dsa" :type :pointer)
        (de "_de" :type :pointer)
        (da "_da" :type :pointer)
        (dsema "_dsema" :type semaphore))

(cenum queue-priority
       ((:high "DISPATCH_QUEUE_PRIORITY_HIGH"))
       ((:default "DISPATCH_QUEUE_PRIORITY_DEFAULT"))
       ((:low "DISPATCH_QUEUE_PRIORITY_LOW")))

(constant (now "DISPATCH_TIME_NOW"))
(constant (forever "DISPATCH_TIME_FOREVER"))

(constant (data-add "DISPATCH_SOURCE_TYPE_DATA_ADD"))
(constant (data-or "DISPATCH_SOURCE_TYPE_DATA_OR"))
(constant (mach-receive "DISPATCH_SOURCE_TYPE_MACH_RECV"))
(constant (mach-send "DISPATCH_SOURCE_TYPE_MACH_SEND"))
(constant (proc "DISPATCH_SOURCE_TYPE_PROC"))
(constant (read "DISPATCH_SOURCE_TYPE_READ"))
(constant (signal "DISPATCH_SOURCE_TYPE_SIGNAL"))
(constant (timer "DISPATCH_SOURCE_TYPE_TIMER"))
(constant (vnode "DISPATCH_SOURCE_TYPE_VNODE"))
(constant (write "DISPATCH_SOURCE_TYPE_WRITE"))