(in-package :dispatch)

(include "dispatch/dispatch.h")

;;; base

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

;;; queue

(constant (+serial-queue-address+ "DISPATCH_QUEUE_SERIAL"))
(constant (+concurrent-queue-address+ "DISPATCH_QUEUE_CONCURRENT"))

(cenum queue-priority
       ((:high "DISPATCH_QUEUE_PRIORITY_HIGH"))
       ((:default "DISPATCH_QUEUE_PRIORITY_DEFAULT"))
       ((:low "DISPATCH_QUEUE_PRIORITY_LOW"))
       ((:background "DISPATCH_QUEUE_PRIORITY_BACKGROUND")))

;;; source

(cenum (source-type :base-type :pointer :define-constants t)
       ((:data-add-source "DISPATCH_SOURCE_TYPE_DATA_ADD"))
       ((:data-or-source "DISPATCH_SOURCE_TYPE_DATA_OR"))
       ((:mach-receive-source "DISPATCH_SOURCE_TYPE_MACH_RECV"))
       ((:mach-send-source "DISPATCH_SOURCE_TYPE_MACH_SEND"))
       ((:process-source "DISPATCH_SOURCE_TYPE_PROC"))
       ((:read-source "DISPATCH_SOURCE_TYPE_READ"))
       ((:signal-source "DISPATCH_SOURCE_TYPE_SIGNAL"))
       ((:timer-source "DISPATCH_SOURCE_TYPE_TIMER"))
       ((:vnode-source "DISPATCH_SOURCE_TYPE_VNODE"))
       ((:write-source "DISPATCH_SOURCE_TYPE_WRITE")))

#| FIXME: It's failing to find these in the headers, even though they appear to exist
(cenum (mach-send-flags :define-constants t)
       ((:dead "DISPATCH_MACH_SEND_DEAD")))

(cenum (process-flags :define-constants t)
       ((:exit "DISPATCH_PROC_EXIT"))
       ((:fork "DISPATCH_PROC_FORK"))
       ((:exec "DISPATCH_PROC_EXEC"))
       ((:signal "DISPATCH_PROC_SIGNAL")))

(cenum (vnode-flags :define-constants t)
       ((:delete "DISPATCH_VNODE_DELETE"))
       ((:write "DISPATCH_VNODE_WRITE"))
       ((:extend "DISPATCH_VNODE_EXTEND"))
       ((:attribute "DISPATCH_VNODE_ATTRIB"))
       ((:link-count "DISPATCH_VNODE_LINK"))
       ((:rename "DISPATCH_VNODE_RENAME"))
       ((:revoke "DISPATCH_VNODE_REVOKE")))
|#

;;; time

(constant (+now+ "DISPATCH_TIME_NOW"))
(constant (+forever+ "DISPATCH_TIME_FOREVER"))

(ctype time "dispatch_time_t")
