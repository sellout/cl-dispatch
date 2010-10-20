cl-dispatch is a Common Lisp wrapper around
[[Grand Central Dispatch|http://en.wikipedia.org/wiki/Grand_Central_Dispatch] (aka
[[libdispatch|http://libdispatch.macosforge.org/]]). It allows you to make lightweight
queues and semaphores which allow the kernel to schedule your tasks across multiple cores.

It's not well documented yet, but the lisp API is in [[cffi.lisp|http://github.com/sellout/cl-dispatch/blob/master/cffi.lisp]], and there is also
[[Apple's documentation of the C API|http://developer.apple.com/library/mac/#documentation/Performance/Reference/GCD_libdispatch_Ref/Reference/reference.html]].
