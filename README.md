cl-dispatch is a Common Lisp wrapper around [Grand Central Dispatch](http://en.wikipedia.org/wiki/Grand_Central_Dispatch) (aka [libdispatch](http://libdispatch.macosforge.org/)). It allows you to make lightweight queues and semaphores which allow the kernel to schedule your tasks across multiple cores.

It's not well documented yet, but the lisp API is in [cffi.lisp](http://github.com/sellout/cl-dispatch/blob/master/cffi.lisp), and there is also [Apple's documentation of the C API](http://developer.apple.com/library/mac/#documentation/Performance/Reference/GCD_libdispatch_Ref/Reference/reference.html).

The lisp functions look and behave like the block versions (EG, `dispatch_sync`) with lisp functions instead of blocks, but they are implemented using the function versions (EG, `dispatch_sync_f`), which means that certain functions (EG, `dispatch_once`) aren't available in our API.

**NOTE**: On at least CCL and SBCL, dispatching asynchronously (this includes `dispatch-after`, etc.) to any queue other than the main queue will likely result in a segfault after not too long. See [CCL issue #911](http://trac.clozure.com/ccl/ticket/911).
