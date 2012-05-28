(in-package #:dispatch)

;;;      Stolen from CCL's level-1/sysutils.lisp, with minor changes to make
;;;      it portable and to use this library's own semaphores rather than add
;;;      a dependency on Bordeaux Threads.

(defstruct id-map
  (vector (make-array 1 :initial-element nil))
  (free 0)
  (semaphore (make-semaphore 1)))

;;; Caller owns the semaphore on the id-map.
(defun id-map-grow (id-map)
  ;; FIXME: without-interrupts
  (let* ((old-vector (id-map-vector id-map))
         (old-size (length old-vector))
         (new-size (+ old-size old-size))
         (new-vector (make-array new-size)))
    (declare (fixnum old-size new-size))
    (dotimes (i old-size)
      (setf (svref new-vector i) (svref old-vector i)))
    (let* ((limit (1- new-size)))
      (declare (fixnum limit))
      (do* ((i old-size (1+ i)))
           ((= i limit) (setf (svref new-vector i) nil))
        (declare (fixnum i))
        (setf (svref new-vector i) (the fixnum (1+ i)))))
    (setf (id-map-vector id-map) new-vector
          (id-map-free id-map) old-size)
    old-size))

;;; Map an object to a small fixnum ID in id-map.
;;; Object can't be NIL or a fixnum itself.
(defun assign-id-map-id (id-map object)
  (check-type object (not (or null fixnum)))
  (with-semaphore-held ((id-map-semaphore id-map) +forever+)
    (let* ((free (or (id-map-free id-map) (id-map-grow id-map)))
           (vector (id-map-vector id-map))
           (newfree (svref vector free)))
      (setf (id-map-free id-map) newfree
            (svref vector free) object)
      free)))
      
;;; Referemce the object with id ID in ID-MAP.  Leave the object in
;;; the map.
(defun id-map-object (id-map id)
  (let* ((object (with-semaphore-held ((id-map-semaphore id-map) +forever+)
                   (svref (id-map-vector id-map) id))))
    (if (or (null object) (typep object 'fixnum))
        (error "invalid index ~d for ~s" id id-map)
        object)))

;;; Referemce the object with id ID in ID-MAP.  Remove the object from
;;; the map.
(defun id-map-free-object (id-map id)
  (with-semaphore-held ((id-map-semaphore id-map) +forever+)
    (let* ((vector (id-map-vector id-map))
           (object (svref vector id)))
      (if (or (null object) (typep object 'fixnum))
          (error "invalid index ~d for ~s" id id-map))
      (setf (svref vector id) (id-map-free id-map)
            (id-map-free id-map) id)
      object)))

(defun id-map-modify-object (id-map id old-value new-value)
  (with-semaphore-held ((id-map-semaphore id-map) +forever+)
    (let* ((vector (id-map-vector id-map))
           (object (svref vector id)))
      (if (or (null object) (typep object 'fixnum))
          (error "invalid index ~d for ~s" id id-map))
      (if (eq object old-value)
          (setf (svref vector id) new-value)))))
