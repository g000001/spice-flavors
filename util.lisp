(in-package :spiceflavors.internal)

(defun fill* (sequence item &key (start 0) end)
  (if (array-has-fill-pointer-p sequence)
      (let ((fp (fill-pointer sequence)))
        (unwind-protect (progn
                          (setf (fill-pointer sequence)
                                (array-dimension sequence 0))
                          (fill sequence item :start start :end end))
          (setf (fill-pointer sequence)
                fp)))
      (fill sequence item :start start :end end)))
