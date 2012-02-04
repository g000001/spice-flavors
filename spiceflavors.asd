;;;; spiceflavors.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :spiceflavors
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "util")
               (:file "kkernel")
               (:file "kernel")
               (:file "flavors")
               (:file "vanilla")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :spiceflavors))))
  (load-system :spiceflavors)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :spiceflavors.internal :spiceflavors))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
