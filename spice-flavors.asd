;;;; spiceflavors.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :spice-flavors
  :version "20200208"
  :description "Spice Lisp Flavors"
  :long-description "Spice Lisp Flavors"
  :author "Steven Handerson"
  :maintainer "CHIBA Masaomi"
  :license "Unlicense"
  :serial t
  :depends-on (:fiveam #+sbcl :sb-cltl2)
  :components ((:file "package")
               (:file "util")
               (:file "kkernel")
               (:file "kernel")
               (:file "flavors")
               (:file "vanilla")
               (:file "test")))


(defmethod perform :after ((o load-op)
                           (c (eql (find-system :spice-flavors))))
  (let ((name "http://pdp-10.trailing-edge.com/clisp#flavors")
        (nickname :flavors))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name
                        name
                        `(,nickname :spice-flavors)))))


(defmethod perform ((o test-op)
                    (c (eql (find-system :spice-flavors))))
  (let ((*package*
         (find-package
          "http://pdp-10.trailing-edge.com/clisp#flavors-internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'spice-flavors)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))

