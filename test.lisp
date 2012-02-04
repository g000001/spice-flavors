(cl:in-package :spiceflavors.internal)

(def-suite spiceflavors)

(in-suite spiceflavors)

(defflavor test-foo (x y z) ()
  :initable-instance-variables
  :settable-instance-variables
  :gettable-instance-variables)

(test flavor
  (let ((ins (make-instance 'test-foo)))
    (is (eq 'unbound (send ins :x)))
    (is (eq 'unbound (send ins :y)))
    (is (eq 'unbound (send ins :z)))
    (send ins :set-x 100)
    (send ins :set-y 200)
    (send ins :set-z 300)
    (is (= 100 (send ins :x)))
    (is (= 200 (send ins :y)))
    (is (= 300 (send ins :z)))) )

(defmethod (test-foo :|* n x|) (n)
  (* n (send self :x)))

(test defmethod
  (let ((i (make-instance 'test-foo :x 1 :y 2 :z 3)))
    (is (= 8 (send i :|* n x| 8)))))

;;; eof
