;;; FLAVORS.SLISP
;;; -*- Mode:clisp; package: Flavors -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC).
;;; **********************************************************************
;;;
;;; Flavors main file.
;;; Written by Steven Handerson.
;;;

;;; Make *remember-wrapper-names* so that we can just remember the hash of
;;; a combined method's wrappers.
;;; MIXTURES.
;;; Write other combination types.
;;; Get-handler-for works on other types?

;;; Things to check: abstract to non-abstract.

#|
(progn (hemlock::defindent "defbits" 0)
       (hemlock::defindent "defwrapper" 0)
       (hemlock::defindent "symbol-macro-let" 0)
       (hemlock::defindent "defun-default-handler" 0)
       (hemlock::defindent "defining-form" 0)
       (hemlock::defindent "handle-message" 0)
       (hemlock::defindent "with-stacks" 0)
       (hemlock::defindent "do-stack-assoc" 0)
       (hemlock::defindent "defdescribe" 0)
       (hemlock::defindent "get-method-types" 0)
       (hemlock::defindent "dovec" 0)
       (hemlock::defindent "with-flavor-instantiated" 0)
       (hemlock::defindent "do-inheriting-flavors" 0)
       (hemlock::defindent "flavor-set-handlers" 0)
       (hemlock::defindent "descriptor-set-handlers" 0)
       (hemlock::defindent "do-methods" 0)
       (hemlock::defindent "do-handlers" 0)
       (hemlock::defindent "mydlet" 0)
       (hemlock::defindent "defflavor" 2)
       (hemlock::defindent "defcombination" 0)
       (hemlock::defindent "defwrapper" 2)
       (hemlock::defindent "defmethod" 2)
       (hemlock::defindent "calculate-all-components" 0)
       (hemlock::defindent "flavor-add-component" 0)
       (hemlock::defindent "flavor-add-included" 0)
       (hemlock::defindent "unless" 0)
       (hemlock::defindent "labels" 0)
       (hemlock::defindent "wrapper-mix" 0)
       (hemlock::defindent "touch-components" 0)
       (hemlock::defindent "touch" 0)
       (hemlock::defindent "get-combination" 0)
       (hemlock::defindent "set-combination" 0)
       (hemlock::defindent "push-method" 0)
       (hemlock::defindent "multiple-value-bind" 2)
       (hemlock::defindent "defsetf" 0)
       (hemlock::defindent "default-set-handlers" 3)
       (hemlock::defindent "define-setf-method" 2)
       )
|#

(in-package :spiceflavors.internal)
;(use-package 'flavor-internals)

#|(export '(send
	  self
	  instancep
	  symeval-in-instance
	  set-in-instance
	  get-handler-for

	  iv-bound-p

	  vanilla-flavor

	  *all-flavor-names*
	  *undefined-flavor-names*
	  *flavor-compile-methods*
	  *default-combination*

	  *dirty-flavors*
	  cleanup-all-flavors
	  cleanup-flavor
	  without-cleaning-flavors

	  defflavor
	  undefflavor
	  defmethod
	  defwrapper
	  defwhopper
	  continue-whopper
	  lexpr-continue-whopper
	  continue-whopper-all
	  undefmethod
	  recompile-flavor
	  make-instance
	  flavor-allowed-init-keywords
	  flavor-allows-init-keyword-p

	  compile-flavor
	  compiler-compile-flavors
	  reconstruct-defflavor

	  method-list
	  call-methods
	  wrapper-mix
	  defcombination-ordering
	  defcombination
	  order-methods
	  order-wrappers))|#

#|(shadow '(nreversef))|#

(eval-when (:compile-toplevel :execute :load-toplevel) ; Eval-when-3


;;;
;;; Random utilities.
;;;

;;; Takes a list of forms and returns values of a list of doc-strings
;;; and declares, and a list of the remaining forms.

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun extract-doc-and-declares (forms)
    (do ((forms forms (cdr forms))
	 (docs nil (cons (car forms) docs)))
	((or (endp forms)
	     (and (not (stringp (car forms)))
		  (not (and (listp (car forms))
			    (eq (caar forms) 'declare)))))
	 (values (nreverse docs) forms))))

  (defmacro mydlet (bindings &body body)
    (let ((runtime nil))
      (dolist (binding bindings)
	(unless (null (Car binding))
	  (let ((list (gensym)))
	    (push `(,list ,@(cdr binding)) runtime)
	    (mapcar #'(lambda (var) (push `(,var (pop ,list)) runtime))
		    (car binding)))))
      (cond ((null runtime) `(progn ,@body))
	    (t `(let* ,(nreverse runtime)
		  ,@body))))))

(defun private-structure-printer (object stream depth)
  (Declare (ignore depth))
  (format stream "#<~A ~A>" (type-of object) (pointer-to-fixnum object)))


;;; Boolean variables shouldn't take up 32 bits.
;;; Syntax and semantics like defstruct.

(eval-when (:compile-toplevel :execute :load-toplevel)
  (Defmacro defbits (str-name &rest names)
    (do ((i 0 (1+ i))
	 (names names (cdr names))
	 (res nil) )
	((null names) `(eval-when (:compile-toplevel :execute :load-toplevel) ,@res))
      (push `(defmacro ,(intern (concatenate 'string (symbol-name str-name)
					     "-" (symbol-name (car names))))
                 (thing)
	       `(logbitp ,,i ,thing) )
	    res )))

  ;; Assoc with a nicer setf method.
  (defmacro my-assoc (key list)
    "Just like simple assoc, but has a nice setf method."
    `(cdr (assoc ,key ,list)) )

  (define-setf-expander my-assoc (key list)
    (multiple-value-bind (temps vals stores store-form access-form)
                         (get-setf-expansion list)
      (let ((ktemp (gensym))
            (list (gensym))
            (assoc (gensym))
            (store (gensym))
            (stemp (first stores)) )
        (values `(,ktemp ,.temps ,list ,assoc)
                `(,key ,.vals ,access-form (assoc ,ktemp ,list))
                (list store)
                `(if ,assoc
                     (setf (cdr ,assoc) ,store)
                     (let ((,stemp (cons (cons ,ktemp ,store) ,list)))
                       ,store-form
                       ,store ))
                `(cdr ,assoc) ))))


  (cl:define-modify-macro nreversef () nreverse)

  (defmacro dovec ((var vec) &body body)
    `(let ((%vec ,vec))
       (dotimes (%i (length %vec))
	 (let ((,var (aref %vec %i)))
	   ,@body )))))


;;; Stacks are just dynamically allocated vectors with fill-pointers.

(defvar %stacks% (make-array 100 :adjustable t :fill-pointer 0))


(eval-when (:compile-toplevel :execute :load-toplevel)
  (defmacro with-stacks (names &body body)
    (multiple-value-bind (docs forms) (extract-doc-and-declares body)
      `(let ,(mapcar #'(lambda (name)
			 `(,name (cond ((> (length %stacks%) 0)
					(let ((res (vector-pop %stacks%)))
					  (setf (fill-pointer res) 0)
					  res))
				       (t (make-array 1000 :adjustable t
						      :fill-pointer 0)))))
		     names)
	 ,@docs
	 (unwind-protect
	   (progn ,@forms)
           ,@(mapcar #'(lambda (name)
                         `(progn
                            (fill* ,name nil :end (array-dimension ,name 0))
                            (vector-push-extend ,name %stacks%)))
                     names)
	   #|,@(mapcar #'(lambda (name)

                         (let ((name-fp (gensym)))
			 `(let ((,name-fp (fill-pointer ,name)))
                            (unwind-protect
                              (progn
                                (setf (fill-pointer ,name)
                                      (array-dimension ,name 0))
                                (fill ,name nil :end (array-dimension ,name 0))
                                (vector-push-extend ,name %stacks%))
                              (setf (fill-pointer ,name)
                                    ,name-fp)))))
		     names)|#))))

  (defmacro set-stack-length (stack size)
    `(let ((stack ,stack)
	   (size ,size))
       (cond ((>= (array-dimension stack 0) size)
	      (setf (fill-pointer stack) size))
	     (t (adjust-array stack size)
		(setf (fill-pointer stack) size))))))

;;; Yech.  Still, if this is gonna get used as much as I think...

(defvar *changed-method-stacks*
  (let ((res (make-array 100 :fill-pointer t :adjustable t)))
    (dotimes (i 100)
      (setf (aref res i) (make-array 10 :adjustable t :fill-pointer 0)))
    res)
  "A bunch of small vectors used to record which methods need recalculating
  for a flavor.")

(defun alloc-tiny-stack ()
  (or (vector-pop *changed-method-stacks*)
      (make-array 10 :adjustable t :fill-pointer 0)))

(defmacro dealloc-tiny-stack (place)
  `(let ((%x (shiftf ,place nil)))
     (when %x
       (fill* %x nil :end (array-dimension %x 0))
       (vector-push-extend %x *changed-method-stacks*))))



;;;
;;; More specific to Flavors.
;;;


(defmacro flavor-function-name (symbol &rest things)
  "Usually called with flavor-name, method, and method-type.
  Interns the name in the package of the first thing (a symbol)."
  `(intern (concatenate
	    'string (symbol-name ,symbol)
	    ,@(mapcan #'(lambda (thing)
			  `("-"  (let ((thing ,thing))
				   (if (symbolp thing)
				       (let ((*print-case* :upcase))
					 (prin1-to-string thing))
				       thing))))
		      things))
	   (symbol-package ,symbol)))


(defun set-name (iv)
  (cond ((get iv 'set-name))
	(t (let ((res (intern (concatenate 'string "SET-" (symbol-name iv))
			      (find-package 'keyword))))
	     (setf (Get iv 'set-name) res)
	     res))))

(DEfun get-name (iv)
  (cond ((get iv 'get-name))
	(t (let ((res (intern (symbol-name iv) (find-package 'keyword))))
	     (setf (get iv 'get-name) res)
	     res))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defmacro combination-ordering (name)
    `(let ((%combo ,name))
       (or (get %combo 'ordering)
	   (error "No such combination: ~S." %combo))))

  (defmacro combination-mixer (name)
    `(let ((%combo ,name))
       (or (get %combo 'mixer)
	   (error "No such combination: ~S." %combo))))

  (defmacro make-combination (name ordering mixer)
    `(progn (setf (get ,name 'ordering) ,ordering
		  (get ,name 'mixer) ,mixer))))

;;;
;;; Environments.
;;;

;;; Special values for the default: REQUIRED and UNSUPPLIED.


(defstruct (method-env (:print-function private-structure-printer)
		       (:include iv-env))
  numordered ; number of vars ordered.
  defaults   ; default forms.
  ables)

(defstruct (instance-env (:print-function private-structure-printer)
			 (:include method-env))
  required) ; list of required ivs.

(defun instance-env-vector (inst-env)			;a mixup somewhere
  (instance-env-required inst-env))			;I guess/Cons

(defbits ables
  gettable
  settable
  initable
  outside-accessible)


(eval-when (:compile-toplevel :execute :load-toplevel)
  (defmacro var-able (x)
    `(aref ables
	   (or (position ,x var-stack)
	       (error "No such instance variable - ~S." ,x)))))

(defun make-env (var-list ordered required
			  settable gettable initable outside)
  (with-stacks (var-stack default-stack)
    (dolist (o ordered)
      (vector-push-extend o var-stack)
      (vector-push-extend 'UNSUPPLIED default-stack))
    (let ((numordered (length var-stack))
	  temp)
      (dolist (var var-list)
	(cond ((listp var)
	       (cond ((Setq temp (position (car var) var-stack))
		      (setf (aref default-stack temp) (cadr var)))
		     (t (vector-push-extend (car var) var-stack)
			(vector-push-extend (cadr var) default-stack))))
	      (t (cond ((setq temp (position var var-stack))
			(setf (Aref default-stack temp) 'UNSUPPLIED))
		       (t (vector-push-extend var var-stack)
			  (vector-push-extend 'UNSUPPLIED default-stack))))))
      (dolist (req required)
	(cond ((find req var-stack))
	      (t (vector-push-extend req var-stack)
		 (vector-push-extend 'REQUIRED default-stack))))
      (let* ((ables (make-array (length default-stack) :initial-element 0)))
	(dolist (s settable) (setf (ables-settable (var-able s)) t
				   (ables-gettable (var-able s)) t
				   (ables-initable (var-able s)) t))
	(dolist (g gettable) (setf (ables-gettable (var-able g)) t))
	(dolist (i initable) (setf (ables-initable (var-able i)) t))
	(dolist (o outside) (setf (ables-outside-accessible (var-able o)) t))
	(make-method-env :numordered numordered
			 :vector (copy-seq var-stack)
			 :defaults (copy-seq default-stack)
			 :ables ables)))))


;;;
;;; Flavor definition.
;;;

(defun-default-handler default-handler (&rest args)
  (let ((message (get-message)))
    (cond ((get-handler :unhandled-message (instance-descriptor self))
	   (apply #'send self :unhandled-message message args))
	  ((get-handler :print-self (instance-descriptor self))
	   (error "Unhandled message ~S in instance ~S." message self))
	  (t (error "Unhandled message ~S in #<Random Instance ~S>."
		    message (pointer-to-fixnum self))))))

;;; Exported specials.

(defvar *all-flavor-names* () "The list of all defined flavors.")
(defvar *undefined-flavor-names* ()
  "List of referred-to but not defflavored flavors.")
(defvar *dirty-flavors* (make-array 100 :fill-pointer 0 :adjustable t)
  "A vector (with fill pointer) of instantiated flavors that need work.")

(defvar *flavor-compile-methods* T
  "If T, new combined methods will automatically be compiled.")

(Defvar *default-combination*'(:daemon . :base-flavor-last)
  "Something like (:daemon . :base-flavor-last).")

;;;
;;; Internal specials.
;;;

;;; Options that alone mean all of the instance variables.
(Defparameter *var-options*
  '(:gettable-instance-variables
    :settable-instance-variables :initable-instance-variables
    :ordered-instance-variables :outside-accessible-instance-variables))

(defbits changed
  components		; Recompute all-components, compute everything if change.
  required-flavors	; Check if we're instantiable.
  required-ivs          ; Ditto.
  iv-order              ; Maybe important to the kernel.
  iv-inits              ; Someday the inits will be in a function.
  all-methods           ; We only stay up-to-date if instantiated.  Redo all.
  required-methods      ; See if instantiable.
  required-inits	; Recompute cached quantities after we know it's inst.
  default-plist		;
  init-keywords
  iv-keywords)

(defconstant %all-components-changed% #b11111111110)

(defbits flags
  vanilla-p
  abstract-p
  defined-p		; Defflavored.
  compiled-p		; Compiled and non-abstract means instantiable.
  			; Compiled and abstract means compiled methods.
  wayward-p)            ; Has instances but is currently uninstantiable.


(defstruct (flavor (:print-function print-flavor))
  name
  (components nil)
  (included-flavors nil)
  (required-flavors nil)
  (required-methods nil)
  (default-plist nil)
  (init-keywords nil)
  (required-inits nil)
  (method-env nil)
  (combinations nil)	; Assoc of name to combination
  (prefix nil)

  (descriptor nil)
  (required-inits* nil)
  (init-keywords* nil)
  (default-plist* nil)
  (iv-keywords* nil)	; Assoc of var to position?

  (dependents nil)
  (changed 1)
  (flags 0)
  (methods (make-method-structure))
  (all-components+ nil)
  (instance-env nil)
  (changed-methods nil)) ; Vector of lists.

(defun print-flavor (object stream depth)
  (declare (ignore depth))
  (format stream "#<Flavor ~S>" (flavor-name object)))

(eval-when (:compile-toplevel :execute :load-toplevel)

  (defmacro flavor-defined-p (flavor)
    `(flags-defined-p (flavor-flags ,flavor)))
  (defmacro flavor-has-vanilla-p (flavor)
    `(flags-vanilla-p (flavor-flags ,flavor)))
  (defmacro flavor-abstract-p (flavor)
    `(flags-abstract-p (flavor-flags ,flavor)))
  (defmacro flavor-compiled-p (flavor)
    `(and (flavor-defined-p ,flavor)
	  (flags-compiled-p (flavor-flags ,flavor))))
  (defsetf flavor-compiled-p (flavor) (new)
    `(setf (flags-compiled-p (flavor-flags ,flavor)) ,new))
  (defmacro flavor-wayward-p (flavor)
    `(flags-wayward-p (flavor-flags ,flavor)))

  (defmacro flavor-instantiated-p (flavor)
    "Returns T for instantiated and wayward flavors."
    `(or (flavor-wayward-p ,flavor)
	 (and (flavor-descriptor ,flavor)
	      (instance-descriptor-instantiated-p (flavor-descriptor ,flavor)))))

  (defmacro get-flavor (name &optional createp)
    `(let ((name ,name))
       (cond ((get name 'flavor))
             ,@(if createp
                   `((,createp (let ((res (make-flavor :name name)))
                                 (setf (get name 'flavor) res)
                                 (pushnew name *undefined-flavor-names*)
                                 res))))
	     (t (error "Flavor ~S does not exist." name)))))

  (defmacro flavor-dirty-p (flavor)
    `(or (plusp (flavor-changed ,flavor))
	 (flavor-changed-methods ,flavor)))

  (defmacro rework-flavor (flavor)
    `(let ((flavor ,flavor))
       (when (and (flavor-instantiated-p flavor)
		  (not (flavor-dirty-p flavor)))
	 (vector-push-extend flavor *dirty-flavors*))))

  (defmacro rework-methods (flavor methods)
    `(let ((flavor ,flavor))
       (cond ((null (flavor-changed-methods flavor))
	      (let ((stack (alloc-tiny-stack)))
		(vector-push-extend ,methods stack)
		(setf (flavor-changed-methods flavor) stack)))
	     (t (vector-push-extend ,methods (flavor-changed-methods flavor))))))

  (defmacro do-inheriting-flavors ((var flavor &optional stack) &body body)
    `(cond ((eq (flavor-name flavor) 'vanilla-flavor)
	    (dolist (fl *all-flavor-names*)
	      (let ((,var (get-flavor fl)))
		,@(if stack `((vector-push-extend ,var ,stack)))
		(when (flags-vanilla-p (flavor-flags ,var))
		  ,@body))))
	   (t ,(let* ((stack (or stack '%inheriting-flavors))
		      (body `((vector-push-extend ,flavor ,stack)
			      (do ((%i 0 (1+ %i)))
				  ((>= %i (length ,stack)))
				(let ((,var (aref ,stack %i)))
				  ,@body)
				(dolist (d (flavor-dependents (aref ,stack %i)))
				  (unless (position d ,stack)
				    (vector-push-extend d ,stack)))))))
		 (if (eq stack '%inheriting-flavors)
		     `(with-stacks (,stack) ,@body)
		     `(progn ,@body)))))))


(Defun message-clean-p (flavor method)
  (not (or (changed-all-methods (flavor-changed flavor))
	   (let ((stack (flavor-changed-methods flavor)))
	     (dotimes (i (length stack))
	       (let ((elt (aref stack i)))
		 (when (or (eq method elt) (member method elt))
		   (return t))))))))

#|(defdescribe internal-describe-flavor flavor (flavor)
  (when flavor
    (cond ((flavor-dependents flavor)
	   (terpri)
	   (princ "It is a direct component of flavors: ")
	   (princ (mapcar #'flavor-name (flavor-dependents flavor))))
	  (t (terpri) (princ "No flavors currently refer to it.")))
    (Cond ((not (flavor-defined-p flavor))
	   (terpri)
	   (princ "It's hasn't been defflavored yet."))
	  (t (terpri)
	     (princ "It was defflavored something like this:")
	     (terpri)
	     (write (reconstruct-defflavor (flavor-name flavor)) :pretty t
		    :length nil :level nil)
	     (when (flavor-compiled-p flavor)
	       (cond ((flavor-dirty-p flavor)
		      (terpri) (princ "It's compiled, but dirty."))
		     (t (terpri) (princ "It's currently compiled.")))
	       (cond ((flavor-wayward-p flavor)
		      (terpri) (princ "It has wayward instances."))
		     ((flavor-instantiated-p flavor)
		      (terpri) (princ "It is instantiated."))))))))|#

(defun reconstruct-defflavor (name)
  "Returns a call to defflavor that was something pretty close to how
  the flavor was defined."
  (let* ((flavor (get-flavor name)))
    (unless (Flavor-defined-p flavor)
      (error "Flavor ~S has not been defflavored." name))
    (let* ((env  (flavor-method-env flavor))
	   (vec (iv-env-vector env))
	   (defaults (method-env-defaults env))
	   (ables (method-env-ables (flavor-method-env flavor)))
	   (vars nil) (initable nil) (settable nil) (gettable nil)
	   (required nil) (ordered nil) (accessible nil)
	   (options nil))
      (let ((numordered (method-env-numordered env)))
	(dotimes (i (length vec))
	  (let ((able (aref ables i))
		(var (aref vec i))
		(def (aref defaults i)))
	    (case def
	      (UNSUPPLIED (push var vars))
	      (REQUIRED (push var required))
	      (t (push (list var def) vars)))
	    (cond ((ables-settable able) (push var settable))
		  (t (when (ables-initable able) (push var initable))
		     (when (ables-gettable able) (push var gettable))))
	    (when (< i numordered) (push var ordered))
	    (when (ables-outside-accessible able) (push var accessible)))))
      (if initable (push `(:initable-instance-variables ,@initable) options))
      (if settable (push `(:settable-instance-variables ,@settable) options))
      (if gettable (push `(:gettable-instance-variables ,@gettable) options))
      (if required (push `(:required-instance-variables ,@required) options))
      (if ordered (push `(:ordered-instance-variables ,@(nreverse ordered))
			options))
      (if accessible
	  (push `(:outside-accessible-instance-variables ,@accessible) options))
      (if (not (flavor-has-vanilla-p flavor)) (push :no-vanilla-flavor options))
      (if (flavor-abstract-p flavor) (push :abstract-flavor options))
      (if (flavor-init-keywords flavor)
	  (push `(:init-keywords ,@(flavor-init-keywords flavor)) options))
      (if (flavor-default-plist flavor)
	  (push `(:default-init-plist ,@(flavor-default-plist flavor)) options))
      (if (flavor-required-inits flavor)
	  (push `(:required-init-keywords ,@(flavor-required-inits flavor))
		options))
      (if (flavor-required-methods flavor)
	  (push `(:required-methods ,@(flavor-required-methods flavor)) options))
      (if (Flavor-required-flavors flavor)
	  (push `(:required-flavors ,@(flavor-required-flavors flavor)) options))
      (if (flavor-included-flavors flavor)
	  (push `(:included-flavors ,@(flavor-included-flavors flavor)) options))
      (if (not (equalp (flavor-prefix flavor)
		       (concatenate 'string (symbol-name (flavor-name flavor))
				    "-")))
	  (push `(:accessor-prefix ,(intern (flavor-prefix flavor)
					    (symbol-package (flavor-name flavor))))
		options))
      (when (flavor-combinations flavor)
	(let ((combos nil))
	  (dolist (cons (flavor-combinations flavor))
	    (push (car cons) (my-assoc (cdr cons) combos)))
	  (push `(:method-combination
		  ,@(mapcar #'(lambda (list)
				`(,(caar list) ,(cdar list) ,@(cdr list)))
			    combos))
		options)))
      (when (documentation (flavor-name flavor) 'flavor)
	(push `(:documentation ,(documentation (flavor-name flavor) 'flavor))
	      options))
      `(defflavor ,(flavor-name flavor) ,(nreverse vars)
	 ,(flavor-components flavor)
	 ,@options))))



;;;
;;; Method structures
;;;

(defstruct (method-structure (:print-function private-structure-printer)
			     (:constructor make-method-structure
					   (&optional methods)))
  methods
  (types (make-list (length methods))))

(defun %get-method-types (name structure &optional create)
  "Returns a list whose car is the list of types."
  (let ((pos (position name (method-structure-methods structure))))
    (if pos (nthcdr pos (method-structure-types structure))
	(when create
	  (push name (method-structure-methods structure))
	  (push nil (method-structure-types structure))))))

(eval-when (:compile-toplevel :execute :load-toplevel)

  (defmacro get-method-types (name structure &optional create)
    `(%get-method-types ,name ,structure ,create))

  (defmacro do-methods ((var structure) &body body)
    `(let ((%str ,structure))
       (macrolet ((get-method-types (name str &optional create)
		    (cond ((and (eq name ',var) (eq str ',structure)) '(car %list))
			  (t `(%get-method-types ,name ,str ,create)))))
	 (do ((%m (method-structure-methods %str) (cdr %m))
	      (%list (method-structure-types %str) (cdr %list)))
	     ((null %m))
	   (unless (null (car %list))
	     (let ((,var (car %m)))
	       ,@body))))))

  (defmacro method-types (name structure)
    `(car (get-method-types ,name ,structure)))

  (defsetf method-types (name structure) (new)
    `(and ,new
	  (setf (car (get-method-types ,name ,structure t)) ,new))))

(defun method-add (name type fn-name structure)
  (let* ((list (get-method-types name structure t)))
    (let ((assoc (assoc type (car list))))
      (cond (assoc
	     (cond ((eq (cdr assoc) fn-name) nil)
		   (t (SEtf (cdr assoc) fn-name) t)))
	    (t (push (cons type fn-name) (car list))
	       t)))))

(defun Method-find (name type structure)
  (let ((list (get-method-types name structure)))
    (and list (cdr (assoc type (car list))))))

;;;
;;; Components, environment
;;;


;;; First we do a depth-first walk of the components.
;;; Into this list we insert all included flavors and their not-already-present
;;; components after the last flavor to include them.
;;; @#@# The order in which this is done is important: do we add includeds from
;;;  the end or from the beginning?
;;; Lastly, we add vanilla-flavor unless a components says not to.

(defun calculate-all-components (flavor undefined)
  (with-stacks (components-stack second-stack)
    (let ((undefined-flavors nil) (undefined-includeds nil))
      (labels ((flavor-add-component (flavor)
		 (vector-push-extend flavor components-stack)
		 (dolist (c (flavor-components flavor))
		   (setq c (get-flavor c t))
		   (cond ((flavor-defined-p c)
			  (unless (find c components-stack)
			    (flavor-add-component c)))
			 (t (push (flavor-name c) undefined-flavors))))))
	(flavor-add-component flavor)
	(dotimes (i (length components-stack))
	  (labels
	    ((flavor-add-included (flavor)
	       (cond ((not (flavor-defined-p flavor))
		      (push (flavor-name flavor) undefined-includeds))
		     ((or (find flavor components-stack :start i)
			  (find flavor second-stack)))
		     (t (vector-push-extend flavor second-stack)
			(dolist (c (flavor-components flavor))
			  (flavor-add-included (get-flavor c t)))
			(dolist (incl (flavor-included-flavors flavor))
			  (flavor-add-included (get-flavor incl t)))))))
	    (let ((flav (aref components-stack i)))
	      (vector-push-extend flav second-stack)
	      (dolist (incl (flavor-included-flavors flav))
		(unless
		  (find incl components-stack :test
			#'(lambda (incl c)
			    (member incl (flavor-included-flavors c))))
		  (flavor-add-included incl))))))
	(unless (find-if #'(lambda (c) (not (flavor-has-vanilla-p c)))
			 second-stack)
	  (vector-push-extend (get-flavor 'vanilla-flavor) second-stack)))
      (cond ((car undefined-flavors)
	     (funcall undefined (flavor-name flavor) undefined-flavors))
	    (T (when (car undefined-includeds)
		 (format *error-output* "Undefined included flavors ignored - ~S."
			 undefined-includeds))
	       (coerce second-stack 'list))))))

;;; We make one pass to get the ordered instance variables; any flavor
;;; that specifies ordered variables should list the same ones
;;; first as any other.
;;; Required variables (those with 'REQUIRED as a default) get
;;; replaced in the stack with the first real iv encountered.
;;; @#@# Generally the first iv to supply a default sets it?
;;; Ables flags (initable, settable, etc.) are ored together.


(defun calculate-instance-env (flavor)
  (with-stacks (ables-stack default-stack variables-stack)
    (let* ((ordered 0) (ovars '#()) oflavor temp)
      (dolist (flavor (flavor-all-components flavor))
	(let* ((env (flavor-method-env flavor))
	       (newnum (method-env-numordered env))
	       (newvars (iv-env-vector env))
	       (diff (mismatch newvars ovars :end2 ordered :end1 ordered)))
	  (macrolet
	    ((diff ()
	       '(if diff
		    (error "Ordered variable ~S in flavor ~S conflicts ~
			   with ordered variable ~S in flavor ~S."
                     (aref ovars diff) (and oflavor (flavor-name oflavor))
                     (aref newvars diff) (flavor-name flavor)))))
	    (cond ((> newnum ordered)
		   (diff)
                   (setq ovars newvars ordered newnum))
		  (t (diff))))))
      (setf (fill-pointer variables-stack) ordered
	    (fill-pointer default-stack) ordered
	    (fill-pointer ables-stack) ordered)
      (replace variables-stack ovars :end1 ordered)
      (fill* default-stack 'UNSUPPLIED)
      (fill* ables-stack 0)
      (dolist (flavor (flavor-all-components flavor))
	(let* ((env (flavor-method-env flavor))
	       (vec (iv-env-vector env)))
	  (dotimes (i (length vec))
	    (let ((var (aref vec i)))
	      (cond ((setq temp (position var variables-stack))
		     (when (eq 'UNSUPPLIED (aref default-stack temp))
		       (setf (Aref default-stack temp)
			     (aref (method-env-defaults env) i)
			     (aref ables-stack temp)
			     (logior (aref (method-env-ables env) i)
				     (aref ables-stack temp)))))
		    (t (vector-push-extend var variables-stack)
		       (vector-push-extend (aref (method-env-defaults env) i)
					   default-stack)
		       (vector-push-extend (aref (method-env-ables env) i)
					   ables-stack)))))))
      (make-instance-env :numordered ordered
			 :vector (copy-seq variables-stack)
			 :defaults (copy-seq default-stack)
			 :ables (copy-seq ables-stack)
			 :required
			 (let (res)
			   (dotimes (i (length default-stack))
			     (when (eq 'REQUIRED (aref default-stack i))
			       (push (aref variables-stack i) res)))
			   res)))))

;;;
;;; Defflavor
;;;

;;; %flavor-forms calculates the currently valid accessor forms (etc.?)
;;; Later operation can figure out which ones are no longer valid.

(defmacro defflavor (flavor-name ivs components &rest options)
  "(flavor-name iv-list component-list . options)
  Refer to the flavor documentation for details."
  (%defflavor flavor-name ivs components options)
  `(progn (eval-when (:load-toplevel)
	    (%defflavor ',flavor-name ',ivs ',components ',options))
	  ,.(%flavor-forms flavor-name)
	  ',flavor-name))


;;; Constructs the list of defstruct-like accessor definitions.

(Defun %flavor-forms (flavor-name)
  (let* ((flavor (get-flavor flavor-name))
	 (prefix (flavor-prefix flavor))
	 (env (flavor-method-env flavor))
	 (vec (iv-env-vector env))
	 (numordered (method-env-numordered env))
	 (ables (method-env-ables env))
	 (defaults (method-env-defaults env))
	 (forms nil))
    (do ((inits nil)
	 (i (1- (length vec)) (1- i)))
	((minusp i)
	 (cons `(defmethod (,flavor-name INTERNAL-INIT) () ,@inits)
	       forms))
      (let ((def (aref defaults i))
	    (var (aref vec i)))
	(unless (or (eq def 'REQUIRED) (eq def 'UNSUPPLIED))
	  (push `(when (slot-unbound-p self ,i)
		   (setq ,var ,def))
		inits))
	(when (ables-outside-accessible (aref ables i))
	  (let* ((print-name (symbol-name (aref vec i)))
		 (name (intern (concatenate 'string prefix print-name)
			       (symbol-package (flavor-name flavor)))))
	    (cond
	     ((< i numordered)
	      (push `(progn
		      (proclaim '(inline ,name))
		      (defun ,name (self) (instance-ref self ,i))
		      (defsetf ,name (self) (new)
			`(setf (instance-ref ,self ,,i) ,new)))
		    forms))
	     (t (let ((set (make-symbol (concatenate 'string "%SET-" print-name)))
		      (get (make-symbol (concatenate 'string "%GET-" print-name)))
		      (var (aref vec i)))
		  (push `(progn
			  (proclaim '(inline ,name))
			  (defun ,name (self) (send self ',get))
			  (defsetf ,name (self) (new) `(send ,self ',',set ,new))
			  (defmethod (,flavor-name ,get) () ,var)
			  (defmethod (,flavor-name ,set) (new) (setq ,var new)))
			forms))))))))))



(defun %defflavor (flavor-name ivs components options)
  (let* ((flavor (get-flavor flavor-name t))
	 (iv-list (mapcar #'(lambda (x) (if (listp x) (car x) x)) ivs))
	 (old-components (flavor-components flavor))
	 (old-includeds (flavor-included-flavors flavor))
	 included-flavors
	 (vanilla-p t)
	 (changed 0)
	 (abstract-p nil)
	 (combinations '((INTERNAL-INIT :progn . :base-flavor-last)))
	 changed-methods
	 ordered required settable gettable initable outside
	 env required-methods required-flavors
	 default-plist init-keywords required-inits prefix
	 args)
    (dolist (opt options)
      (cond ((listp opt) (setq args (cdr opt) opt (car opt)))
	    ((member opt *var-options*) (setq args iv-list))
	    (t (setq args t)))
      (case opt
	(:method-order nil)
	(:required-instance-variables (Setq required args))
	(:ordered-instance-variables (setq ordered args))
	(:settable-instance-variables (setq settable args))
	(:gettable-instance-variables (setq gettable args))
	(:initable-instance-variables (setq initable args))
	(:outside-accessible-instance-variables (setq outside args))
	(:no-vanilla-flavor (setq vanilla-p nil))
	(:abstract-flavor (setq abstract-p t))
	(:documentation (setf (documentation flavor-name 'flavor) (car args)))
	(:accessor-prefix (setq prefix (symbol-name (Car args))))
	(:required-methods (setq required-methods args))
	(:required-flavors (setq required-flavors args))
	(:included-flavors (setq included-flavors args))
	(:init-keywords (setq init-keywords args))
	(:default-init-plist (setq default-plist args))
	(:required-init-keywords (setq required-inits args))
	(:method-combination
	 (dolist (ordering args)
	   (let ((combination (cons (car ordering) (cadr ordering))))
	     (dolist (method (cddr ordering))
	       (push (cons method combination) combinations)))))
	(t (error "Unknown defflavor option ~S." opt))))

    (setq env (make-env ivs ordered required settable gettable initable outside)
	  prefix (or prefix (concatenate 'string (symbol-name flavor-name) "-")))


    (when (not (and (equal components old-components)
		    (equal included-flavors old-includeds)
		    (eq vanilla-p (flavor-has-vanilla-p flavor))
		    (flavor-defined-p flavor))) ; If we're an undefined included.
      (setf (changed-components changed) t)
      (do-inheriting-flavors (i flavor)
	(rework-flavor i)
	(setf (changed-components (flavor-changed i)) t))
      (setf (flavor-included-flavors flavor) included-flavors
	    (flavor-components flavor) components
	    (flavor-has-vanilla-p flavor) vanilla-p)
      (dolist (c components)
	(pushnew flavor (flavor-dependents (Get-flavor c)))))

    (setf (changed-required-flavors changed)
	  (not (equal required-flavors (flavor-required-flavors flavor)))
	  (changed-required-methods changed)
	  (not (equal required-methods (flavor-required-methods flavor)))
	  (changed-default-plist changed)
	  (not (equal default-plist (flavor-default-plist flavor)))
	  (changed-init-keywords changed)
	  (not (equal init-keywords (flavor-init-keywords flavor)))
	  (changed-required-inits changed)
	  (not (Equal required-inits (flavor-required-inits flavor))))

    ;; This has been done pretty lazily.
    (let* ((old-env (flavor-method-env flavor)))
      (cond ((null old-env))
	    ((not (equalp (iv-env-vector env) (iv-env-vector old-env)))
	     (setf (changed-iv-order changed) t
		   (changed-required-ivs changed) t
		   (changed-iv-inits changed) t))
	    (t (let ((old-env-ables (method-env-ables old-env))
		     (env-ables (method-env-ables env)))
		 (dotimes (i (length env-ables))
		   (when (not (eq (ables-initable (aref env-ables i))
				  (ables-initable (aref old-env-ables i))))
		     (setf (changed-iv-keywords changed) t)))))))

    (dolist (c (flavor-combinations flavor))
      (when (not (member c combinations :test #'equal))
	(pushnew (car c) changed-methods)))
    (dolist (c combinations)
      (when (not (member c (flavor-combinations flavor) :test #'equal))
	(pushnew (car c) changed-methods)))

    (when (and (not (flavor-abstract-p flavor))
	       abstract-p
	       (flavor-instantiated-p flavor)
	       (not (flavor-wayward-p flavor)))
      (setf (flavor-wayward-p flavor) t)
      (format *error-output* "Instances of flavor ~S temporarily dissociated:~%~
	      it is now an abstract flavor."
	      (flavor-name flavor))
      (setf (flavor-abstract-p flavor) t)
      (freeze-instances (flavor-descriptor flavor)))
    (when (and (flavor-abstract-p flavor)
	       (not abstract-p))
      (setq changed (logior %all-components-changed% changed))
      (setf (flavor-changed flavor) %all-components-changed%
	    (flavor-abstract-p flavor) nil))
    (let ((defined-methods (flavor-methods flavor)))
      (dolist (s settable)
	(let ((method (get-name s)))
	  (unless (method-find method :primary defined-methods)
	    (let ((fn (flavor-function-name flavor-name method :primary)))
	      (define-get-method fn s)
	      (method-add method :primary fn defined-methods)
	      (push method changed-methods))))
	(let ((method (set-name s)))
	  (unless (method-find method :primary defined-methods)
	    (let ((fn (flavor-function-name flavor-name method :primary)))
	      (define-set-method fn s)
	      (method-add method :primary fn defined-methods)
	      (push method changed-methods)))))
      (dolist (g gettable)
	(let ((method (get-name g)))
	  (unless (method-find method :primary defined-methods)
	    (let ((fn (flavor-function-name flavor-name method :primary)))
	      (define-get-method fn g)
	      (method-add method :primary fn defined-methods)
	      (push method changed-methods)))))
      (when (flavor-defined-p flavor)
	(let* ((ables (method-env-ables env))
	       (vec (iv-env-vector env)))
	  (dotimes (i (length vec))
	    (let ((var (aref vec i)))
	      (when (and (ables-settable (aref ables i))
			 (not (member var settable)))
		(let ((name (set-name var)))
                  (setf (method-types name defined-methods)
                        (delete :primary (method-types name defined-methods)
                                :key #'car))
		  (push name changed-methods)))
	      (when (and (ables-gettable (aref ables i))
			 (not (member var gettable)))
		(let ((name (get-name var)))
                  (setf (method-types name defined-methods)
                        (delete :primary (method-types name defined-methods)
                                :key #'car))
		  (push name changed-methods))))))))

    (macrolet ((doit ()
		     '(setf (flavor-changed flavor) changed
			    (flavor-method-env flavor) env
			    (flavor-combinations flavor) combinations
			    (flavor-required-methods flavor) required-methods
			    (flavor-required-flavors flavor) required-flavors
			    (flavor-prefix flavor) prefix
			    (flavor-default-plist flavor) default-plist
			    (flavor-init-keywords flavor) init-keywords
			    (flavor-required-inits flavor) required-inits
			    (flavor-defined-p flavor) t)))
      (cond ((not (And (zerop changed) (null changed-methods)))
	     (with-stacks (affected)
	       (do-inheriting-flavors (i flavor affected)
		 (rework-flavor i)
		 (setf (flavor-changed i) (logior changed (flavor-changed i)))
		 (if changed-methods (rework-methods i changed-methods)))
	       (doit)
	       (with-stacks (ordered)
		 (order-flavors affected ordered)
		 (dotimes (i (length ordered))
		   (cleanup-flavor (aref ordered i))))))
	    (t (doit)))
      (pushnew (flavor-name flavor) *all-flavor-names*)
      (delete (flavor-name flavor) *undefined-flavor-names*)) ))
;;;
;;;
;;; This is the heart of flavors - the routine that calculates the various
;;; parts of a flavor in sequence.  Flavor-all-components is split off
;;; so that it can be used in other places.
;;;


(defvar *cleanup-enable* T "If nil, cleanup is suppressed.")

(defmacro without-cleaning-flavors (&body forms)
  "Suppresses heavy flavors calculation inside the body.  Useful when
  defining a series of wrappers or something, especially if you have
  *flavor-compile-methods* set to T."
  `(progn (let ((*cleanup-enable* nil))
	    ,@forms)
	  (setq *cleanup-enable* nil)))

(defvar *flavors-compile* nil) ; If T, not just cleaning up.
;; Used by compiler-compile-flavors, so it can set the handlers itself.")
(defvar *dont-do-methods* nil)

(defvar *uninstantiable* nil) ; (flavor why-string &rest why-args)
(defvar *inheritablep* nil) ; (flavor message) --> T, nil, or :redefine
(defvar *remap* nil) ; (flavor)
(defvar *definer* nil) ; (message form)
(defvar *set-handlers* nil) ; (flavor name-stack fn-stack all-p)
;; Note: nil means no handler

(defvar *methods* nil) ; Assoc of name to assoc of slot to list of methods.
(defvar *description* nil) ; A vector of slot seqs, pushed onto by order-wrappers
(defvar *called-methods* nil) ; A stack that gets push-newed onto.
(defvar *message* nil) ; Used to pass message to the ordering functions.


;;;
;;; Stuff used in cleaning up.
;;;


(defun dissociate-instances (flavor why &rest why-args)
  (cond ((and (flavor-instantiated-p flavor)
	      (not (flavor-wayward-p flavor)))
	 (setf (flavor-wayward-p flavor) t)
	 (format *error-output*
		 "Instances of flavor ~S temporarily dissociated:~%"
		 (flavor-name flavor))
	 (apply #'format *error-output* why why-args)
	 (let ((descriptor (flavor-descriptor flavor)))
	   (freeze-instances descriptor)))
	(*flavors-compile*
	 (format *error-output*
		 "Could not instantiated flavor ~S:~%" flavor)
	 (apply #'format *error-output* why why-args))))

(defun descriptor-set-handlers (descriptor names fn-stack all-p)
  (when descriptor
    (dotimes (i (length names))
      (cond ((aref fn-stack i)
	     (handle-message (aref names i) descriptor (aref fn-stack i)))
	    (t (unhandle-message (aref names i) descriptor))))
    (when all-p
      (do-handlers ((h fn) descriptor)
		   (declare (ignore fn))
		   (unless (find h names)
		     (unhandle-message h descriptor))))))


;;; This should only get called if the flavor is instantiable, not wayward.
;;; This stuff should happen both at load-time and compile-time.

(defun flavor-set-handlers (flavor names fns all-p)
  (let* ((old-desc (flavor-descriptor flavor))
	 (env (flavor-instance-env flavor))
	 (waywardp (flavor-wayward-p flavor)))
    (cond ((null old-desc)
	   (let ((new (make-instance-descriptor (flavor-name flavor)
						env 'default-handler)))
	     (descriptor-set-handlers new names fns all-p)
	     (setf (flavor-descriptor flavor) new)))
	  ((or (and (flavor-instantiated-p flavor)
		    (not (equalp (instance-env-vector env)
				 (instance-env-vector
				  (instance-descriptor-env old-desc)))))
	       waywardp)
	   (let ((new-desc (make-instance-descriptor
			    (flavor-name flavor) env 'default-handler)))
	     (do-handlers ((h fn) old-desc) (handle-message h new-desc fn))
	     (descriptor-set-handlers new-desc names fns all-p)
	     (resize-instances old-desc new-desc
			       #'(lambda (self)
				   (send self 'INTERNAL-INIT)))
	     (setf (flavor-descriptor flavor) new-desc
		   (flavor-wayward-p flavor) nil))
	   (When waywardp
	     (format *error-output*
		     "Instances of flavor ~S reunited with the flavor."
		     (flavor-name flavor))))
	  (t (descriptor-set-handlers
	      (flavor-descriptor flavor) names fns all-p)))))

(defvar *undefined-components*
  #'(lambda (f c) (error "Flavor ~S has undefined components - ~S." f c)))

(defun flavor-all-components (flavor)
  (when (or (changed-components (flavor-changed flavor))
	    (null (flavor-all-components+ flavor)))
    (setf (flavor-all-components+ flavor)
	  (calculate-all-components flavor *undefined-components*)
	  (flavor-changed flavor) %all-components-changed%))
  (flavor-all-components+ flavor))


;;; First, get a stack of the methods we want to compile.
;;; Then do a pass to get the combination.
;;; Then do a pass to order the methods.
;;; (Note that we get the :base-flavor-last methods in the reverse order).
;;; Then go through the methods and generate the code.
;;;


(defun internal-cleanup-flavor
       (flavor &optional (really *cleanup-enable*)
	       &aux all-components (*flavors-compile* *flavors-compile*))
  (unless really (return-from internal-cleanup-flavor nil))
  (if *flavors-compile*
      (if (flavor-compiled-p flavor) (setq *flavors-compile* nil)
	  (setf (flavor-changed flavor)
		(logior %all-components-changed% (flavor-changed flavor))))
      (unless (flavor-compiled-p flavor)
	(return-from internal-cleanup-flavor nil)))
  (setq all-components (flavor-all-components flavor))
  (when (changed-required-flavors (flavor-changed flavor))
    (cond ((flavor-abstract-p flavor))
	  (t (dolist (c all-components)
	       (let ((rflavors nil))
		 (dolist (f (flavor-required-flavors c))
		   (unless (member f all-components) (push f rflavors)))
		 (when rflavors
		   (funcall *uninstantiable* flavor
			    "Additional required flavors: ~S." rflavors))))))
    (setf (changed-required-flavors (flavor-changed flavor)) nil))
  (when (let ((changed (flavor-changed flavor)))
	  (or (changed-iv-order changed)
	      (changed-required-ivs changed)
	      (changed-iv-inits changed)))
    (let ((new-env (setf (flavor-instance-env flavor)
			 (calculate-instance-env flavor))))
      (when (instance-env-required new-env)
	(unless (flavor-abstract-p flavor)
	  (funcall *uninstantiable*
		   flavor "Required instance variables ~S."
		   (instance-env-required new-env))))
      (setf (changed-required-ivs (flavor-changed flavor)) nil)))
  (setf (changed-iv-order (flavor-changed flavor)) nil)
  (setf (changed-iv-inits (Flavor-changed flavor)) nil)

  (catch 'dont-do-methods
    (if *dont-do-methods* (throw 'dont-do-methods nil))
    (with-stacks (methods combinations functions *description* *called-methods*)
      (cond ((changed-all-methods (Flavor-changed flavor))
	     (dolist (c all-components)
	       (let ((str (flavor-methods c)))
		 (do-methods (m str)
		   (unless (find m methods)
		     (when (find :combined (get-method-types m str) :key #'car
				 :test-not #'eq)
		       (vector-push-extend m methods)))))))
	    ((flavor-changed-methods flavor)
	     (let ((vec (flavor-changed-methods flavor)))
	       (dotimes (i (length vec))
		 (let ((elt (aref vec i)))
		   (if (listp elt)
		       (dolist (e elt)
			 (unless (find e methods)
			   (vector-push-extend e methods)))
		       (unless (find elt methods)
			 (vector-push-extend elt methods))))))))
      (set-stack-length combinations (length methods))
      (set-stack-length functions (length methods))
      (labels
	((defining-form (fn flavor description form)
           ;(declare (inline defining-form))
	   `(progn (setf (get ',fn 'description) ',(copy-seq description))
		   (internal-define-method
		    ,fn ,(flavor-instance-env flavor) (&rest %combined-args)
		    ,(list form))
		   (method-add
		    ',*message* :combined ',fn
		    (flavor-methods (get-flavor ',(flavor-name flavor)))))))
	(let ((*methods* nil))
	  (dolist (c all-components)
	    (dotimes (i (length methods))
	      (let* ((*message* (aref methods i))
		     (newc (cdr (assoc *message* (flavor-combinations c))))
		     oldc)
		(when newc
		  (cond ((setq oldc (aref combinations i))
			 (if (not (equal newc oldc))
			     (error "Method combination conflict for method ~S."
				    *message*)))
			(t (setf (aref combinations i) newc)))))))
	  (dolist (c all-components)
	    (let ((defined-methods (Flavor-methods c)))
	      (dotimes (i (length methods))
		(let* ((*message* (aref methods i))
		       (type-assoc (method-types *message* defined-methods)))
		  (unless (null type-assoc)
		    (let ((comb (or (aref combinations i) *default-combination*)))
		      (funcall (combination-ordering (car comb)) (cdr comb)
			       type-assoc)))))))
	  (dotimes (i (length methods))
	    (let* ((*message* (aref methods i))
		   (comb (or (aref combinations i) *default-combination*)))
	      (setf (fill-pointer *description*) 0)
	      (funcall (combination-ordering (car comb)) (cdr comb) nil t)
	      (let (inherit fn)
		(dolist (c all-components)
		  (setq fn (method-find *message* :combined (flavor-methods c)))
		  (when (setq inherit
			      (and fn (equalp *description* (get fn 'description))
				   (= (length (method-called-methods fn))
				      (length *called-methods*))
				   (every #'(lambda (m) (find m *called-methods*))
					  (method-called-methods fn))
				   (funcall *inheritablep* c *message*)))
		    (return nil)))
		(cond
		 ((or (eq inherit :redefine) (null inherit))
		  (unless fn (setq fn (flavor-function-name
				       (flavor-name flavor) *message* :combined)))
		  (let ((form
			 (funcall (combination-mixer (car comb)) (cdr comb))))
		    (cond ((eq (car form) 'method-apply)
			   (setf (ARef functions i) (Cadr form)))
			  (t (funcall *definer* fn
				      (defining-form fn flavor *description* form))
			     (setf (aref functions i) fn)))))
		 (t (setf (Aref functions i) fn))))))))
      (when (changed-required-methods (flavor-changed flavor))
	(cond ((flavor-abstract-p flavor))
	      (t (let ((rmethods nil))
		   (dolist (c all-components)
		     (dolist (m (flavor-required-methods c))
		       (unless (or (aref functions (position m methods))
				   (and (flavor-descriptor flavor)
					(get-handler m
						     (flavor-descriptor flavor))))
			 (push m rmethods))))
		   (when rmethods
		     (funcall *uninstantiable* flavor
			      "Additonal required methods: ~S." rmethods)))))
	(setf (changed-required-methods (flavor-changed flavor)) nil))
      (unless (flavor-abstract-p flavor)
	(funcall *set-handlers* flavor methods functions
		 (changed-all-methods (flavor-changed flavor))))))
  (dealloc-tiny-stack (flavor-changed-methods flavor))
  (setf (changed-all-methods (Flavor-changed flavor)) nil)
  (setf (flavor-compiled-p flavor) t)
  (when (changed-iv-keywords (flavor-changed flavor))
    (let* ((env (flavor-instance-env flavor))
	   (vec (iv-env-vector env))
	   (ables (method-env-ables env))
	   (keyword (find-package "KEYWORD"))
	   res)
      (dotimes (i (length vec))
	(when (ables-initable (Aref ables i))
	  (push (cons (intern (symbol-name (aref vec i)) keyword) i) res)))
      (setf (flavor-iv-keywords* flavor) res))
    (setf (changed-iv-keywords (flavor-changed flavor)) nil))
  (let* ((changed (flavor-changed flavor))
	 (req-inits (changed-required-inits changed))
	 (plist (changed-default-plist changed))
	 (keywords (changed-init-keywords changed)))
    (when (or req-inits plist keywords)
      (let ((new-req-inits nil) (newplist nil) (newkeywords nil))
	(dolist (c all-components)
	  (when req-inits
	    (dolist (req-init (flavor-required-inits c))
	      (pushnew req-init new-req-inits)))
	  (when plist
	    (do ((list (flavor-default-plist c) (cddr list)))
		((endp (cdr list)))
	      (when (eq 'foo (getf newplist (car list) 'foo))
		(push (cadr list) newplist)
		(push (car list) newplist))))
	  (when keywords
	    (dolist (key (flavor-init-keywords c))
	      (pushnew key newkeywords))))
	(when req-inits
	  (setf (flavor-required-inits* flavor) new-req-inits
		(changed-required-inits (flavor-changed flavor)) nil))
	(when plist (setf (flavor-default-plist* flavor) newplist
			  (changed-default-plist (flavor-changed flavor)) nil))
	(when keywords
	  (setf (flavor-init-keywords* flavor) newkeywords
		(changed-init-keywords (flavor-changed flavor)) nil))))))

;;;
;;; The interface routines.
;;;


;;; We try to clean up the components first so we can inherit their
;;; combined methods.  This works in all cases but where a loop
;;; is present in the structure, so we have to test whether we can inherit
;;; or not.
;;;
;;; We traverse the tree of components, keeping track of the current
;;; path in touched (thus preventing loops), and ignoring when we
;;; hit the current path again.

(defun order-flavors (stack result-stack)
  (with-stacks (touched)
    (macrolet ((touch (f)
		 `(cond ((find ,f touched))
			((find ,f result-stack))
			(t (touch-components ,f)))))
      (labels ((touch-components (f)
		 (catch 'out
		   (vector-push-extend f touched)
		   (let ((*undefined-components*
			  #'(lambda (f c)
			      (format *error-output*
				      "Flavor ~S has undefined components ~S."
				      (flavor-name f) c)
			      (vector-pop touched)
			      (throw 'out nil))))
		     (dolist (c (flavor-all-components f)) (touch c)))
		   (when (find f stack) (vector-push-extend f result-stack))
		   (vector-pop touched))))
	(dotimes (i (length stack))
	  (let ((thing (ARef stack i)))
	    (touch thing)))))))

(defun cleanup-all-flavors ()
  "Optimizes the cleaning of dirty flavors, allowing effective sharing
  of combined methods."
  (with-stacks (ordered)
    (order-flavors *dirty-flavors* ordered)
    (dotimes (i (length ordered))
      (cleanup-flavor (Aref ordered i))))
  (setf (fill-pointer *dirty-flavors*) 0))

(defun cleanup-flavor (flavor)
  "Cleans the flavor."
  (when (symbolp flavor) (Setq flavor (get-flavor flavor)))
  (let ((*uninstantiable* #'dissociate-instances)
	(*inheritablep* #'message-clean-p)
	(*definer* #'(lambda (name form)
		       (declare (ignore name))
		       (if *flavor-compile-methods*
			   (funcall (compile nil `(lambda () ,form)))
			   (eval form))))
	(*set-handlers* #'flavor-set-handlers))
    (internal-cleanup-flavor flavor t)))

;;; Propogates methods-need-work and then cleans up.
;;; Does nil message and do-dependents t mean all of everybody's methods,
;;; or just those inherited from the original?

(Defun recompile-flavor (flavor-name &optional message (do-dependents t))
  "Use this when something compiled into a combined method has changed,
  for instance a macro used by a wrapper and hence compiled into the
  combined method."
  (let ((flavor (get-flavor flavor-name))
	(*uninstantiable* #'dissociate-instances)
	(*inheritablep* #'message-clean-p)
	(*definer* #'(lambda (name form)
		       (declare (ignore name))
		       (if *flavor-compile-methods*
			   (funcall (compile nil `(lambda () ,form)))
			   (eval form))))
	(*set-handlers* #'flavor-set-handlers))
    (cond (do-dependents
	   (with-stacks (flavors ordered)
	     (do-inheriting-flavors (flavor flavor flavors)
	       (rework-flavor flavor)
	       (if (null message)
		   (setf (changed-all-methods (flavor-changed flavor)) t)
		   (rework-methods flavor message)))
	     (order-flavors flavors ordered)
	     (dovec (o ordered) (internal-cleanup-flavor o))))
	  ((null message)
	   (rework-flavor flavor)
	   (setf (changed-all-methods (flavor-changed flavor)) t)
	   (internal-cleanup-flavor flavor))
	  (t (rework-flavor flavor)
	     (rework-methods flavor message)
	     (internal-cleanup-flavor flavor)))))

(defun compile-flavor (flavor &optional (*flavor-compile-methods*
					 *flavor-compile-methods*))
  "Makes a non-abstract flavor ready for instantiation, and an abstract
  flavor sharable with respect to combined methods.  If you're doing this
  for a file, use compiler-compile-flavors instead."
  (let ((*flavors-compile* t))
    (cleanup-flavor flavor)))

(defmacro compiler-compile-flavors (&rest flavor-names)
  "Just like compile-flavors, only it includes the definitions of all
  combined methods - compile-flavors doesn't redefine methods it can
  find in the runtime environment.  This way, you're guaranteed to have
  all the functions you expect when you load the file.
  Uses the cleanup machinery, so it cleans up affected flavors first."

  (with-stacks (stack ordered defined-methods)
    (dolist (f flavor-names) (vector-push-extend (get-flavor f) stack))
    (order-flavors stack ordered)
    (dotimes (i (length ordered))
      (cleanup-flavor (aref ordered i)))
    (Setf (fill-pointer ordered) 0)
    (dolist (name flavor-names)
      (let ((flavor (get-flavor name)))
	(setf (changed-all-methods (flavor-changed flavor)) t)))
    (let* ((forms nil)
	   (*uninstantiable* #'dissociate-instances)
	   (*inheritablep*
	    #'(lambda (flavor message)
		(cond ((member (flavor-name flavor) flavor-names) t)
		      ((message-clean-p flavor message) :redefine)
		      (t nil))))
	   (*definer* #'(lambda (name form)
			  (unless (find name defined-methods)
			    (push form forms)
			    (vector-push-extend name defined-methods))))
	   (*set-handlers*
	    #'(lambda (flavor name-stack fn-stack all-p)
		(declare (ignore all-p))
		(push `(flavor-set-handlers (get-flavor ',(flavor-name flavor))
			 ',(copy-seq name-stack) ',(copy-seq fn-stack) t)
		      forms)))
	   (*flavors-compile* t))
      (order-flavors stack ordered)
      (dotimes (i (length ordered))
	(internal-cleanup-flavor (aref ordered i) t))
      `(progn (eval-when (:load-toplevel)
		(let ((*dont-do-methods* t))
		  (mapc #'compile-flavor ',flavor-names)))
	      (eval-when (compile load)
		,@(nreverse forms))))))


;;;
;;; Combination stuff.
;;;

;;; These macros refer to the specials of the previous section.

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defmacro get-slot (Message slot)
    `(my-assoc ,message (my-assoc ,slot *methods*))))

(defun method-list (slot-name)
  "Takes the slot-name given in order-methods.  Returns the list of
  method-function-names resulting from the ordering.  For a :primary
  ordered slot, this will be a list of one function."
  (get-slot *message* slot-name))

(defun call-methods (slot-name-or-list)
  "Takes either a slot-name or a list of method function names.  Returns a list
  of forms that call them."
  (mapcar #'(lambda (x) `(method-apply ,x %combined-args))
	  (if (listp slot-name-or-list) slot-name-or-list
	      (get-slot *message* slot-name-or-list))))

(defun wrapper-mix (slot-name-or-list form)
  "Wraps the form with the wrappers (and whoppers) of the given types.
  (See defwrapper, defwhopper.)  The ordering of the wrapper functions
  is the reverse of the order in which the wrappings will be encountered
  at runtime."
  (let ((list (if (atom slot-name-or-list) (get-slot *message* slot-name-or-list)
		  slot-name-or-list)))
    (dolist (l list)
      (setq form (funcall l form)))
    form))


(defmacro defcombination-ordering (name (arg) &body body)
  "Should use ORDER-METHODS or ORDER-WRAPPERS (almost exclusively) to
  order the method types of a method into named slots.
  Keep in mind that the code may be executed many times, depending upon the
  implementation."
  `(defun ,name (,arg type-assoc &optional reverse-and-describe)
     (dolist (cons (if reverse-and-describe '((foo . bar)) type-assoc))
       (let ((type (car cons))
	     (method (cdr cons)))
	 ,@body))))

(defmacro order-methods (order slot types)
  "Slot and types are evaled.  Slot-name is the name used in message-list, etc.
  Order is one of :base-flavor-last, :primary, or :base-flavor-first,
  as described in the documentation.  The method-types in the list are added
  in order to the list of methods in the slot."
  (case order
    (:primary `(if reverse-and-describe
		   (let ((thing (car (get-slot *message* ,slot))))
		     (unless (find thing *called-methods*)
		       (vector-push-extend thing *called-methods*)))
		   (when (member type ,types)
		     (unless (get-slot *message* ,slot)
		       (push method (get-slot *message* ,slot))))))
    (:base-flavor-first `(if reverse-and-describe
			     (dolist (m (get-slot *message* ,slot))
			       (unless (find m *called-methods*)
				 (vector-push-extend m *called-methods*)))
			     (when (member type ,types)
			       (push method (get-slot *message* ,slot)))))
    (:base-flavor-last `(if reverse-and-describe
			    (dolist (m (nreversef (get-slot *message* ,slot)))
			      (unless (find m *called-methods*)
				(vector-push-extend m *called-methods*)))
			    (when (member type ,types)
			      (push method (get-slot *message* ,slot)))))))

(defmacro order-wrappers (order slot types)
  "Just like order-methods, except remembers the list of wrapper functions
  along with the list of methods."
  `(cond (reverse-and-describe
	  ,(if (eq order :base-flavor-last)
	       `(nreversef (get-slot *message* ,slot)))
	  (vector-push-extend (get-slot *message* ,slot) *description*))
	 (t (order-methods ,order ,slot ,types))))


(defmacro defcombination (name ordering-fn (order-arg) &body body)
  "The order-arg is bound to the argument of the combination name.
  the ordering-fn is used to order the methods of the method we're combining
  (see defcombination-ordering).  The body of this form should produce
  the actual combined method, using the functions METHOD-LIST, CALL-METHODS,
  CALL-METHOD, AND WRAPPER-MIX to access the slots of the ordered methods.
  The system will optimize only bare method-calls, not those surrounded
  by AND, etc."
  `(make-combination ',name ',ordering-fn
		     #'(lambda (,order-arg) ,order-arg ,@body)))

;;;
;;; Random user-level functions
;;;

(defun symeval-in-instance (instance symbol &optional no-error-p unbound)
  "If no-error-p is non-nil, unbound ivs will simply return the value of
  the second optional. Ivs not present in the environment will signal an error
  in either case."
  (let* ((vec (iv-env-vector (instance-descriptor-env
			      (instance-descriptor instance))))
	 (pos (position symbol vec)))
    (cond ((null pos)
	   (error "Instance variable ~S not found in instance ~S."
		  symbol instance))
	  ((slot-unbound-p instance pos)
	   (if no-error-p unbound
	       (error "Instance variable ~S unbound." symbol)))
	  (t (instance-ref instance pos)))))

(defun set-in-instance (instance symbol value)
  "An error is signalled if the symbol is not an instance variable in the given
  instance."
  (let* ((vec (iv-env-vector (instance-descriptor-env
			      (instance-descriptor instance))))
	 (pos (position symbol vec)))
    (cond ((null pos)
	   (error "Instance variable ~S not found in instance ~S."
		  symbol instance))
	  (t (setf (instance-ref instance pos) value)))))

(defun get-handler-for (object message)
  "Returns the function that handles the given message."
  (get-handler message object))

(defmacro lexpr-funcall-self (message &rest arguments)
  "Supplied for compatibility with Symbolics Flavors."
  `(apply #'send self ,message ,@arguments))

(defmacro funcall-self (message &rest arguments)
  "Supplied for compatibility with Symbolics Flavors."
  `(send self ,message ,@arguments))

(Defun flavor-allowed-init-keywords (flavor-name)
  "Returns a list of the allowable init-keywords for the flavor."
  (let ((flavor (get-flavor flavor-name))
	(res nil))
    (dolist (flav (flavor-all-components flavor))
      (setf res (append (flavor-init-keywords flav) res)))
    (sort res #'string-lessp :key #'symbol-name)))

(defun flavor-allows-init-keyword-p (flavor-name keyword)
  "Returns the flavor that provides this init option, or NIL if none."
  (let ((flavor (get-flavor flavor-name)))
    (dolist (flav (flavor-all-components flavor))
      (if (find keyword (flavor-init-keywords (flavor-method-env flav)))
	  (return-from flavor-allows-init-keyword-p flavor-name)))))

(defun make-instance (flavor-name &rest init-plist)
  "Compiles the flavor and makes sure it's initable, inits any initable
  instance variables from the init-plist and inits others if inits given,
  then sends :INIT with the plist if it's handled.  Returns the new instance."
  #|(typecase init-plist
    (list)
    (symbol (setq init-plist (symbol-plist init-plist)))
    (t (error "Init-plist not list or symbol - ~S." init-plist)))|#
  (let* ((flavor (get-flavor flavor-name)))
    (when (flavor-abstract-p flavor)
      (error "Attempt to instantiate an abstract flavor: ~S." flavor-name))
    (let ((*flavors-compile* t))
      (cleanup-flavor (get-flavor flavor-name)))
    (with-stacks (unused-properties used-properties)
      (let* ((desc (flavor-descriptor flavor))
	     (self (instantiate-instance-descriptor desc))
	     (new-plist nil))
	;; Scan the init-plist for variable inits.  If it's not a variable
	;; init, make sure it's in the allowed-keywords sequence.
	(do ((plist init-plist (cddr plist)))
	    ((endp plist))
	  (if (plusp (length unused-properties))
	      (if (not (find (car plist) (flavor-init-keywords* flavor)))
		  (vector-push-extend (car plist) unused-properties))
	      (let ((iv (cdr (assoc (car plist) (flavor-iv-keywords* flavor)))))
		(cond (iv (when (slot-unbound-p self iv)
			    (setf (instance-ref self iv) (cadr plist))
			    (vector-push-extend (car plist) used-properties)))
		      ((find (car plist) (flavor-init-keywords* flavor))
		       (when (eq 'frob (getf new-plist (car plist) 'frob))
			 (vector-push-extend (car plist) used-properties)
			 (push (eval (cadr plist)) new-plist)
			 (push (car plist) new-plist)))
		      (t (vector-push-extend (car plist) unused-properties))))))
	(do ((plist (flavor-default-plist* flavor) (cddr plist)))
	    ((endp plist))
	  (if (plusp (length unused-properties))
	      (if (not (find (car plist) (flavor-init-keywords* flavor)))
		  (vector-push-extend (car plist) unused-properties))
	      (let ((iv (cdr (assoc (car plist) (flavor-iv-keywords* flavor)))))
		(cond (iv (when (slot-unbound-p self iv)
			    (setf (instance-ref self iv) (EVAL (cadr plist)))
			    (vector-push-extend (Car plist) used-properties)))
		      ((find (car plist) (Flavor-init-keywords* flavor))
		       (when (eq 'frob (getf new-plist (Car plist) 'frob))
			 (push (cadr plist) new-plist)
			 (push (car plist) new-plist)
			 (vector-push-extend (car plist) used-properties)))
		      (t (vector-push-extend (car plist) unused-properties))))))
	(if (plusp (length unused-properties))
	    (error "Unknown init keywords for make-instance: ~S."
		   (coerce unused-properties 'list)))
	(send self 'INTERNAL-INIT)
	(let (required)
	  (dolist (req (flavor-required-inits* flavor))
	    (unless (find req used-properties)
	      (push req required)))
	  (if required (error "Additional required keywords for flavor ~S: ~S."
			      flavor-name required)))
	(send self :send-if-handles :init new-plist)
	self))))


(defun undefflavor (name)
  "Permanently dissociates the flavor's instances and undefines the flavor."
  (let ((flavor (get-flavor name)))
    (when (flavor-defined-p flavor)
      (with-stacks (affected)
	(do-inheriting-flavors (i flavor affected)
	  (rework-flavor i)
	  (setf (changed-components (flavor-changed i)) t))
	(dissociate-instances flavor "Flavor has been undefflavored.")
	(setf (flavor-methods flavor) (make-method-structure)
	      (flavor-descriptor flavor) nil
	      (flavor-compiled-p flavor) nil
	      (flavor-defined-p flavor) nil)
	(setq *all-flavor-names*
              (delete name *all-flavor-names*))
	(dolist (c (flavor-components flavor))
	  (setf (flavor-dependents (flavor-dependents (get-flavor c)))
                (delete name (flavor-dependents (get-flavor c)))))
	(with-stacks (ordered)
	  (order-flavors affected ordered)
	  (dotimes (i (length ordered))
	    (cleanup-flavor (ARef ordered i))))))
    name))

(Defmacro undefmethod ((flavor type &optional (message nil mp)))
  "Undoes the effect of the corresponding defmethod, defwrapper (use type :wrapper)
  , or whopper (type :whopper)."
  (if (not mp) (setq message type type :primary))
  `(%undefmethod ',flavor ',message ',type))
(defun %undefmethod (flavor-name message type)
  (let ((flavor (get-flavor flavor-name))
        dummy)
    (setq dummy
          (delete type (method-types message (Flavor-methods flavor))
                  :key #'car))
    (recompile-flavor (flavor-name flavor) message))
  message)

;;; Defines a function that takes the argument name and a a rest arg of the
;;; body, and that does the transformation for that wrapper, returning the
;;; new form.

(defmacro defwrapper ((flavor type &optional (method nil mp))
		      (args . passed-body) &body body)
  "Wrappers are sort of like macros, getting expanded into the combined method
  for flavors that inherit it.  The args is either ignore or a list that
  is destructure-bound at send-time to the method's argument-list.  The
  passed-body gets bound at 'expansion time' to the 'inside' of the wrapper."
  ;;
  (if (not mp) (setq method type type :wrapper))
  (if (eq args 'ignore) (Setq args nil))
  (let* ((name (flavor-function-name flavor method type))
	 (new-defun `(defun ,name (&rest ,passed-body)
		       (let ((form (progn ,@body)))
			 `(mydlet ((,',args %combined-args))
			    ,form)))))
    `(progn
      (eval-when (:compile-toplevel :execute :load-toplevel)
	,new-defun)
      (eval-when (:compile-toplevel :execute)
	(let ((new-defun ',new-defun)
	      (old-defun (get ',name 'old-defun))
	      (new-hash nil))
	  (when (or (and (null old-defun)
			 (let ((hash (get ',name 'sxhash)))
			   (or (null hash)
			       (not (= hash (setq new-hash (sxhash new-defun)))))))
		    (not (equal new-defun old-defun)))
	    (method-add ',method ',type ',name
			(flavor-methods (get-flavor ',flavor)))
	    (recompile-flavor ',flavor ',method)
	    (setf (get ',name 'old-defun) new-defun)
	    (if new-hash
		(setf (get ',name 'sxhash) new-hash)))))
      (eval-when (:load-toplevel)
	(let ((hash (Get ',name 'sxhash))
	      (new (sxhash ,new-defun)))
	  (when (or (null hash) (not (= hash new)))
	    (method-add ',method ',type ',name
			(flavor-methods (get-flavor ',flavor)))
	    (recompile-flavor ',flavor ',method)
	    (setf (Get ',name 'sxhash) new))))
      ',method)))


(defmacro defmethod ((flavor-name type &optional (method nil methodp))
		     args &body forms)
  "Refer to the flavor documentation for details."
  (if (not methodp) (psetq method type type :primary))
  (let ((flavor (get-flavor flavor-name))
	(function-name (flavor-function-name flavor-name method type)))
    (unless (flavor-defined-p flavor)
      (error "Flavor ~S not defined." flavor-name))
    (multiple-value-bind (docs forms) (extract-doc-and-declares forms)
      `(eval-when (:execute :load-toplevel :compile-toplevel)
	 (internal-define-method ,function-name ,(flavor-method-env flavor)
				 ,args (,@docs (block ,method ,@forms)))
	 (%defmethod ',flavor-name ',method ',type ',function-name)))))

(defun %defmethod (flavor-name method type function-name)
  (if (method-add method type function-name
		  (flavor-methods (get-flavor flavor-name)))
      (recompile-flavor flavor-name method))
  method)


;;; The continuation is a lambda closed in the instance environment.
;;; The type used in the defwhopper is the type of the wrapper (default :whopper).
;;; The whopper itself is a method of the flavor - it (and hence the name)
;;; depends upon the flavor it's defined for, the method and type.
;;; (so that all inheriting flavors have it).

(defmacro defwhopper ((name &rest method-and-type) args &body body)
  "Whoppers are to wrappers what functions are to macros.  Whoppers
  are functions that call continue-whopper, lexpr-continue-whopper,
  or continue-whopper-all with the desired arguments to continue with the
  wrapped code."
  (setq method-and-type (nreverse method-and-type))
  (let* ((method (pop method-and-type))
	 (type (or (pop method-and-type) :whopper))
	 (whopper-method (flavor-function-name name method type "WHOPPER")))
    `(progn (defmethod (,name ,whopper-method)
		       (%continuation %combined-args ,@args)
	      ,@body)
	    (defwrapper (,name ,type ,method) (nil . wrapper-body)
	      `(apply #'send self ',',whopper-method
		      #'(lambda (%arglist &rest %combined-args)
			  (if (listp %arglist) (setq %combined-args %arglist))
			  ,@wrapper-body)
		      %combined-args
		      %combined-args)))))

(defmacro continue-whopper-all ()
  "See defwhopper.  Continues the combined method with all the passed arguments
  (efficiently)."
  `(funcall %continuation %combined-args))
(defmacro continue-whopper (&rest args)
  "See defwhopper and continue-whopper-all.  Continue-whopper lets you continue
  the combined method with different arguments."
  `(funcall %continuation t ,@args))
(defmacro lexpr-continue-whopper (&rest args)
  "See defwhopper."
  `(apply %continuation t ,@args))

) ;Eval-when-3
