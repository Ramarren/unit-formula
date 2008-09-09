(in-package :unit-formulas)

(defparameter *base-units*
  (list :meter 0 :second 1 :kelvin 2 :kilogram 3 :ampere 4 :mole 5 :candela 6))

(defclass unit ()
  ((factor :accessor factor-of :initform 1 :initarg :factor)
   (units :accessor units-of :initarg :units
	  :initform (make-array (length *base-units*) :initial-element 0))))

(defmethod print-object ((unit unit) stream)
  (print-unreadable-object (unit stream :type t :identity nil)
    (format stream "~a:" (factor-of unit))
    (iter (for u in-vector (units-of unit))
	  (format stream " ~a" u))))

(defparameter *units* (make-hash-table))

(iter (for (unit-keyword unit-index) on *base-units* by #'cddr)
      (for unit-instance = (make-instance 'unit))
      (setf (aref (units-of unit-instance) unit-index) 1)
      (setf (gethash (intern (symbol-name unit-keyword) :unit-formulas) *units*)
	    unit-instance))

(defmacro define-units (&rest unit-definitions)
  `(progn ,@(iter (for (unit-names unit-definition) on unit-definitions by #'cddr)
		  (collect
		      `(let ((unit-instance (reduce-unit ',unit-definition)))
			 (iter (for name in ',(ensure-list unit-names))
			       (setf (gethash name *units*) unit-instance)))))))

(defmacro define-factors (unit &rest factor-definitions)
  `(define-units ,@(iter (for (factor-names factor) on factor-definitions by #'cddr)
			 (appending `(,factor-names (,factor ,unit))))))
