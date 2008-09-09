(in-package :unit-formulas)

(defclass unit ()
  ((factor :accessor factor-of :initform 1 :initarg :factor)
   (units :accessor units-of :initarg :units
	  :initform (make-array (length *base-units*) :initial-element 0))))

(defmethod print-object ((unit unit) stream)
  (print-unreadable-object (unit stream :type t :identity nil)
    (format stream "~a " (factor-of unit))
    (print-unit-vector (units-of unit) stream)))