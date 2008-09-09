(in-package :unit-formulas)

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
