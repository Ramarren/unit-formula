(in-package :unit-formulas)

(define-condition incomplete-unit-conversion (error)
  ((input-unit :reader incomplete-unit-conversion-input-unit
	       :initarg :input-unit
	       :initform nil)
   (output-unit :reader incomplete-unit-conversion-output-unit
		:initarg :output-unit
		:initform nil)
   (missing-units-of :reader incomplete-unit-conversion-missing-units-of
		     :initarg :missing-units-of))
  (:report (lambda (iuc s)
	     (format s
		     "Incomplete unit transformation.  Cannot cancle out the following units: '~A'~@[ with input-unit of '~A'~]~@[ to output-unit of '~A'~]."
		     (with-output-to-string (uv)
		       (print-unit-vector
			(incomplete-unit-conversion-missing-units-of iuc)
			uv))
		     (incomplete-unit-conversion-input-unit iuc)
		     (incomplete-unit-conversion-output-unit iuc)))))

(defgeneric transform-units (input-unit output-unit unit-bag)
  (:documentation "TRANSFORM-UNITS, a way of doing dimensional analysis on a set of units.  The idea here is that a grab bag of units (UNIT-BAG) can be supplie along with the input unit and desired output units.  Dimensional analysis will figure out which units are needed to complete the transformation from one set of units to another until the requested units are reached.  TRANSFORM-UNITS will create a condition if UNIT-BAG doesn't provide the required units to reach the requested unit.")
  (:method ((input-unit unit) (output-unit unit) (unit-bag list))
    (multiple-value-bind (dimensioned dimensionless)
	(bag-units unit-bag)
      (convert-unit
       (reduce-unit
	(cons '*
	      (cons input-unit
		    (append dimensionless
			    (find-best-units
			     (add-unit-vectors (units-of output-unit)
					       (invert-unit-vector
						(units-of input-unit)))
			     dimensioned)))))
       output-unit)))
  (:method ((input-unit list) output-unit unit-bag)
    (transform-units (reduce-unit input-unit) output-unit unit-bag))
  (:method (input-unit (output-unit list) unit-bag)
    (transform-units input-unit (reduce-unit output-unit) unit-bag))
  (:method ((input-unit symbol) output-unit unit-bag)
    (transform-units (reduce-unit input-unit) output-unit unit-bag))
  (:method (input-unit (output-unit symbol) unit-bag)
    (transform-units input-unit (reduce-unit output-unit) unit-bag)))

(defun find-best-units (desired-vector unit-bag &optional (depth (length unit-bag)))
  (if (< depth 0)
      (error 'incomplete-unit-conversion
	     :missing-units-of desired-vector)
      (unless (every #'zerop desired-vector)
	(let ((unit (find-best-unit-match desired-vector unit-bag)))
	  (if unit
	      (cons unit
		    (find-best-units
		     (subtract-unit-vectors desired-vector (units-of unit))
		     (remove-if #'(lambda (u) (same-unit-p u unit)) unit-bag)
		     (1- depth)))
	      (error 'incomplete-unit-conversion
		     :missing-units-of desired-vector))))))

(defun collect-dimensionless-p (unit-bag)
  (loop for unit in unit-bag
       when (dimensionless-p unit)
       collect unit))

(defun add-unit-vectors (vector1 vector2)
  "ADD-UNIT-VECTORS, adds two vectors."
  (assert (and (vectorp vector1) (vectorp vector2)))
  (map 'vector #'+ vector1 vector2))

(defun subtract-unit-vectors (vector1 vector2)
  "SUBTRACT-UNIT-VECTORS, subtracts two vectors"
  (assert (and (vectorp vector1) (vectorp vector2)))
  (map 'vector #'- vector1 vector2))

(defun invert-unit-vector (unit)
  "Inverts the direction of all dimensions in a unit-vector."
  (assert (and (vectorp unit)))
  (map 'vector #'- unit))

(defgeneric invert-unit (unit)
  (:documentation "Accepts a UNIT and returns the inversion of it.")
  (:method ((unit unit))
    (make-instance 'unit
		   :factor (/ 1 (factor-of unit))
		   :units (invert-unit-vector (units-of unit))
		   :convert-to (convert-to unit)
		   :convert-from (convert-from unit)))
  (:method (unit)
    (invert-unit (reduce-unit unit))))

(defun bag-units (unit-bag)
  "Preps the list UNIT-BAG for use by reducing all units and inverted units.
Returns multiple values consisting of dimensioned and dimensionless units."
  (assert (listp unit-bag))
  (let ((dimensioned ())
	(dimensionless ()))
    (flet ((append-unit (u)
	     (if (dimensionless-p u)
		 (push u dimensionless)
		 (setf dimensioned
		       (append (list u (invert-unit u)) dimensioned)))))
      (loop for unit in unit-bag
	 when (or (listp unit) (symbolp unit))
	 do (append-unit (reduce-unit unit))
	 when (eql 'unit (type-of unit))
	 do (append-unit unit)))
    (values dimensioned dimensionless)))

(defun score-unit (unit-vector unit)
  (let ((cancled-units (add-unit-vectors unit-vector
					 (invert-unit-vector (units-of unit)))))
    (reduce #'(lambda (a b) (+ (abs a) (abs b))) 
	    cancled-units)))

(defun find-best-unit-match (unit-vector unit-bag)
  (loop with max = 100
     with lowest-match = nil
     for unit in unit-bag
     for score = (score-unit unit-vector unit)
     when (< score max)
     do (setf max score
	      lowest-match unit)
     finally (return lowest-match)))
