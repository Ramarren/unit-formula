(in-package :unit-formulas)

(define-condition incomplete-unit-transformation (error)
  ((input-unit :reader incomplete-unit-transformation-input-unit
	       :initarg :input-unit
	       :initform nil)
   (output-unit :reader incomplete-unit-transformation-output-unit
		:initarg :output-unit
		:initform nil)
   (missing-units-of :reader incomplete-unit-transformation-missing-units-of
		     :initarg :missing-units-of))
  (:report (lambda (iut s)
	     (format s
		     "Incomplete unit transformation.  Cannot cancel out the following units: '~A'~@[ with input-unit of '~A'~]~@[ to output-unit of '~A'~]."
		     (with-output-to-string (uv)
		       (print-unit-vector
			(incomplete-unit-transformation-missing-units-of iut)
			uv))
		     (incomplete-unit-transformation-input-unit iut)
		     (incomplete-unit-transformation-output-unit iut)))))

(defgeneric transform-units (input-unit output-unit unit-bag)
  (:documentation
"TRANSFORM-UNITS, a method of performing complex conversions on units.
The concept is that a grab bag of units (UNIT-BAG) can be supplied as a 
list along with an input unit and desired output unit.  TRANSFORM-UNITS 
will calculate the needed transformation from the INPUT-UNITS to 
OUTPUT-UNITS, using the provided units in UNIT-BAG.  TRANSFORM-UNITS creates
a condition of type INCOMPLETE-UNIT-TRANSFORMATION if UNIT-BAG lacks the 
units necessary to reach the requested OUTPUT-UNIT.")
  (:method ((input-unit unit) (output-unit unit) (unit-bag list))
    (multiple-value-bind (dimensioned dimensionless)
	(bag-units unit-bag)
      (convert-unit
       (reduce-unit
	`(* ,input-unit
	    ,@dimensionless
	    ,@(find-best-units (subtract-unit-vectors (units-of output-unit)
						      (units-of input-unit))
			       dimensioned)))
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
      (error 'incomplete-unit-transformation
	     :missing-units-of desired-vector)
      (unless (every #'zerop desired-vector)
	(let ((unit (find-best-unit-match desired-vector unit-bag)))
	  (if unit
	      (cons unit
		    (find-best-units
		     (subtract-unit-vectors desired-vector (units-of unit))
		     (remove-if #'(lambda (u) (same-unit-p u unit)) unit-bag)
		     (1- depth)))
	      (error 'incomplete-unit-transformation
		     :missing-units-of desired-vector))))))

(defun subtract-unit-vectors (vector1 vector2)
  "SUBTRACT-UNIT-VECTORS, subtracts each index of two vectors"
  (assert (and (vectorp vector1) (vectorp vector2)))
  (map 'vector #'- vector1 vector2))

(defun invert-unit-vector (unit)
  "Inverts the direction of all dimensions in a unit-vector."
  (assert (vectorp unit))
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
  "Preps the list UNIT-BAG for use by reducing all units and their inversions.
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
  "Returns an integer representing the complexity of the unit compared with
unit-vector.  The unit with the lowest score cancels more of the unit-vector."
  (let ((canceled-units (subtract-unit-vectors unit-vector (units-of unit))))
    (reduce #'(lambda (a b) (+ (abs a) (abs b))) 
	    canceled-units)))

(defun find-best-unit-match (unit-vector unit-bag)
  "Loops over the unit-bag checking the score of the unit and returns the
unit with the lowest score."
  (loop with max = most-positive-fixnum
     with lowest-match = nil
     for unit in unit-bag
     for score = (score-unit unit-vector unit)
     when (< score max)
     do (setf max score
	      lowest-match unit)
     finally (return lowest-match)))
