;;     unit-formulas - library for unit checked formula definitions
;;     Copyright (C) 2008 Jakub Higersberger

;;     This program is free software; you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation; either version 2 of the License, or
;;     (at your option) any later version.

;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with this program; if not, write to the Free Software
;;     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


(in-package :unit-formulas)

;;; units are described by s-expressions, with operations: + - * / expt sqrt
;;; * is implied if car of a list is other symbol

(defclass additive-unit (unit)
  ()
  (:documentation
   "A class to represent additive unit definitions"))

(defun additive-unit-p (unit)
  (typep unit 'additive-unit))

(defun add-units (units)
  (let ((same-units-p (reduce #'same-unit-p
			      (remove-if #'dimensionless-p units))))
    (if (and (cdr units) same-units-p)
	(make-instance 'additive-unit
		       :factor (reduce #'+ (mapcar #'factor-of units))
		       :units (units-of same-units-p))
	(car units))))

(defun subtract-units (units)
  (let ((same-units-p (reduce #'same-unit-p
			      (remove-if #'dimensionless-p units))))
    (if (and (cdr units) same-units-p)
	(make-instance 'additive-unit
		       :factor (reduce #'- (mapcar #'factor-of units))
		       :units (units-of same-units-p))
	(car units))))

(defun multiply-units (units)
  (if (cdr units)
      (make-instance 'unit
		     :factor (reduce #'* (mapcar #'factor-of units))
		     :units (apply #'map 'vector #'+ (mapcar #'units-of units)))
      (car units)))

(defun divide-units (units)
  (if (cdr units)
      (make-instance 'unit
		     :factor (reduce #'/ (mapcar #'factor-of units))
		     :units (apply #'map 'vector #'- (mapcar #'units-of units)))
      (car units)))

(defun expt-units (unit power)
  (check-type power (or integer ratio))
  (make-instance 'unit
		 :factor (expt (factor-of unit) power)
		 :units (map 'vector (curry #'* power) (units-of unit))))

(defun sqrt-units (unit)
  (make-instance 'unit
		 :factor (sqrt (factor-of unit))
		 :units (map 'vector (rcurry #'/ 2) (units-of unit))))

(defgeneric same-unit-p (unit1 unit2 &key factor)
  (:method ((unit1 unit) (unit2 unit) &key (factor nil))
    (when (and (every #'= (units-of unit1) (units-of unit2))
	       (if factor
		   (= (factor-of unit1) (factor-of unit2))
		   t))
      unit1))
  (:method ((unit1 t) (unit2 t) &key (factor nil))
    (same-unit-p (reduce-unit unit1) unit2 :factor factor))
  (:method ((unit1 unit) (unit2 t) &key (factor nil))
    (same-unit-p unit1 (reduce-unit unit2) :factor factor)))

(defgeneric dimensionless-p (unit)
  (:method ((unit unit))
    (every #'zerop (units-of unit)))
  (:method ((unit t))
    (dimensionless-p (reduce-unit unit))))

(defun reduce-unit (unit-description)
  (etypecase unit-description
    (null (make-instance 'unit))
    (unit unit-description)
    (number
     (make-instance 'unit :factor unit-description))
    (symbol
     (let ((symbol
	    (find-symbol (symbol-name unit-description)
			 (load-time-value (find-package :unit-formulas) t))))
       (cond ((gethash symbol *units*))
	     (t (error "Unknown unit ~a" unit-description)))))
    (list
     (case (car unit-description)
       (+ (add-units (mapcar #'reduce-unit (cdr unit-description))))
       (- (subtract-units (mapcar #'reduce-unit (cdr unit-description))))
       (* (multiply-units (mapcar #'reduce-unit (cdr unit-description))))
       (/ (divide-units (mapcar #'reduce-unit (cdr unit-description))))
       (expt (expt-units (reduce-unit (cadr unit-description))
			 (caddr unit-description)))
       (sqrt (sqrt-units (reduce-unit (cadr unit-description))))
       (formula-unit (apply #'make-instance unit-description))
       (t
	(let* ((units (mapcar #'reduce-unit unit-description))
	       (formula (find-if #'formula-unit-p units)))
	  (cond (formula
		 ;; formula must have dimensionless units in it's definition.
		 (apply (formula-to formula) (remove formula units)))
		((some #'additive-unit-p units)
		 (add-units units))
		(t (multiply-units units)))))))))

(defmacro make-unit (value unit-description)
  "Makes an unit object with value and unit description. The second one is not evaluated."
  (let ((unit-prototype (reduce-unit unit-description)))
    (if (numberp value)
	(make-instance 'unit
		       :factor (* (factor-of unit-prototype) value)
		       :units (units-of unit-prototype))
	`(make-instance 'unit
			:factor ,(if (= (factor-of unit-prototype) 1)
				     value
				     `(* ,(factor-of unit-prototype) ,value))
			:units ,(units-of unit-prototype)))))

(defparameter *convert-unit-behaviour* :error)

(defun convert-unit (unit-from unit-to)
  (let ((unit-from (reduce-unit unit-from))
	(unit-to (reduce-unit unit-to)))
    (if (same-unit-p unit-from unit-to)
	(cond ((formula-unit-p unit-to)
	       (factor-of (funcall (formula-from unit-to) unit-from)))
	      ((or (additive-unit-p unit-from)
		   (additive-unit-p unit-to))
	       (- (factor-of unit-from) (factor-of unit-to)))
	      (t (/ (factor-of unit-from) (factor-of unit-to))))
	(case *convert-unit-behaviour*
	  (:error
	   (error "Invalid conversion between ~a and ~a" unit-from unit-to))
	  (:warn
	   (warn "Invalid conversion between ~a and ~a" unit-from unit-to)
	   :incorrect-conversion)
	  (t
	   :incorrect-conversion)))))
