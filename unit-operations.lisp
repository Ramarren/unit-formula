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

;;; units are described by s-expressions, with operations: * / expt sqrt
;;; * is implied if car of a list is other symbol

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
		 :units (map 'vector (curry #'/ 2) (units-of unit))))

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
     (let ((substitution
	    (gethash (find-symbol (symbol-name unit-description)
				  (load-time-value (find-package :unit-formulas) t))
		     *units*)))
       (if substitution substitution (error "Unknown unit ~a" unit-description))))
    (list
     (case (car unit-description)
       (* (multiply-units (mapcar #'reduce-unit (cdr unit-description))))
       (/ (divide-units (mapcar #'reduce-unit (cdr unit-description))))
       (expt (expt-units (reduce-unit (cadr unit-description)) (caddr unit-description)))
       (sqrt (sqrt-units (reduce-unit (cadr unit-description))))
       (t (multiply-units (mapcar #'reduce-unit unit-description)))))))

(defun convert-unit (unit-from unit-to)
  (let ((unit-from (reduce-unit unit-from))
	(unit-to (reduce-unit unit-to)))
    (if (same-unit-p unit-from unit-to)
	(/ (factor-of unit-from) (factor-of unit-to))
	:incorrect-conversion)))
