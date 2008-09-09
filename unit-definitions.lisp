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
			       (setf (gethash (intern (symbol-name name) (find-package :units-formulas))
					      *units*) unit-instance)))))))

(defmacro define-factors (unit &rest factor-definitions)
  `(define-units ,@(iter (for (factor-names factor) on factor-definitions by #'cddr)
			 (appending `(,factor-names (,factor ,unit))))))
