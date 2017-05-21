;;     unit-formulas - library for unit checked formula definitions
;;     Copyright (C) 2017 Elliott Johnson

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

(defparameter *operators* (make-hash-table))
(defparameter *formulae* (make-hash-table))

(defclass formula-unit (unit)
  ((formula-to :accessor formula-to :initarg :formula-to)
   (formula-from :accessor formula-from :initarg :formula-from)))

(defmethod initialize-instance :after ((formula-unit formula-unit) &key)
  (with-accessors ((units-of units-of)
		   (formula-to formula-to)
		   (formula-from formula-from))
      formula-unit
    (setf units-of
	  (cond (formula-to (units-of (gethash formula-to *formulae*)))
		(formula-from (units-of (gethash formula-from *formulae*)))
		(t units-of)))))

(defun formula-unit-p (unit)
  (typep unit 'formula-unit))
