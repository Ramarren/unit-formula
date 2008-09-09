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

(defparameter *base-units*
  (list :meter 0 :second 1 :kelvin 2 :kilogram 3 :ampere 4 :mole 5 :candela 6))

;;; order matters -- keyword print-string
(defparameter *unit-print-list*
  (list :kilogram "kg" :meter "m" :second "s" :kelvin "K" :ampere "A" :candela "cd" :mole "mol"))

(defun print-unit-vector (unit-vector stream)
  (let ((first-print t))
   (iter (for (keyword print-string) on *unit-print-list* by #'cddr)
	 (for power = (aref unit-vector (getf *base-units* keyword)))
	 (unless (zerop power)
	   (unless first-print
	     (format stream " "))
	   (setf first-print nil)
	   (if (= power 1)
	       (format stream "~a" print-string)
	       (format stream "~a^~a" print-string power))))))