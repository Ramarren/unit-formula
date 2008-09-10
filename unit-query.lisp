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

(defparameter *quantities*
  '(mass length time temperature current substance luminosity force area
    volume power energy speed acceleration pressure density charge
    electric-potential electric-field capacitance resistance
    conductance magnetic-field magnetic-flux
    inductance frequency time-squared dose)
  "List of symbols naming unit synonyms for base quantities")

(defun query-unit (unit)
  "Return a plist of unit value and unit exponents"
  (append (list :value (factor-of unit))
	  (iter (with unit-vector = (units-of unit))
		(for (keyword index) on *base-units* by #'cddr)
		(for power = (aref unit-vector index))
		(unless (zerop power)
		  (collect keyword)
		  (collect power)))))
