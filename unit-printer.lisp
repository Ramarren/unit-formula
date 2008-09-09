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