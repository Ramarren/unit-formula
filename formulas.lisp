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

;;; a formula is a mathematical expression with units all operations must have defined allowed units
;;; terminals are either symbols naming variables, naming non-composite units, dimensionless
;;; numbers, lists describing operation, or lists decribing numeric constanst with units

(defparameter *operators* (make-hash-table))
(defparameter *formulae* (make-hash-table))

;; operator kinds are: :agree :multiply :divide :expt :sqrt :dimensionless :formula
(defmacro define-operators (&body operators)
  `(progn ,@(iter (for (ops kind) on operators by #'cddr)
		  (appending
		   (iter (for op in (ensure-list ops))
			 (collect `(setf (gethash ',op *operators*) ,kind)))))))

(define-operators
  if :if
  (< > <= >=) :comparision
  (+ - abs) :agree
  * :multiply
  / :divide
  expt :expt
  sqrt :sqrt
  (sin cos tan sinh cosh tanh) :dimensionless
  (asin acos atan asinh acosh atanh) :dimensionless
  (log exp) :dimensionless)

(defclass variable-with-unit (unit)
  ((name :initarg :name :accessor name-of)
   (value :initarg :value :accessor value-of :initform nil :documentation "Value of constant, if constant")))

(defmethod print-object ((unit variable-with-unit) stream)
  (print-unreadable-object (unit stream :type t :identity nil)
    (format stream "~a ~a*~a " (name-of unit) (if (value-of unit)
						  (value-of unit)
						  1)
	    (factor-of unit))
    (print-unit-vector (units-of unit) stream)))

(defun unitify-formula-terminals (formula env)
  "Takes variable/constant bindings in env and replaces them in formula"
  (etypecase formula
    (unit formula)
    (number (make-instance 'unit :factor formula))
    (symbol
       (let ((var (gethash formula env)))
	 (if var
	     var
	     (let ((u (gethash (intern (symbol-name formula) (find-package :unit-formulas))
			       *units*)))
	       (if u
		   u
		   (error "Symbol ~a does name neither variable nor a constant." formula))))))
    (list
       (cond ((numberp (car formula))
	      (reduce-unit formula))
	     ((gethash (car formula) *operators*)
	      (cons (car formula) (mapcar (rcurry #'unitify-formula-terminals env) (cdr formula))))
	     ((gethash (car formula) *formulae*)
	      (cons (car formula) (mapcar (rcurry #'unitify-formula-terminals env) (cdr formula))))
	     (t (error "Unkown operation ~a." (car formula)))))))

(defun verify-formula (formula)
  "Takes unitified formula and returns it's return unit, if correct, signals error if not."
  (if (atom formula)
      formula
      (let ((kind (gethash (car formula) *operators*)))
	(ecase kind
	  (:if
	      (destructuring-bind (if-op condition true-result false-result) formula
		;; there always must be both results for this to work
		(assert (eql if-op 'if));;sanity check, also to suppress ignored variable warning
		(let ((true-result-unit (verify-formula true-result))
		      (false-result-unit (verify-formula false-result))
		      (condition-unit (verify-formula condition)))
		  (if (and (same-unit-p true-result-unit false-result-unit)
			   (eql condition-unit :logical))
		      true-result-unit
		      (error "Units do not agree in ~a" formula)))))
	  ((:agree :comparision :multiply :divide :expt :sqrt :dimensionless :formula)
	     (let ((args (mapcar #'verify-formula (cdr formula))))
	       (case kind
		 (:comparision (if (reduce #'same-unit-p args)
				   :logical
				   (error "Units do not agree in ~a" formula)))
		 (:agree (if (reduce #'same-unit-p args)
			     (first args)
			     (error "Units do not agree in ~a" formula)))
		 (:multiply
		    (multiply-units args))
		 (:divide
		    (divide-units args))
		 (:expt
		    (destructuring-bind (unit power) args
		      (if (dimensionless-p power)
			  (expt-units unit (factor-of power))
			  (error "Power in expt in ~a cannot have an unit" formula))))
		 (:sqrt
		    (if (length= args 1)
			(sqrt-units (car args))
			(error "Cannot sqrt more that one argument in ~a." formula)))
		 (:dimensionless
		    (if (every #'dimensionless-p args)
			(car args)
			(error "Operation ~a needs dimensionless arguments." formula)))
		 (:formula
		  (gethash (car formula) *formulae*)))))))))


;;; formula variables and constant are described by (name unit &optional value)
(defun make-formula-environment (variable-specs)
  (alist-hash-table
   (iter (for spec in variable-specs)
	 (destructuring-bind (name unit &optional (value nil)) spec
	   (let ((unit-instance (reduce-unit unit)))
	     (collect
	      (cons name
		    (make-instance 'variable-with-unit
				   :factor (factor-of unit-instance)
				   :units (units-of unit-instance)
				   :name name
				   :value value))))))))

(defun replace-formula-terminals (formula &key (in-formula nil))
  "Take unitified environment and transform it into list passable to defun, with all units conversions in place."
  (etypecase formula
    (variable-with-unit
     (if in-formula
	 (if (value-of formula)
	     ;; If we have a value then we can make a unit using that value.
	     (make-instance 'unit
			    :factor (* (value-of formula) (factor-of formula))
			    :units (units-of formula))
	     ;; If we don't have a value then a variable will provide the value down the road.
	     `(make-instance 'unit
			     :factor (* ,(factor-of formula) ,(name-of formula))
			     :units ,(units-of formula)))
	 (if (value-of formula)
	     (* (value-of formula) (factor-of formula))
	     (if (= (factor-of formula) 1)
		 (name-of formula)
		 `(* ,(factor-of formula) ,(name-of formula))))))
    (unit
     (factor-of formula))
    (list
     (let ((formulae (gethash (car formula) *formulae*)))
       (if formulae
	   ;; Call the formula with arguments that reference the provided variables... finally
	   ;; call convert-unit to return the results as a value for further processing.  Kind of
	   ;; convoluted, but it should work for now.
	   `(convert-unit ,(cons (car formula)
				 (mapcar #'(lambda (f) (replace-formula-terminals f :in-formula t))
					 (cdr formula)))
			  ;; Maybe there is an easier way to specify the unit of the final value.
			  ,(make-instance 'unit :factor 1 :units (units-of formulae)))
	   (cons (car formula)
		 (mapcar #'replace-formula-terminals (cdr formula))))))))

;;; in-spec defines variables and constant with which formula is described defformula creates a
;;; function which takes arguments in forms (name value &optional unit), converts them, checks if
;;; they agree with in-spec and then plugs the numbers into the closure created by replace-formula
;;; terminals which works on numbers directly (note argument order)

;;; modification: value can be either a number, in which case it is of unit unit, or dimensionless
;;; if not provided, or be unit instance, in which case it's factor is a value. This is returned
;;; from formulas later.

(defun strip-constants-from-spec (in-spec)
  (iter (for spec in in-spec)
	      (destructuring-bind (name unit &optional (value nil)) spec
		(unless value
		  (collect (list name (reduce-unit unit)))))))

(defun make-argument-list (in-spec)
  "Eliminate constants and sort in-spec (same thing as create env above)."
  (sort (strip-constants-from-spec in-spec)
	#'string< :key #'car))

(defun reduce-argument-list (args arglist)
  "Take arguments and argument list and match them, returning a list of properly sorted argument values,
 and error if units don't match."
  (iter (for (name unit) in arglist)
	(for (arg-name arg-value . arg-unit) = (assoc name args))
	(collect
	 (etypecase arg-value
	   (null (error "Missing argument ~a." name))
	   (unit (if (same-unit-p arg-value unit)
		     (/ (factor-of arg-value) (factor-of unit))
		     (error "Argument ~a has wrong unit, is ~a should be ~a." name arg-value unit)))
	   (number (let ((arg-unit (reduce-unit arg-unit)))
		     (if (same-unit-p arg-unit unit)
			 (* arg-value (/ (factor-of arg-unit) (factor-of unit)))
			 (error "Argument ~a has wrong unit, is ~a should be ~a." name arg-unit unit))))))))

(defmacro defformula (name (&rest in-spec) formula-expression)
  (let ((arglist (make-argument-list in-spec))
	(env (make-formula-environment in-spec)))
    (let ((formula (unitify-formula-terminals formula-expression env)))
      (let ((formula-units (units-of (verify-formula formula))))
	(with-gensyms (args internal-function)
	  `(defun ,name (&rest ,args)
	     (labels ((,internal-function ,(mapcar #'car arglist)
			,(replace-formula-terminals formula)))
	       (make-instance 'unit
			      :factor (apply (function ,internal-function)
					     (reduce-argument-list ,args ',arglist))
			      :units ,formula-units))))))))

(defmacro defformula* (name (&rest in-spec) formula-exprression)
  "This creates a formula using positional arguments, with much less error checking. If wrong unit
is passed it will still fail, because symbol will be passed to the formula."
  (let ((arglist (strip-constants-from-spec in-spec))
	(env (make-formula-environment in-spec)))
    (let ((formula (unitify-formula-terminals formula-exprression env)))
      (let ((formula-units (units-of (verify-formula formula))))
	(with-gensyms (internal-function)
	  `(defun ,name ,(mapcar #'car arglist)
	     (labels ((,internal-function ,(mapcar #'car arglist)
			,(replace-formula-terminals formula)))
	       (make-instance 'unit
			      :factor (,internal-function
				       ,@(iter (for (arg unit) in arglist)
					       (collect `(convert-unit ,arg ,unit))))
			      :units ,formula-units))))))))

(defmacro defformulae* (name (&rest in-spec) formula-expression)
  "This creates a formula using positional arguments, with much less error checking. If wrong unit
is passed it will still fail, because symbol will be passed to the formula.  Formula-expression
allows for nesting of formulas."
  (let* ((arglist (strip-constants-from-spec in-spec))
	 (env (make-formula-environment in-spec))
	 (formula (unitify-formula-terminals formula-expression env))
	 (verified-units (verify-formula formula))
	 (formula-units (units-of verified-units)))
    (with-gensyms (internal-function)
      `(prog1
	   (defun ,name ,(mapcar #'car arglist)
	     (labels ((,internal-function ,(mapcar #'car arglist)
			,(replace-formula-terminals formula)))
	       (make-instance 'unit
			      :factor (,internal-function
				       ,@(iter (for (arg unit) in arglist)
					       (collect `(convert-unit ,arg ,unit))))
			      :units ,formula-units)))
	 (setf (gethash ',name *operators*) :formula
	       (gethash ',name *formulae*) ',verified-units)))))
