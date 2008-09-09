(in-package :unit-formulas)

;;; a formula is a mathematical expression with units all operations must have defined allowed units
;;; terminals are either symbols naming variables, naming non-composite units, dimensionless
;;; numbers, lists describing operation, or lists decribing numeric constanst with units

(defparameter *operators* (make-hash-table))

;; operator kinds are: :agree :multiply :divide :expt :sqrt :dimensionless
(defmacro define-operators (&body operators)
  `(progn ,@(iter (for (ops kind) on operators by #'cddr)
		  (appending
		   (iter (for op in (ensure-list ops))
			 (collect `(setf (gethash ',op *operators*) ,kind)))))))

(define-operators (+ -) :agree
		  * :multiply
		  / :divide
		  expt :expt
		  sqrt :sqrt
		  (sin cos tan sinh cosh tanh) :dimensionless
		  (asin acos atan asinh acosh atanh) :dimensionless)

(defclass variable-with-unit (unit)
  ((name :initarg :name :accessor name-of)
   (value :initarg :value :accessor value-of :initform nil :documentation "Value of constant, if constant")))

(defun unitify-formula-terminals (formula env)
  "Takes variable/constant bindings in env and replaces them in formula"
  (etypecase formula
    (unit formula)
    (number (make-instance 'unit :factor formula))
    (symbol
     (let ((var (gethash formula env)))
       (if var
	   var
	   (let ((u (gethash formula *units*)))
	     (if u
		 u
		 (error "Symbol ~a does name neither variable nor a constant." formula))))))
    (list
     (cond ((numberp (car formula))
	    (reduce-unit formula))
	   ((gethash (car formula) *operators*)
	    (cons (car formula) (mapcar #'unitify-formula-terminals (cdr formula))))
	   (t (error "Unkown operation ~a." (car formula)))))))
