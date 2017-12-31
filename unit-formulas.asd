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

(asdf:defsystem unit-formulas
    :version "0"
    :description "Formulas with unit checking and conversion"
    :maintainer " <ramarren@cignet.higersbergernet>"
    :author " <ramarren@cignet.higersbergernet>"
    :licence "GPLv2"
    :depends-on (:iterate :alexandria)
    :components ((:file "package")
		 (:file "unit-printer" :depends-on ("package"))
		 (:file "unit-class" :depends-on ("package" "unit-printer"))
		 (:file "unit-query" :depends-on ("package" "unit-class"))
		 (:file "unit-definitions"
			:depends-on ("package" "unit-operations" "unit-class"))
		 (:file "unit-operations"
			:depends-on ("package" "unit-class"))
		 (:file "unit-data" :depends-on ("package" "unit-definitions"))
		 (:file "formulas" :depends-on ("package" "unit-operations" "unit-definitions"))
		 (:file "dimensional-analysis"
			:depends-on ("package")))
    )
