(asdf:defsystem unit-formulas
    :version "0"
    :description "Formulas with unit checking and conversion"
    :maintainer " <ramarren@cignet.higersbergernet>"
    :author " <ramarren@cignet.higersbergernet>"
    :licence "GPLv2"
    :depends-on (:iterate :alexandria :bpm)
    :components ((:file "package")
		 (:file "unit-printer" :depends-on ("package"))
		 (:file "unit-class" :depends-on ("package" "unit-printer"))
		 (:file "unit-definitions" :depends-on ("package" "unit-operations" "unit-class"))
		 (:file "unit-operations" :depends-on ("package" "unit-class"))
		 (:file "unit-data" :depends-on ("package" "unit-definitions"))
		 (:file "formulas" :depends-on ("package" "unit-operations" "unit-definitions"))))

