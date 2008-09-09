(asdf:defsystem unit-formulas
    :version "0"
    :description "Formulas with unit checking and conversion"
    :maintainer " <ramarren@cignet.higersbergernet>"
    :author " <ramarren@cignet.higersbergernet>"
    :licence "GPLv2"
    :depends-on (:iterate :alexandria :bpm)
    :components ((:file "package")
		 (:file "unit-definitions" :depends-on ("package" "unit-operations"))
		 (:file "unit-operations" :depends-on ("package"))
		 (:file "unit-data" :depends-on ("package" "unit-definitions"))))

