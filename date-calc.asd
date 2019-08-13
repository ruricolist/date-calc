(defsystem "date-calc"
  :description "Package for simple date calculation"
  :author "Heiko Schröter <schroete@iup.physik.uni-bremen.de>"
  :maintainer "Paul M. Rodriguez <pmr@ruricolist.com>"
  :licence "GPL or Artistic"
  :in-order-to ((test-op (test-op "date-calc/test")))
  :version "0.2"
  :components
  ((:file "date-calc")))

(defsystem "date-calc/test"
  :description "Test suite for date-calc."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("date-calc" "fiveam")
  :perform (test-op (o c) (symbol-call :date-calc/test :run-date-calc-tests))
  :pathname "test/"
  :serial t
  :components ((:file "package")
               (:file "test")))
