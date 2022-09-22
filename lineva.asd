(in-package :asdf-user)

(defsystem :lineva
  :description "Linear evaluation macro system"
  :version "0.0.1"
  :author "Li Dzangfan <li-fn@outlook.com>"
  :licence "GPLv3"
  :depends-on ()
  :components ((:file "lineva"))
  :in-order-to ((test-op (test-op :lineva/test))))

(defsystem :lineva/test
  :description "lineva's test suite"
  :author "Li Dzangfan <li-fn@outlook.com>"
  :license "GPLv3"
  :depends-on (:fiveam :lineva)
  :components ((:file "lineva-test"))
  :perform (test-op (o c) (symbol-call :fiveam :run!)))
