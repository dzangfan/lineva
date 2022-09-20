(in-package :asdf-user)

(defsystem :lineva
  :description "Linear evaluation macro"
  :version "0.0.1"
  :author "Li Dzangfan <li-fn@outlook.com>"
  :licence "GPLv3"
  :depends-on ()
  :components ((:file "packages")
               (:file "lineva" :depends-on ("packages"))))
