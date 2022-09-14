(in-package :cl-user)

(defpackage :lineva
  (:nicknames :la)
  (:use :common-lisp)
  (:export #:instruction #:definst #:leva #:$rest-code
           #:available-instructions #:describe-instruction)
  (:documentation "Package providing linear evaluation feature."))
