
(in-package :cl-user)

(defpackage :lineva/test
  (:use :common-lisp :fiveam))

(in-package :lineva/test)

(defmacro does-leva-expand-to (result (&rest leva-args))
  `(is (equalp (macroexpand '(la:leva ,@leva-args)) ,result)))

(test empty-leva
  (does-leva-expand-to '(progn) ()))

(test one-elt-leva
  (does-leva-expand-to 1 (1))
  (does-leva-expand-to '(- 1) ((- 1))))

(test many-trivial-elt-leva
  (does-leva-expand-to '(progn 1 2 3) (1 2 3))
  (does-leva-expand-to '(progn (print 1) 1.5 (print 2) 3)
      ((print 1) 1.5 (print 2) 3)))

(test fake-inst-leva
  (does-leva-expand-to ''(:println "Hello world!")
      ('(:println "Hello world!")))
  (does-leva-expand-to '(progn '(:println 1) '(:println 2))
      ('(:println 1) '(:println 2)))
  (does-leva-expand-to '(values :break)
      ((values :break))))

(test simple-inst-leva
  (does-leva-expand-to '(let ((x 10)) (progn))
      ((:let (x 10))))
  (does-leva-expand-to '(let ((x 10) (y 20)) (+ x y))
      ((:let (x 10) (y 20)) (+ x y)))
  (does-leva-expand-to '(progn (print 1) (print 2) (let ((x 10)) x))
      ((print 1) (print 2) (:let (x 10)) x))
  (does-leva-expand-to '(progn (print 1) (print 2) (let ((x 10)) (progn)))
      ((print 1) (print 2) (:let (x 10))))
  (does-leva-expand-to '(let ((x 10)) (progn (print 1) (print x)))
      ((:let (x 10)) (print 1) (print x))))

(test inst-local-variables
  (is (eql 30 (la:leva (:let (x 10) (y 20)) (+ x y))))
  (is (eql :error
           (handler-case (la:leva
                           (:let-assert (x 10) (y 20) (z nil)) (+ x y))
             (t () :error))))
  (is (eql 60 (la:leva (:let-assert (x 10) (y 20) (z 30)) (+ x y z))))
  (is (eql 22 (la:leva (:flet (add1 (x) (+ 1 x))
                         (dot2 (x) (* 2 x)))
                (dot2 (add1 10)))))
  (is (eql 8 (la:leva
               (:labels (fib (n)
                             (if (< n 2)
                                 1
                                 (+ (fib (- n 1)) (fib (- n 2))))))
               (fib 5))))
  (is (equal '("Joe" 20 nil)
           (la:leva
             (:macrolet (record (&rest values) `(list ,@values)))
             (record "Joe" 20 nil))))
  (let ((output (with-output-to-string (s)
                  (is (equal '(nil nil nil)
                             (la:leva (:symbol-macrolet (x (format s "...~%")))
                               (list x x x)))))))
    (is (string= (format nil "...~%...~%...~%") output)))
  (is (eql 6 (la:leva
               (:defun fac (n)
                 (if (zerop n)
                     1
                     (* n (fac (- n 1)))))
               (fac 3))))
  (is (eql 10 (la:leva
                (:defvar x 10)
                x)))
  (is (equal '(1 2 (3 4 5))
             (la:leva
               (:bind (a b &rest c) '(1 2 3 4 5))
               (list a b c))))
  (is (string= "ALEXANDRIA"
               (la:leva
                 (:defvar name :alexandria)
                 (:setf name (symbol-name name)
                        :if (not (stringp name)))
                 (string-upcase name)))))

(test inst-debug
  (does-leva-expand-to '(progn (print 1) (progn (break) (print 2)))
      ((print 1) :break (print 2)))
  (is (eql 10 (la:leva
                (:defvar x 10)
                (:assert (numberp x) (plusp x) (evenp x))
                x)))
  (is (eql :error
           (handler-case (la:leva
                           (:defvar x 11)
                           (:assert (numberp x) (plusp x) (evenp x))
                           x)
             (t () :error))))
  (is (equal '("Joe" 20)
             (la:leva
               (:let (name "Joe") (age 20))
               (:check-type (name (array character *) "a string")
                            (age (integer 0 150)))
               (list name age))))
  (is (eql :error
           (handler-case (la:leva
                           (:let (name "Joe") (age 200))
                           (:check-type (name (array character *) "a string")
                                        (age (integer 0 150)))
                           (list name age))
             (t () :error)))))

(test inst-control-flow
  (is (eql 10 (la:leva
                (:defvar x -10)
                (:return (- x) :if (minusp x))
                (error "Bye!"))))
  (is (string= "cn.bing.com"
               (la:leva
                 (:defvar table
                   '(:bing "cn.bing.com"))
                 (:try (getf table :google)
                       (getf table :duckduckgo)
                       (getf table :bing))
                 "No search engine available.")))
  (let ((output (with-output-to-string (s)
                  (is (eql :done
                           (la:leva
                             (:defun close-conn () (format s "Bye!~%"))
                             (format s "Hello!~%")
                             (:defer (close-conn) (terpri s))
                             (format s "[...]~%")
                             (values :done)))))))
    (is (string= (format nil  "Hello!~%[...]~%Bye!~%~%") output))))

(defmacro does-leva-output (content (&rest leva-args))
  (let ((s (gensym)))
    `(is (string= ,content
                  (with-output-to-string (,s)
                    (let ((*standard-output* ,s))
                      (la:leva ,@leva-args)))))))

(test inst-display
  (does-leva-output (format nil "Hello ~S!~%" :world)
      ((:printf "Hello ~S!~%" :world)))
  (does-leva-output (format nil "Hello world!~%")
      ((:println "Hello world!")))
  (does-leva-output (format nil "~S~%" "Hello world!")
      ((:pn "Hello world!"))))
