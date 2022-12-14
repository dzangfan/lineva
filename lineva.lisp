(in-package :cl-user)

(defpackage :lineva
  (:nicknames :la)
  (:use :common-lisp)
  (:export #:instruction #:definst #:leva
           #:available-instructions #:describe-instruction)
  (:documentation "Package providing linear evaluation feature."))

(in-package :lineva)

(defmacro always-eval (&body body)
  "Equal to

  (eval-when (:compile-toplevel :load-toplevel :execute) 
    ...)"
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

(defparameter *expander-table*
  (make-hash-table :test #'eq)
  "A table of code transformer, whose keys are symbols and values are
function which has signature

  (KEY REST-CODE OTHER-PARAMETERS) -> CODE

KEY is its corresponding symbol in the *evaluator-table*, REST-CODE is
expanded code in linear evaluation. Take the following code for example:

  ((K₁ ...)
   (K₂ ...)
   (K₃ ...)
   (K₄ ...))

the function corresponding to K₂ will receive code expanded from
  
  ((K₃ ...)
   (K₄ ...))

as its REST-CODE. OTHER-PARAMETERS is a parameter list customized by
user, which correspond to ... in the code above.")

(defparameter *expander-document-table*
  (make-hash-table :test #'eq)
  "Documentation table of item in *expander-table*")

(defun find-expander (symbol)
  "Find expander in *expander-table* by SYMBOL, signal a error if
nothing is found."
  (or (gethash symbol *expander-table*)
      (error "Unknown key ~S in *EXPANDER-TABLE*" symbol)))

(always-eval
  (defparameter *rest-code-name* '$rest-code)

  (defun unary-p (list)
    "Return true if LIST is a list containing only one element."
    (and list (listp list) (null (cdr list)))))

(defmacro leva (&body body)
  "Evaluate every instruction in BODY sequentially. Every instruction
has one of following form:

  (:instruction-name ...)
  :instruction-name [= (:instruction-name)]
  compound-form

where compound-form can be any valid lisp form except the first
two. For example

  (la:leva
    (:let (x 10) (y 20))                  ; Define local variables
    :break                                ; Create breakpoint
    (list x y))                           ; Any Lisp form

See `definst' for more information about user-defined
instruction-name. See `available-instructions' for all defined
instructions. Use `describe-instruction' to get document of a
instruction."
  (loop :with body* := (reverse body)
        :and result := nil
        :for step :in body*
        :for step* := (if (keywordp step) (list step) step)
        :if (and step* (listp step*) (keywordp (car step*))) :do
          (destructuring-bind (key &rest params) step*
            (let* ((expander (find-expander key))
                   (rest-code (if (unary-p result)
                                  (first result)
                                  `(progn ,@result)))
                   (result* (funcall expander key rest-code params)))
              (setf result (list result*))))
        :else
          :do (push step* result)
        :end
        :finally
           (return
             (if (unary-p result)
                 (first result)
                 `(progn ,@result)))))

(always-eval
  (defun has-docstring-p (body)
    "Determine whether BODY has docstring."
    (and body (stringp (first body))))
  (defun extract-docstring (keyword lambda-list body)
    "Extract docstring and construct a better docstring by KEYWORD
and LAMBDA-LIST."
    (let ((origin-doc (if (has-docstring-p body) (first body) nil))
          (header (format nil "~S ~S" keyword lambda-list)))
      (if origin-doc
          (format nil "~A~%~%~A" header origin-doc)
          header))))

(defun define-instruction (keyword docstring function)
  "Define a new instruction called KEYWORD with FUNCTION. If the
instruction has been defined, the old definition will be ignored."
  (when (remhash keyword *expander-table*)
    (warn "Instruction ~S has been defined. Old definition will be ignored"
          keyword))
  (remhash keyword *expander-document-table*)
  (setf (gethash keyword *expander-table*) function
        (gethash keyword *expander-document-table*) docstring)
  (values))

(defmacro definst (keyword lambda-list &body body)
  "Define a instruction named KEYWORD. KEYWORD and LAMBDA-LIST
correspond to instruction in `leva':

  (KEYWORD LAMBDA-LIST...)

i.e.

  (:instruction ...)

the BODY is a code which yield real code that the instruction KEYWORD
defined. Special symbol `$rest-code' represents rest code of linear
evaluation. If the first element of BODY is a `string', it will be
seen as `documentation' of instruction KEYWORD. For example

  (definst :println (thing)
    \"Print THING and newline.\"
    `(progn (princ ,thing) (terpri)
            ,$rest-code))

then

  (la:leva (:println \"Hello world!\") (values))

will expand to

  (progn (princ \"Hello world!\") (terpri) (values))"
  (assert (keywordp keyword) (keyword)
          "Name of instruction should be a keyword, but found ~S" keyword)
  (let* ((key (gensym "key"))
         (params (gensym "params"))
         (doc (extract-docstring keyword lambda-list body))
         (body* (if (has-docstring-p body) (cdr body) body)))
    `(define-instruction ,keyword ,doc
       (lambda (,key ,*rest-code-name* ,params)
         (declare (ignore ,key))
         (destructuring-bind ,lambda-list ,params
           ,@body*)))))

(defmethod documentation (keyword (type (eql 'la:instruction)))
  (declare (ignore type))
  (gethash keyword *expander-document-table*))

(defmethod (setf documentation) (new-value object (type (eql 'la:instruction)))
  (declare (ignore type))
  (assert (keywordp object) (object)
          "Name of instruction should be a keyword, but found ~S" object)
  (assert (stringp new-value) (new-value)
          "Value of documentation should be a string, but found ~S" new-value)
  (setf (gethash object *expander-document-table*) new-value))

(defun available-instructions ()
  "Return all defined instructions."
  (loop :for key :being :the :hash-keys :of *expander-table*
        :collecting key :into keys
        :finally (return (sort keys #'string<))))

(defun describe-instruction (keyword &optional (stream *standard-output*))
  "Display documentation of KEYWORD."
  (let ((doc (documentation keyword 'instruction)))
    (if doc
        (princ doc stream)
        (format stream "Instruction is not found. Available instructions:~%~A"
                (available-instructions)))
    (terpri stream)))

;; Display

(definst :printf (format-string &rest arguments)
  "Print content to standard output. FORMAT-STRING and ARGUMENTS have
the same meaning of `format'.

  >>> (la:leva (:printf \"Hello ~S!~%\" :world))"
  `(progn (format t ,format-string ,@arguments)
          ,$rest-code))

(definst :println (thing)
  "Print content to standard output and add newline. Use `princ' to
output.

  >>> (la:leva (:println \"Hello world!\"))"
  `(progn (princ ,thing) (terpri)
          ,$rest-code))

(definst :pn (thing)
  "Print content to standard output and add newline. Use `prin1' to
output.

  >>> (la:leva (:pn \"Hello world!\"))"
  `(progn (prin1 ,thing) (terpri)
          ,$rest-code))

;; Local variable

(definst :let (&rest let-arguments)
  "Define local variables by `let'. LET-ARGUMENTS has the same
meaning of `let'.

  >>> (la:leva 
        (:let (x 10) (y 20))
        (+ x y))"
  `(let ,let-arguments ,$rest-code))

(definst :let-assert (&rest let-arguments)
  "Define local variables by `let' and assert its
value. LET-ARGUMENTS has the same meaning of `let'.

  >>> (la:leva
      (:let-assert (x 10) (y 20) (z nil))
      (+ x y z))"
  (if (null let-arguments)
      $rest-code
      (destructuring-bind (name value) (first let-arguments)
        `(let ((,name ,value))
           (assert ,name (,name) "~A should not be `nil'" ',name)
           (leva
             (:let-assert ,@(rest let-arguments))
             ,$rest-code)))))

(definst :flet (&rest flet-arguments)
  "Define local function by `flet', FLET-ARGUMENTS has the same
meaning with `flet'.

  >>> (la:leva
        (:flet (add1 (x) (+ 1 x))
               (dot2 (x) (* 2 x)))
        (dot2 (add1 10)))"
  `(flet ,flet-arguments ,$rest-code))

(definst :labels (&rest labels-arguments)
  "Define local function by `labels'. LABELS-ARGUMENTS has the same
meaning with `labels'

  >>> (la:leva
        (:labels (fib (n)
                      (if (< n 2)
                          1
                          (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 5))"
  `(labels ,labels-arguments ,$rest-code))

(definst :macrolet (&rest macrolet-arguments)
  "Define local macro by `macrolet'. MACROLET-ARGUMENTS has the same
meaning with `macrolet'.

  >>> (la:leva
        (:macrolet (record (&rest values) `(list ,@values)))
        (record \"Joe\" 20 nil))"
  `(macrolet ,macrolet-arguments ,$rest-code))

(definst :symbol-macrolet (&rest symbol-macrolet-arguments)
  " Define a local symbol-macro by `symbol-macrolet'.
SYMBOL-MACROLET-ARGUMENTS has the same meaning with
`symbol-macrolet'.

  >>> (la:leva (:symbol-macrolet (x (format t \"...~%\")))
        (list x x x))"
  `(symbol-macrolet ,symbol-macrolet-arguments ,$rest-code))

(definst :defun (name lambda-list &body body)
  "Define a local function by `labels'.

  >>> (la:leva
        (:defun fac (n)
          (if (zerop n)
              1
              (* n (fac (- n 1)))))
        (fac 3))"
  `(labels ((,name ,lambda-list ,@body)) ,$rest-code))

(definst :defvar (name &optional value)
  "Define a local variable by `let'.

  >>> (la:leva
        (:defvar x 10)
        x)"
  `(let ((,name ,value))
     ,$rest-code))

(definst :bind (lambda-list expression)
  "Define local variables by `destructuring-bind'.

  >>> (la:leva
        (:bind (a b &rest c) '(1 2 3 4 5))
        (list a b c))"
  `(destructuring-bind ,lambda-list ,expression ,$rest-code))

(definst :setf (place value &key (if t))
  "Invoke `setf' to set PLACE to VALUE if IF is not `nil'.

  >>> (la:leva
        (:defvar name :alexandria)
        (:setf name (symbol-name name)
               :if (not (stringp name)))
        name)"
  `(progn (when ,if (setf ,place ,value))
          ,$rest-code))

;; Debug

(definst :break (&optional format-control &rest format-arguments)
  "Enter debugger by call `break'. Arguments has the same meaning with
`break'.

  >>> (la:leva  
        (:break \"Let's ~A!!!\" :burn))"
  (let ((break-args (and format-control `(,format-control ,@format-arguments))))
    `(progn (break ,@break-args) ,$rest-code)))

(definst :inspect (object)
  "Enter inspector with OBJECT.

  >>> (la:leva
        (:defvar x '(:foo :bar))
        (:inspect x))"
  `(progn (inspect ,object) ,$rest-code))


(definst :assert (&rest conditions)
  "Quickly assert that all CONDITIONS is true.

  >>> (la:leva
        (:defvar x 10)
        (:assert (numberp x) (plusp x) (evenp x))
        x)"
  (if (null conditions)
      $rest-code
      `(progn
         (assert ,(first conditions))
         (leva (:assert ,@(cdr conditions))
           ,$rest-code))))

(definst :check-type (&rest check-type-parameters)
  "Invoke `check-type' over each element of CHECK-TYPE-PARAMETERS.

  >>> (la:leva
        (:let (name \"Joe\") (age 20))
        (:check-type (name (array character *) \"a string\")
                     (age (integer 0 150)))
        (list name age))"
  (if (null check-type-parameters)
      $rest-code
      `(progn
         (check-type ,@(car check-type-parameters))
         (leva (:check-type ,@(cdr check-type-parameters))
           ,$rest-code))))

;; Control flow

(definst :return (value &key (if t))
  "Return VALUE if condition IF is true.

  >>> (la:leva
        (:defvar x (read))
        (:return (- x) :if (minusp x))
        x)"
  `(if ,if ,value ,$rest-code))

(definst :try (&rest values)
  "Return first value in VALUES which is not `nil'. If all VALUES is
`nil', evaluate rest code.

  >>> (la:leva
        (:defvar table
          '(:bing \"cn.bing.com\"))
        (:try (getf table :google)
              (getf table :duckduckgo)
              (getf table :bing))
        \"No search engine available.\")"
  (if (null values)
      $rest-code
      `(or ,(car values)
           (leva (:try ,@(cdr values))
             ,$rest-code))))

(definst :defer (&rest forms)
  "Evaluate rest codes, then evaluate FORMS sequentially. Result of
rest code will be returned. Evaluation of rest code will be protected
by `unwind-protect'.

  >>> (la:leva
        (:defun close-conn () (format t \"Bye!~%\"))
        (format t \"Hello!~%\")
        (:defer (close-conn) (terpri))
        (format t \"[...]~%\"))"
  `(unwind-protect ,$rest-code
     ,@forms))
