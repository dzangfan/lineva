(in-package :lineva)

(defmacro always-eval (&body body)
  "Equal to

  (eval-when (:compile-toplevel :load-toplevel :execute) 
    ...)"
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

(always-eval
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

  (defparameter *rest-code-name* '$rest-code))

(defmacro leva (&body body)
  "Evaluate every instruction in BODY sequentially. Every instruction
has one of following form:

  :instruction-name
  (:instruction-name ...)
  compound-form

where compound-form can be any valid lisp form except the first
two. See `definst' for more information about user-defined
instruction-name."
  (loop :with body* := (reverse body)
        :and need-head-p := nil
        :and result := nil
        :for step :in body*
        :for step* := (if (keywordp step) (list step) step)
        :if (and step* (listp step*) (keywordp (car step*))) :do
          (destructuring-bind (key &rest params) step*
            (let* ((expander (find-expander key))
                   (rest-code (if need-head-p `(progn ,@result) result))
                   (result* (funcall expander key rest-code params)))
              (setf result result*
                    need-head-p nil)))
        :else
          :do (setf result (cons step* result)
                    need-head-p t)
        :end
        :finally
           (return
             (if need-head-p
                 `(progn ,@result)
                 result))))

(defmacro definst (keyword lambda-list &body body)
  "Define a instruction named KEYWORD. KEYWORD and LAMBDA-LIST
correspond to step in `eva':

  (KEYWORD LAMBDA-LIST...)

i.e.

  (:instruction ...)

the BODY is a code which yield real code that the instruction KEYWORD
defined. Special symbol `$rest-code' represents rest code of linear
evaluation. See `*expander-table*' for more information. If the first
element of BODY is a `string', it will be seen as `documentation' of
instruction KEYWORD."
  (assert (keywordp keyword) (keyword)
          "Name of instruction should be a keyword, but found ~S" keyword)
  (when (remhash keyword *expander-table*)
    (warn "Instruction ~S has been defined. Old definition will be ignored"
          keyword))
  (remhash keyword *expander-document-table*)
  (let* ((key (gensym "key"))
         (params (gensym "params"))
         (doc (if (and body (stringp (first body))) (car body) nil))
         (body* (if doc (cdr body) body)))
    (setf (gethash keyword *expander-table*)
          (eval `(lambda (,key ,*rest-code-name* ,params)
                   (declare (ignore ,key))
                   (destructuring-bind ,lambda-list ,params
                     ,@body*))))
    (when doc (setf (gethash keyword *expander-document-table*) doc))))

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

(definst :println (format-string &rest arguments)
  "Print content to standard output and append new line. FORMAT-STRING
and ARGUMENTS have the same meaning of `format'.

  >>> (la:leva (:println \"Hello, ~A!\" \"world\"))"
  `(progn (format t ,format-string ,@arguments)
          (terpri)
          ,$rest-code))

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
           (assert ,name (,name) "~A should be general true" ',name)
           (leva
             (:let-assert ,@(rest let-arguments))
             ,$rest-code)))))