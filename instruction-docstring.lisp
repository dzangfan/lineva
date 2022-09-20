(ql:quickload :str)

(defparameter *docstring-seperator*
  (make-string 2 :initial-element #\Newline))

(defparameter *docstring-example-tag*
  "  >>> ")

(defparameter *docstring-example-tag-length* 6)

(defun split-docstring (docstring)
  (la:leva
    (:bind (lambda-list detail example)
           (str:split *docstring-seperator* docstring))
    (:let (example-lines (str:split #\Newline example)))
    (:let (first-line (first example-lines)))
    (let ((tag (str:substring 0 *docstring-example-tag-length* first-line)))
      (assert (string-equal *docstring-example-tag* tag)))
    (:defvar example-lines*
      (mapcar (lambda (line)
                (str:substring *docstring-example-tag-length* nil line))
              example-lines))
    (:let (example* (str:join #\Newline example-lines*)))
    (values lambda-list detail example*)))

(defun docstring->org-block (instruction &optional (title-level 4))
  (with-output-to-string (output)
    (let ((title (str:repeat title-level "*")))
      (format output "~A ~(~S~)~%~%" title instruction))
    (let ((docstring (documentation instruction 'la:instruction)))
      (assert docstring)
      (multiple-value-bind (lambda-list detail example)
          (split-docstring docstring)
        (format output "*lambda-list*: ~~~A~~~%~%" lambda-list)
        (format output "~A~%~%" detail)
        (format output "#+begin_src lisp~%")
        (format output "~A~%" example)
        (format output "#+end_src~%~%")))))

(defun docstring->org-block+group
    (group-name instruction-list &optional (title-level 3))
  (with-output-to-string (output)
    (let ((title (str:repeat title-level "*")))
      (format output "~A ~A~%~%" title group-name))
    (loop :for inst :in instruction-list :do
      (format output "~A~%" (docstring->org-block inst (+ 1 title-level))))))

(defun output-docstring->org (stream title-level body-list)
  (loop :for group :in body-list :do
    (destructuring-bind (group &rest instructions) group
      (princ (docstring->org-block+group group instructions title-level)
             stream))))

(defmacro with-docstring->org (title-level &body body)
  `(output-docstring->org *standard-output* ,title-level ',body))

(with-docstring->org 3
  ("Local Variables"
   :let :let-assert :flet :labels :macrolet :symbol-macrolet
   :defun :defvar :bind :setf)
  ("Debug" :break :inspect
           :assert :check-type)
  ("Contro Flow" :return :try :defer)
  ("Display" :printf
             :println
             :pn))
