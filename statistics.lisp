(ql:quickload :uiop :silent t)
(ql:quickload :alexandria :silent t)

(defparameter *maximum-depth* 3)

(defun read-all-forms (file-name)
  (let ((sharp-dot (get-dispatch-macro-character #\# #\.)))
    (set-dispatch-macro-character
     #\# #\. (lambda (s c n)
               (declare (ignore s c n))
               nil))
    (unwind-protect
         (handler-bind ((error (lambda (c)
                                 (let ((r (find-restart 'continue c)))
                                   (when r (invoke-restart r))))))
           (with-open-file (stream file-name)
             (loop :for next-form := (read stream nil :end-of-file)
                   :for i :from 1
                   :until (eq :end-of-file next-form)
                   :collecting next-form)))
      (set-dispatch-macro-character #\# #\. sharp-dot))))

(defun collect-toplevel-forms (forms predicate)
  (loop :for form :in forms
        :if (funcall predicate form)
          :collecting form))

(defun collect-forms (forms predicate &optional (depth *maximum-depth*))
  (unless (plusp depth) (return-from collect-forms nil))
  (let ((collected nil))
    (when (and (not (alexandria:circular-list-p forms))
               (alexandria:proper-list-p forms))
      (when (funcall predicate forms)
        (push forms collected))
      (loop :for form :in forms
            :for more-forms := (collect-forms form predicate (- depth 1))
            :do (setf collected (append more-forms collected))))
    collected))

(defun symbol-approx-equal (s1 s2)
  (and (symbolp s1)
       (symbolp s2)
       (string-equal (symbol-name s1)
                     (symbol-name s2))))

(defun is-a (operator-symbol form)
  (and form (symbol-approx-equal operator-symbol (car form))))

(defun form<defun+long-body> (form)
  (and (alexandria:proper-list-p form)
       (or (is-a :defun form) (is-a :defmacro form))
       (let ((body (cdddr form)))
         (when (and body (stringp (first body)))
           (setf body (cdr body)))
         (cdr body))))

(defun extract-concern (file-name-list)
  (loop :for file-name :in file-name-list
        :for forms := (handler-case (read-all-forms file-name)
                        (t () nil))
        :for concern := (collect-forms forms #'form<defun+long-body>)
        :appending concern))

(defun summary-concern (defuns)
  (loop :with counter := (make-hash-table :test #'eq)
        :for d :in defuns
        :for body := (cdddr d)
        :for body* := (if (stringp (first body)) (cdr body) body) :do
          (loop :for concern :in body* :do
            (cond ((listp concern) (symbolp (first concern))
                   (incf (gethash (car concern) counter 0)))
                  ((atom concern)
                   (incf (gethash concern counter 0)))))
        :finally
           (let ((alist (alexandria:hash-table-alist counter)))
             (return (sort alist #'> :key #'cdr)))))

(defun statis-source* (&rest file-name-list)
  (let* ((file-names (apply #'list* file-name-list))
         (concerns (extract-concern file-names)))
    (summary-concern concerns)))

(defun statis-source (&rest file-names)
  (statis-source* file-names))

(defparameter directories ()
  "Parameter: Root directories containing lisp sources directly or
indirectly.")

(defparameter files
  (loop :for dir :in directories
        :appending (uiop:directory-files dir "**/*.lisp")))

(defparameter result (statis-source* files))

(remove-if (lambda (record) (< (cdr record) 10))
           result)

'(:date "2022-9-18"
  :description
  "Most frequently used forms in `defun' and `defmacro' which have
more than one form in their bodies. Ran over 671 lisp files."
  :result ((DECLARE . 513) (LET . 163) (WHEN . 124) (CHECK-TYPE . 118)
           (UNLESS . 79) (LET* . 62) (SETF . 53) (LOOP . 49) (IF . 39)
           (SB-INT:QUASIQUOTE . 34) (MULTIPLE-VALUE-BIND . 33)
           (ASSERT . 29) (FORMAT . 27) (OR . 23) (VALUES . 21)
           (NIL . 19) (SETQ . 17) (COND . 17) (FAST-WRITE-SEQUENCE . 14)
           (CLRHASH . 14) (SIGNAL-ERROR-IF-CURRENT-THREAD . 14) (T . 14)
           (TEST-BROKEN-INPUT . 13) (APPLY . 13) (DOLIST . 13)
           (ETYPECASE . 12) (ECASE . 10) (STRING-UPCASE . 10)))
