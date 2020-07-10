;;; This file is automatically generated from file `literate-lisp.org'.
;;; It is not designed to be readable by a human.
;;; Please read file `literate-lisp.org' to find out the usage and implementation detail of this source file.

(in-package :common-lisp-user)
(defpackage :literate-lisp
  (:use :cl)
  (:nicknames :lp)
  (:export :install-globally :tangle-org-file :with-literate-syntax)
  (:documentation "a literate programming tool to write Common Lisp codes in org file."))
(pushnew :literate-lisp *features*)
(in-package :literate-lisp)

(defvar debug-literate-lisp-p nil)
(declaim (type boolean debug-literate-lisp-p))


(defun load-p (feature)
  (case feature
    ((nil :yes) t)
    (:no nil)
    (t (or (find feature *features* :test #'eq)
           (when (eq :test feature)
             (find :literate-test *features* :test #'eq))))))

(defun read-org-code-block-header-arguments (string begin-position-of-header-arguments)
  (with-input-from-string (stream string :start begin-position-of-header-arguments)
    (let ((*readtable* (copy-readtable nil))
          (*package* #.(find-package :keyword))
          (*read-suppress* nil))
       (loop for elem = (read stream nil)
                     while elem
                     collect elem))))

(defun start-position-after-space-characters (line)
  (loop for c of-type character across line
        for i of-type fixnum from 0
        until (not (find c '(#\Tab #\Space)))
        finally (return i)))

(defvar org-lisp-begin-src-id "#+begin_src lisp")
(defun sharp-space (stream a b)
  (declare (ignore a b))
  (loop for line = (read-line stream nil nil)
        until (null line)
        for start1 = (start-position-after-space-characters line)
        do (when debug-literate-lisp-p
             (format t "ignore line ~a~%" line))
        until (and (equalp start1 (search org-lisp-begin-src-id line :test #'char-equal))
                   (let* ((header-arguments (read-org-code-block-header-arguments line (+ start1 (length org-lisp-begin-src-id)))))
                     (load-p (getf header-arguments :load :yes)))))
  (values))

;;; If X is a symbol, see whether it is present in *FEATURES*. Also
;;; handle arbitrary combinations of atoms using NOT, AND, OR.
(defun featurep (x)
  (typecase x
    (cons
     (case (car x)
       ((:not not)
        (cond
          ((cddr x)
           (error "too many subexpressions in feature expression: ~S" x))
          ((null (cdr x))
           (error "too few subexpressions in feature expression: ~S" x))
          (t (not (featurep (cadr x))))))
       ((:and and) (every #'featurep (cdr x)))
       ((:or or) (some #'featurep (cdr x)))
       (t
        (error "unknown operator in feature expression: ~S." x))))
    (symbol (not (null (member x *features* :test #'eq))))
    (t
      (error "invalid feature expression: ~S" x))))

(defun read-feature-as-a-keyword (stream)
  (let ((*package* #.(find-package :keyword))
        ;;(*reader-package* nil)
        (*read-suppress* nil))
    (read stream t nil t)))

(defun handle-feature-end-src (stream sub-char numarg)
  (when debug-literate-lisp-p
    (format t "found #+END_SRC,start read org part...~%"))
  (funcall #'sharp-space stream sub-char numarg))

(defun read-featurep-object (stream)
  (read stream t nil t))

(defun read-unavailable-feature-object (stream)
  (let ((*read-suppress* t))
    (read stream t nil t)
    (values)))

(defun sharp-plus (stream sub-char numarg)
  (let ((feature (read-feature-as-a-keyword stream)))
    (when debug-literate-lisp-p
      (format t "found feature ~s,start read org part...~%" feature))
    (cond ((eq :END_SRC feature) (handle-feature-end-src stream sub-char numarg))
          ((featurep feature)    (read-featurep-object stream))
          (t                     (read-unavailable-feature-object stream)))))

(defun install-globally ()
  (set-dispatch-macro-character #\# #\space #'sharp-space)
  (set-dispatch-macro-character #\# #\+ #'sharp-plus))
#+literate-global(install-globally)

(defmacro with-literate-syntax (&body body)
  (let ((original-reader-for-sharp-space (gensym "READER-FUNCTION"))
        (original-reader-for-sharp-plus (gensym "READER-FUNCTION")))
    `(let ((,original-reader-for-sharp-space (get-dispatch-macro-character #\# #\Space))
           (,original-reader-for-sharp-plus (get-dispatch-macro-character #\# #\+))
           (*readtable* #-allegro *readtable* #+allegro(copy-readtable nil)))
       ;; install it in current readtable
       (set-dispatch-macro-character #\# #\space #'literate-lisp::sharp-space)
       (set-dispatch-macro-character #\# #\+ #'literate-lisp::sharp-plus)
       (unwind-protect
           (progn ,@body)
         ;; restore our modifications to current readtable if necessary.
         (when (eq #'literate-lisp::sharp-space (get-dispatch-macro-character #\# #\Space))
           (set-dispatch-macro-character #\# #\Space ,original-reader-for-sharp-space))
         (when (eq #'literate-lisp::sharp-plus (get-dispatch-macro-character #\# #\+))
           (set-dispatch-macro-character #\# #\+ ,original-reader-for-sharp-plus))))))

(defun tangle-org-file (org-file &key
                        (keep-test-codes nil)
                        (output-file (make-pathname :defaults org-file
                                                    :type "lisp")))
  (let ((*features* (if keep-test-codes
                      *features*
                      (remove-if #'(lambda (feature)
                                     (find feature '(:literate-test :test)))
                                 *features*))))
    (with-open-file (input org-file)
      (with-open-file (output output-file :direction :output
                              :if-does-not-exist :create
                              :if-exists :supersede)
        (format output
                ";;; This file is automatically generated from file `~a.~a'.
;;; It is not designed to be readable by a human.
;;; Please read file `~a.~a' to find out the usage and implementation detail of this source file.~%~%"
                (pathname-name org-file) (pathname-type org-file)
                (pathname-name org-file) (pathname-type org-file))
        (block read-org-files
          (loop do
            ;; ignore all lines of org syntax.
            (sharp-space input nil nil)
            ;; start to read codes in code block until reach `#+end_src'
            (loop for line = (read-line input nil nil)
                  do
               (cond ((null line)
                      (return-from read-org-files))
                     ((string-equal "#+end_src" (string-trim '(#\Tab #\Space) line))
                      (when debug-literate-lisp-p
                        (format t "reach end of source code block.~%"))
                      (write-line "" output)
                      (return))
                     (t
                      (when debug-literate-lisp-p
                        (format t "read code line:~s~%" line))
                      (write-line line output))))))))))

(defclass asdf::org (asdf:cl-source-file)
  ((asdf::type :initform "org")))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(asdf::org) :asdf))

(defmethod asdf:perform :around (o (c asdf:org))
  (literate-lisp:with-literate-syntax
    (call-next-method)))

(defmethod asdf/system:find-system :around (name &optional (error-p t))
  (literate-lisp:with-literate-syntax
    (call-next-method)))

