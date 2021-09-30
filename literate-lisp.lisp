;;; This file is automatically generated from file `literate-lisp.org'.
;;; It is not designed to be readable by a human.
;;; Please read file `literate-lisp.org' to find out the usage and implementation detail of this source file.

(in-package :common-lisp-user)
(defpackage :literate-lisp
  (:use :cl :cl-ppcre :iterate)
  (:nicknames :lp)
  (:export :install-globally :tangle-org-file :with-literate-syntax)
  (:documentation "a literate programming tool to write Common Lisp codes in org file."))
(pushnew :literate-lisp *features*)
(in-package :literate-lisp)

(defvar debug-literate-lisp-p nil)
(declaim (type boolean debug-literate-lisp-p))


(defvar current-org-context (make-hash-table))

(defun org-context (name)
  (gethash name current-org-context nil))

(defun set-org-context (name new-value)
  (setf (gethash name current-org-context) new-value))
(defsetf org-context set-org-context)

(defmacro define-lexer (name regex-pattern parameters &rest body)
  (let ((fun-name (intern (format nil "ORG-LEXER-FOR-~a" name))))
    `(progn (defun ,fun-name ,parameters
              ,@body)
            (if (assoc ',name (get 'lexer 'patterns))
                (setf (cdr (assoc ',name (get 'lexer 'patterns)))
                        (list ',fun-name ,regex-pattern ,(length parameters)))
                (push (list ',name ',fun-name ,regex-pattern ,(length parameters))
                  (get 'lexer 'patterns))))))

(defun run-patterns (line)
  (iter (for (name fun-name regex-pattern parameters-count) in (get 'lexer 'patterns))
        (multiple-value-bind (match-start match-end reg-starts reg-ends)
            (scan regex-pattern line)
          (declare (ignore match-end))
          (when match-start
            (iter (with arguments = nil)
                  (for i from 0 below parameters-count)
                  (for start-index = (aref reg-starts i))
                  (setf arguments
                          (nconc arguments
                                 (list (if start-index
                                           (subseq line start-index (aref reg-ends i))
                                           nil))))
                  (finally
                   (when debug-literate-lisp-p
                     (format t "apply pattern ~a with arguments ~a~%" name arguments))
                   (apply fun-name arguments)))
            (finish)))))

(defstruct headline 
  ;; the level
  (level 0 :type integer)
  ;; the content
  (content "" :type string)
  ;; the property specified for this headline
  (properties (make-hash-table :test #'equalp) :type hash-table))

(defun org-headlines ()
  (org-context :headline))

(defun set-org-headlines (new-value)
  (setf (org-context :headline) new-value))
(defsetf org-headlines set-org-headlines)

(defun current-headline ()
  (first (org-headlines)))

(defun current-headline-level ()
  (headline-level (first (org-headlines))))

(defun current-headline-content ()
  (headline-content (first (org-headlines))))

(defun pop-org-headline ()
  (pop (org-headlines)))

(defun push-org-headline (level content)
  (push (make-headline :level level :content content) (org-headlines)))

(defun setup-headline ()
  (push-org-headline 0 ""))

(define-lexer :headline "^\\s*(\\*+)\\s+(.*)$"
  (indicators content)
  (let ((level (length indicators))
        (previous-level (current-headline-level)))
    (cond ((= previous-level level)
           ;; meet a new headline with same level, pop the old one and push the new one
           (pop-org-headline)
           (push-org-headline level content))
          ((> previous-level level) 
           ;; meet a new headline with lower level, pop the old one until meet the same level. 
           (iter (pop-org-headline)
                 (until (< (current-headline-level) level)))
           (push-org-headline level content))
          (t
           ;; meet a new headline with higher level. 
           (push-org-headline level content)))
    (when debug-literate-lisp-p
      (format t "current headline, level:~D, content:~a~%"
              (current-headline-level)
              (current-headline-content)))))

(define-lexer :property-in-a-line "^\\s*\\#\\+PROPERTY:\\s*(\\S+)\\s+(.*)$"
  (key value)
  (when debug-literate-lisp-p
    (format t "Found property in level ~D, ~a:~a.~%"
            (current-headline-level) key value))
  (setf (gethash key (headline-properties (current-headline))) value))

(define-lexer :begin-of-properties "^(\\s*:PROPERTIES:\\s*)$"
  (line)
  (declare (ignore line))
  (when debug-literate-lisp-p
    (format t "Found beginning of properties.~%"))
  (setf (org-context :in-properties) t))

(define-lexer :end-of-properties "(^\\s*:END:\\s*$)"
  (line)
  (declare (ignore line))
  (when (org-context :in-properties)
    (when debug-literate-lisp-p
      (format t "Found end of properties.~%"))
    (setf (org-context :in-properties) nil)))

(define-lexer :property-in-properties "^\\s*:(\\S+):\\s*(\\S+.*)$"
  (key value)
  (when (org-context :in-properties)
    (format t "Found property in level ~D, ~a:~a.~%"
            (current-headline-level) key value)
    (setf (gethash key (headline-properties (current-headline))) value)))

(defun org-property-value (key)
  ;; (when debug-literate-lisp-p
  ;;   (format t "head lines:~a~%" (org-headlines)))
  (iter (for headline in (org-headlines))
        ;; (when debug-literate-lisp-p
        ;;   (format t "Check head line:~D:~a~%" (headline-level headline)
        ;;           (headline-content headline)))
        (for value = (gethash key (headline-properties headline)))
        (if value
            (return value))))

(defun load-p (feature)
  (cond ((eq :yes feature)
         t)
        ((eq :no feature)
         nil)
        ((null feature)
         ;; check current org property `literate-load'.
         (let ((load (org-property-value "literate-load")))
           (when debug-literate-lisp-p
             (format t "get current property value of literate-load:~a~%" load))
           (if load
               (load-p (first (read-org-code-block-header-arguments load 0)))
               t)))
        (t (or (find feature *features* :test #'eq)
             (when (eq :test feature)
               (find :literate-test *features* :test #'eq))))))

(defun read-org-code-block-header-arguments (string begin-position-of-header-arguments)
  (with-input-from-string (stream string :start begin-position-of-header-arguments)
    (let ((*readtable* (copy-readtable nil))
          (*package* #.(find-package :keyword))
          (*read-suppress* nil))
      (iter (for elem = (read stream nil))
            (while elem)
            (collect elem)))))

(defun start-position-after-space-characters (line)
  (iter (for c in-sequence line)
        (for i from 0)
        (until (not (find c '(#\Tab #\Space))))
        (finally (return i))))

(defvar org-lisp-begin-src-id "#+begin_src lisp")
(defvar org-name-property "#+NAME:")
(defvar org-name-property-length (length org-name-property))
(defvar org-block-begin-id "#+BEGIN_")
(defvar org-block-begin-id-length (length org-block-begin-id))
(defun sharp-space (stream a b)
  (declare (ignore a b))
  ;; reset org content in the beginning of the file;
  ;; here we assume sharp space meaning it.
  (setf current-org-context (make-hash-table))
  (setup-headline)
  (sharp-org stream))

(defun sharp-org (stream)
  (let ((named-code-blocks nil))
    (iter (with name-of-next-block = nil)
          (for line = (read-line stream nil nil))
          (until (null line))
          (for start1 = (start-position-after-space-characters line))
          (when debug-literate-lisp-p
            (format t "ignore line ~a~%" line))
          (run-patterns line)
          (until (and (equalp start1 (search org-lisp-begin-src-id line :test #'char-equal))
                      (let* ((header-arguments (read-org-code-block-header-arguments line (+ start1 (length org-lisp-begin-src-id)))))
                        (load-p (getf header-arguments :load)))))
          (cond ((equal 0 (search org-name-property line :test #'char-equal))
                 ;; record a name.
                 (setf name-of-next-block (string-trim '(#\Tab #\Space) (subseq line org-name-property-length))))
                ((equal 0 (search org-block-begin-id line :test #'char-equal))
                 ;; record the context of a block.
                 (if name-of-next-block
                     ;; start to read text in current block until reach `#+END_'
                     (let* ((end-position-of-block-name (position #\Space line :start org-block-begin-id-length))
                            (end-block-id (format nil "#+END_~a" (subseq line org-block-begin-id-length end-position-of-block-name)))
                            (block-stream (make-string-output-stream)))
                       (when (read-block-context-to-stream stream block-stream name-of-next-block end-block-id)
                         (setf named-code-blocks
                                 (nconc named-code-blocks
                                        (list (cons name-of-next-block
                                                    (get-output-stream-string block-stream)))))))
                     ;; reset name of code block if it's not sticking with a valid block.
                     (setf name-of-next-block nil)))
                (t
                 ;; reset name of code block if it's not sticking with a valid block.
                 (setf name-of-next-block nil))))
    (if named-code-blocks
        `(progn
           ,@(iter (for (block-name . block-text) in named-code-blocks)
                   (collect `(defparameter ,(intern (string-upcase block-name)) ,block-text))))
        ;; Can't return nil because ASDF will fail to find a form like `defpackage'.
        (values))))

(defun read-block-context-to-stream (input-stream block-stream block-name end-block-id)
  (iter (for line = (read-line input-stream nil))
        (cond ((null line)
               (return nil))
              ((string-equal end-block-id (string-trim '(#\Tab #\Space) line))
               (when debug-literate-lisp-p
                 (format t "reach end of block for '~a'.~%" block-name))
               (return t))
              (t
               (when debug-literate-lisp-p
                 (format t "read line for block '~a':~s~%" block-name line))
               (write-line line block-stream)))))

;;; If X is a symbol, see whether it is present in *FEATURES*. Also
;;; handle arbitrary combinations of atoms using NOT, AND, OR.
(defun featurep (x)
  #+allegro(excl:featurep x)
  #+lispworks(sys:featurep x)
  #-(or allegro lispworks)
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
  (declare (ignore sub-char numarg))
  (when debug-literate-lisp-p
    (format t "found #+END_SRC,start read org part...~%"))
  (funcall #'sharp-org stream))

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
  `(let ((*readtable* (copy-readtable)))
     ;; install it in current readtable
     (set-dispatch-macro-character #\# #\space #'literate-lisp::sharp-space)
     (set-dispatch-macro-character #\# #\+ #'literate-lisp::sharp-plus)
     ,@body))

#+named-readtables
(named-readtables:defreadtable syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\space #'sharp-space)
  (:dispatch-macro-char #\# #\+ #'sharp-plus))

(defun tangle-org-file (org-file &key (features *features*)
                                   (header ";;; This file is automatically generated from file `~a.~a'.
;;; It is not designed to be readable by a human.
;;; Please read file `~a.~a' to find out the usage and implementation detail of this source file.~%~%")
                                   (header-args (list (pathname-name org-file) (pathname-type org-file)
                                                      (pathname-name org-file) (pathname-type org-file)))
                                   (force-tangle nil)
                                   (output-file (make-pathname :defaults org-file :type "lisp")))
  (when (and (null force-tangle) (tangled-file-update-outside-p output-file))
    (error "The output file has been updated outside, please merge it into your org file before tangling!"))
  (let ((*features* features)
        ;; reset org context
        (current-org-context (make-hash-table)))
    (setup-headline)
    (with-open-file (input org-file)
      (with-open-file (output output-file :direction :output
                                          :if-does-not-exist :create
                                          :if-exists :supersede)
        (apply #'format output header header-args)
        (block read-org-files
          (iter
            ;; ignore all lines of org syntax.
            (sharp-org input)
            ;; start to read codes in code block until reach `#+END_SRC'
            (if (read-block-context-to-stream input output "LISP" "#+END_SRC")
                (write-line "" output)
                (return)))))))
  (cache-tangled-file output-file)
  t)

(defun tangled-cached-file (path)
  (translate-pathname (asdf/driver:resolve-absolute-location path)
                      #P"/**/*.*"
                      (merge-pathnames "literate-lisp/**/*.*" (asdf/driver:xdg-cache-home))))

(defun tangled-file-update-outside-p (file)
  (let ((cache-file (tangled-cached-file file)))
    (when (and (probe-file cache-file); It has never been tangled yet.
               (probe-file file))
      (string/= (uiop:read-file-string file)
                (uiop:read-file-string cache-file)))))

(defun cache-tangled-file (file)
  (let ((cache-file (tangled-cached-file file)))
    (ensure-directories-exist cache-file)
    (uiop:copy-file file cache-file)))

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

