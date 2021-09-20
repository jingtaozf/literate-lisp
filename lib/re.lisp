;;;; The following codes are from https://github.com/massung/re
;;;; Regular Expressions for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :massung.re
  (:use :cl :massung.parse)
  (:export
   #:with-re
   #:with-re-match

   ;; interface
   #:compile-re
   #:match-re
   #:find-re
   #:split-re
   #:replace-re

   ;; match readers
   #:match-string
   #:match-groups
   #:match-pos-start
   #:match-pos-end))

(in-package :massung.re)

;;; ----------------------------------------------------

(defclass re ()
  ((pattern   :initarg :pattern    :reader re-pattern)
   (expr      :initarg :expression :reader re-expression))
  (:documentation "Regular expression."))

;;; ----------------------------------------------------

(defclass re-match ()
  ((match     :initarg :match      :reader match-string)
   (groups    :initarg :groups     :reader match-groups)
   (start-pos :initarg :start-pos  :reader match-pos-start)
   (end-pos   :initarg :end-pos    :reader match-pos-end))
  (:documentation "Matched pattern."))

;;; ----------------------------------------------------

(defmethod print-object ((re re) s)
  "Output a regular expression to a stream."
  (print-unreadable-object (re s :type t)
    (format s "~s" (re-pattern re))))

;;; ----------------------------------------------------

(defmethod print-object ((match re-match) s)
  "Output a regular expression match to a stream."
  (print-unreadable-object (match s :type t)
    (format s "~s" (match-string match))))

;;; ----------------------------------------------------

(defun tab-p (c)
  "T if c is a tab character."
  (char= c #\tab))

;;; ----------------------------------------------------

(defun space-p (c)
  "T if c is a whitespace character."
  (or (char= c #\tab)
      (char= c #\space)))

;;; ----------------------------------------------------

(defun return-p (c)
  "T if c is a return character."
  (char= c #\return))

;;; ----------------------------------------------------

(defun newline-p (c)
  "T if c is a newline character."
  (or (char= c #\return)
      (char= c #\linefeed)))

;;; ----------------------------------------------------

(defun word-char-p (c)
  "T if is alphanumeric or an underscore."
  (or (alphanumericp c) (char= c #\_)))

;;; ----------------------------------------------------

(defun punctuation-p (c)
  "T if c is a punctuation character."
  (find c "`~!@#$%^&*()-+=[]{}\|;:',./<>?\"" :test #'char=))

;;; ----------------------------------------------------

(defun hex-char-p (c)
  "T if c is a hexadecimal character."
  (digit-char-p c 16))

;;; ----------------------------------------------------

(define-parser re-parser
  "A regular expression is one or more expressions."
  (.let (ex (.many 're-expr))
    (.opt ex (.let (otherwise (.do (.is :or) 're-parser))
               (.ret `((:or ,ex ,otherwise)))))))

;;; ----------------------------------------------------

(define-parser re-expr
  "A single character, set, or loop of expressions."
  (.let (e (.or 're-boundary
                're-bounds
                're-char
                're-set
                're-group))

    ;; check to see if there is a following iteration token
    (.opt e (.or (.do (.is :*) (.ret (list :* e)))
                 (.do (.is :-) (.ret (list :- e)))
                 (.do (.is :+) (.ret (list :+ e)))
                 (.do (.is :?) (.ret (list :? e)))))))

;;; ----------------------------------------------------

(define-parser re-boundary
  "The start or end of a string."
  (.or (.do (.is :start) (.ret (list :start)))
       (.do (.is :end) (.ret (list :end)))))

;;; ----------------------------------------------------

(define-parser re-bounds
  "Lua-style %b bounds."
  (.let (bs (.is :bounds))
    (.ret (cons :bounds bs))))

;;; ----------------------------------------------------

(define-parser re-char
  "Match any character, exact character, or predicate function."
  (.or (.do (.is :any) (.ret '(:any)))

       ;; predicates and exact characters
       (.let (p (.is :is)) (.ret (list :is p)))
       (.let (c (.is :char)) (.ret (list :char c)))))

;;; ----------------------------------------------------

(define-parser re-set
  "Match from a set of characters."
  (.let* ((exclusive (.is :set))
          (predicates 're-set-chars))
    (flet ((any (c)
             (some #'(lambda (p) (funcall p c)) predicates)))
      (.ret (list (if exclusive :is-not :is) #'any)))))

;;; ----------------------------------------------------

(define-parser re-set-chars
  "Characters, character ranges, and named character sets."
  (.let (ps (.many1 (.or (.is :is)

                         ;; exact character
                         (.let (a 're-set-char)

                           ;; range of characters?
                           (.or (.let (z (.do (.is :-) 're-set-char))
                                  (.ret #'(lambda (c) (char<= a c z))))
                                (.ret #'(lambda (c) (char= c a))))))))

    ;; match the end of the set and return the predicates
    (.do (.is :end-set) (.ret ps))))

;;; ----------------------------------------------------

(define-parser re-set-char
  "Valid characters in a character set."
  (.or (.is :char)

       ;; special characters are aren't special in a set
       (.do (.is :any) (.ret #\.))
       (.do (.is :or) (.ret #\|))
       (.do (.is :*) (.ret #\*))
       (.do (.is :-) (.ret #\-))
       (.do (.is :+) (.ret #\+))
       (.do (.is :?) (.ret #\?))
       (.do (.is :group) (.ret #\())
       (.do (.is :end-group) (.ret #\)))))

;;; ----------------------------------------------------

(define-parser re-group
  "Match an optionally captured group."
  (.let* ((ignorep (.is :group))
          (xs 're-parser))
    (.do (.is :end-group)
         (.ret (list (if ignorep :ignore :capture) xs)))))

;;; ----------------------------------------------------

(defun is-not (pred)
  "Create a predicate that tests the inverse."
  #'(lambda (c) (not (funcall pred c))))

;;; ----------------------------------------------------

(defun escape (stream)
  "Return the test and predicate for an escaped character."
  (let ((c (read-char stream)))
    (case c

      ;; user-defined predicate
      (#\: (let ((sym (with-output-to-string (s)
                        (do ((c (read-char stream)
                                (read-char stream)))
                            ((eql c #\:))
                          (write-char c s)))))
             (values :is (read-from-string sym))))

      ;; boundary test
      (#\b (let ((b1 (read-char stream))
                 (b2 (read-char stream)))
             (values :bounds (list b1 b2))))

      ;; named inclusive sets
      (#\s (values :is #'space-p))
      (#\t (values :is #'tab-p))
      (#\r (values :is #'return-p))
      (#\n (values :is #'newline-p))
      (#\a (values :is #'alpha-char-p))
      (#\l (values :is #'lower-case-p))
      (#\u (values :is #'upper-case-p))
      (#\d (values :is #'digit-char-p))
      (#\w (values :is #'word-char-p))
      (#\x (values :is #'hex-char-p))
      (#\p (values :is #'punctuation-p))

      ;; named exclusive sets
      (#\S (values :is (is-not #'space-p)))
      (#\T (values :is (is-not #'tab-p)))
      (#\R (values :is (is-not #'return-p)))
      (#\N (values :is (is-not #'newline-p)))
      (#\A (values :is (is-not #'alpha-char-p)))
      (#\L (values :is (is-not #'lower-case-p)))
      (#\U (values :is (is-not #'upper-case-p)))
      (#\D (values :is (is-not #'digit-char-p)))
      (#\W (values :is (is-not #'word-char-p)))
      (#\X (values :is (is-not #'hex-char-p)))
      (#\P (values :is (is-not #'punctuation-p)))

      ;; just a character
      (otherwise (values :char c)))))

;;; ----------------------------------------------------

(defun parse-re (pattern)
  "Parse a regular expression pattern."
  (with-input-from-string (stream pattern)
    (flet ((token-reader ()
             (let ((c (read-char stream nil nil)))
               (when c
                 (case c

                   ;; any character
                   (#\. :any)

                   ;; escaped characters
                   (#\% (escape stream))

                   ;; iterators
                   (#\* :*)
                   (#\+ :+)
                   (#\? :?)

                   ;; lazy iterator, or end of set
                   (#\- (if (eql (peek-char nil stream nil nil) #\])
                            (values :char #\-)
                          (values :-)))

                   ;; conditional
                   (#\| :or)

                   ;; groups
                   (#\( (if (eql (peek-char nil stream nil nil) #\?)
                            (values :group (read-char stream))
                          (values :group ())))

                   ;; sets
                   (#\[ (if (eql (peek-char nil stream nil nil) #\^)
                            (values :set (read-char stream))
                          (values :set ())))

                   ;; group and set terminals
                   (#\) :end-group)
                   (#\] :end-set)

                   ;; start/end boundary
                   (#\^ :start)
                   (#\$ :end)

                   ;; default to just an exact character match
                   (otherwise (values :char c)))))))

      ;; parse all the tokens in the regular expression
      (parse 're-parser #'token-reader))))

;;; ----------------------------------------------------

(defun compile-re (pattern)
  "Create a regular expression from a pattern string."
  (let ((re (make-array 8 :adjustable t :fill-pointer 0))
        (bs (make-array 4 :adjustable t :fill-pointer 0)))
    (labels ((compile-op (op &rest args)
               (vector-push-extend (cons op args) re))

             ;; branch labels
             (make-label ()
               (vector-push-extend nil bs))
             (resolve-label (label)
               (setf (aref bs label) (fill-pointer re)))

             ;; compile a list of tokens recursively
             (compile-tokens (xs)
               (loop
                  for (op x y) in xs

                  ;; compile each token
                  do (case op

                       ;; if not x then y
                       (:or (let ((this (make-label))
                                  (else (make-label))
                                  (done (make-label)))
                              (compile-op :split this else)
                              (resolve-label this)
                              (compile-tokens x)
                              (compile-op :jump done)
                              (resolve-label else)
                              (compile-tokens y)
                              (resolve-label done)))

                       ;; zero or more (greedy)
                       (:* (let ((try (make-label))
                                 (done (make-label))
                                 (again (make-label)))
                             (resolve-label again)
                             (compile-op :split try done)
                             (resolve-label try)
                             (compile-tokens (list x))
                             (compile-op :jump again)
                             (resolve-label done)))

                       ;; zero or more (lazy)
                       (:- (let ((try (make-label))
                                 (done (make-label))
                                 (again (make-label)))
                             (resolve-label again)
                             (compile-op :split done try)
                             (resolve-label try)
                             (compile-tokens (list x))
                             (compile-op :jump again)
                             (resolve-label done)))

                       ;; one or more matches
                       (:+ (let ((rep (make-label))
                                 (done (make-label)))
                             (resolve-label rep)
                             (compile-tokens (list x))
                             (compile-op :split rep done)
                             (resolve-label done)))

                       ;; maybe match
                       (:? (let ((this (make-label))
                                 (else (make-label)))
                             (compile-op :split this else)
                             (resolve-label this)
                             (compile-tokens (list x))
                             (resolve-label else)))

                       ;; Lua boundary
                       (:bounds (let ((try (make-label))
                                      (done (make-label))
                                      (again (make-label)))
                                  (compile-op :char x)
                                  (resolve-label again)
                                  (compile-op :split done try)
                                  (resolve-label try)
                                  (compile-op :any)
                                  (compile-op :jump again)
                                  (resolve-label done)
                                  (compile-op :char y)))

                       ;; ignore groups just match tokens
                       (:ignore (compile-tokens x))

                       ;; capture groups push, match, and pop
                       (:capture (progn
                                   (compile-op :push)
                                   (compile-tokens x)
                                   (compile-op :pop)))

                       ;; all other tokens compile to themselves
                       (otherwise (compile-op op x))))))

      ;; compile the parsed tokens
      (compile-tokens (parse-re pattern))

      ;; resolve all labels in split and jump instuctions
      (dotimes (i (length re))
        (symbol-macrolet ((b1 (second (aref re i)))
                          (b2 (third (aref re i))))
          (case (first (aref re i))
            (:jump  (setf b1 (aref bs b1)))
            (:split (setf b1 (aref bs b1)
                          b2 (aref bs b2))))))

      ;; finally, append the match instruction
      (compile-op :match)

      ;; return the regular expression
      (make-instance 're :pattern pattern :expression re))))

;;; ----------------------------------------------------

(defstruct (re-thread (:constructor make-re-thread (pc sp groups stack)))
  pc        ; program counter in compiled re instruction vector
  sp        ; string pointer
  groups    ; pushed capture groups (subseq)
  stack)    ; pushed capture groups (sp)

;;; ----------------------------------------------------

(defun match (s thread start offset)
  "Create a re-match from a thread that matched."
  (with-slots (sp groups)
      thread
    (let ((cs (let (cs)
                (do ((g (pop groups)
                        (pop groups)))
                    ((null g) cs)
                  (push (subseq s (first g) (second g)) cs)))))
      (make-instance 're-match
                     :start-pos (+ start offset)
                     :end-pos sp
                     :groups cs
                     :match (subseq s (+ start offset) sp)))))

;;; ----------------------------------------------------

(defun run (re s start end &aux (pc 0) (offset 0))
  "Execute a regular expression program."
  (loop
     with threads = (list (make-re-thread pc (+ start offset) nil nil))

     ;; get the next thread off the list
     for thread = (pop threads)

     ;; once all the threads have been exhausted, the match fails
     until (null thread)

     ;; evaluate the next thread, checking for a match
     do (with-slots (pc sp groups stack)
            thread
          (loop
             for (op x y) = (aref re pc)

             ;; get the current character
             for c = (when (< sp end) (char s sp))

             ;; advance the program counter
             do (incf pc)

             ;; loop until the thread fails an instruction
             while (case op

                     ;; start and end boundaries
                     (:start   (= sp 0))
                     (:end     (= sp (length s)))

                     ;; match any character
                     (:any     (when (< sp end)
                                 (incf sp)))

                     ;; match an exact character
                     (:char    (when (eql c x)
                                 (incf sp)))

                     ;; match a predicate function
                     (:is      (when (and c (funcall x c))
                                 (incf sp)))

                     ;; fail to match a predicate function
                     (:is-not  (when (and c (not (funcall x c)))
                                 (incf sp)))

                     ;; push a capture group
                     (:push    (let ((capture (list sp)))
                                 (push capture stack)
                                 (push capture groups)))

                     ;; pop a capture group
                     (:pop     (rplacd (pop stack) (list sp)))

                     ;; jump to an instruction
                     (:jump    (setf pc x))

                     ;; fork a thread
                     (:split   (let ((b (make-re-thread y sp groups stack)))
                                 (push b threads)
                                 (setf pc x)))

                     ;; successfully matched, create and return
                     (:match   (return-from run
                                 (match s thread start offset))))))))

;;; ----------------------------------------------------

(defmacro with-re ((re pattern) &body body)
  "Compile pattern if it's not a RE object and execute body."
  (let ((p (gensym)))
    `(let ((,p ,pattern))
       (let ((,re (if (subtypep (type-of ,p) 're)
                      ,p
                    (compile-re ,p))))
         (progn ,@body)))))

;;; ----------------------------------------------------

(defmacro with-re-match ((match match-expr &key no-match) &body body)
  "Intern match symbols to execute a body."
  (let (($$ (intern "$$" *package*))
        ($1 (intern "$1" *package*))
        ($2 (intern "$2" *package*))
        ($3 (intern "$3" *package*))
        ($4 (intern "$4" *package*))
        ($5 (intern "$5" *package*))
        ($6 (intern "$6" *package*))
        ($7 (intern "$7" *package*))
        ($8 (intern "$8" *package*))
        ($9 (intern "$9" *package*))
        ($_ (intern "$_" *package*))
        ($* (intern "$*" *package*)))
    `(let ((,match ,match-expr))
       (if (null ,match)
           ,no-match
         (let ((,$$ (match-string ,match))
               (,$* (match-groups ,match)))
           (declare (ignorable ,$$ ,$*))
           (symbol-macrolet ((,$1 (first ,$*))
                             (,$2 (second ,$*))
                             (,$3 (third ,$*))
                             (,$4 (fourth ,$*))
                             (,$5 (fifth ,$*))
                             (,$6 (sixth ,$*))
                             (,$7 (seventh ,$*))
                             (,$8 (eighth ,$*))
                             (,$9 (ninth ,$*))
                             (,$_ (nthcdr 9 ,$*)))
             (progn ,@body)))))))

;;; ----------------------------------------------------

(defun match-re (pattern s &key exact (start 0) (end (length s)))
  "Test a pattern re against a string."
  (with-re (re pattern)
    (let ((m (run (re-expression re) s start end)))
      (if (not exact)
          m
        (when m
          (and (= (match-pos-end m) end) m))))))

;;; ----------------------------------------------------

(defun find-re (pattern s &key all (start 0) (end (length s)))
  "Find a regexp pattern match somewhere in a string."
  (with-re (re pattern)
    (let ((i start))
      (flet ((next-match ()
               (loop
                  until (>= i end)

                  ;; is there a match at this offset?
                  for m = (run (re-expression re) s i end)

                  ;; return the found match or advance the offset
                  do (if m
                         (return (prog1 m
                                   (setf i (match-pos-end m))))
                       (incf i)))))
        (if all
            (loop for m = (next-match) while m collect m)
          (next-match))))))

;;; ----------------------------------------------------

(defun split-re (pattern s &key all coalesce-seps (start 0) (end (length s)))
  "Split a string into one or more strings by pattern match."
  (with-re (re pattern)
    (let* ((seqs (list nil)) (tail seqs))
      (do ((m (find-re re s :start start :end end)
              (find-re re s :start start :end end)))
          ((null m))

        ;; only split if not all, coalescing, or there's something there
        (when (or (not coalesce-seps) (> (match-pos-start m) start))
          (let ((split (subseq s start (match-pos-start m))))
            (setf tail (cdr (rplacd tail (list split))))))

        ;; update the search position after the split
        (setf start (match-pos-end m))

        ;; stop after a single match?
        (unless (or all (eq tail seqs))
          (return)))

      ;; add everything that's left
      (when (< start end)
        (rplacd tail (list (subseq s start end))))

      ;; return the list or two values
      (if all
          (rest seqs)
        (values-list (rest seqs))))))

;;; ----------------------------------------------------

(defun replace-re (pattern with s &key all (start 0) (end (length s)))
  "Replace patterns found within a string with a new value."
  (with-re (re pattern)
    (with-output-to-string (rep nil :element-type 'character)
      (do ((m (find-re re s :start start :end end)
              (find-re re s :start start :end end)))
          ((null m))

        ;; write out everything up to the match
        (when (< start (match-pos-start m))
          (write-string s rep :start start :end (match-pos-start m)))

        ;; replace the match with a value
        (princ (if (functionp with) (funcall with m) with) rep)

        ;; update the search position after the match
        (setf start (match-pos-end m))

        ;; stop after a single replace?
        (unless all (return)))

      ;; add everything that's left
      (when (< start end)
        (write-string s rep :start start :end end)))))

;;; ----------------------------------------------------

(defmethod make-load-form ((re re) &optional env)
  "Tell the system how to save and load a regular expression to a FASL."
  (declare (ignore env))
  `(compile-re ,(re-pattern re)))

;;; ----------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((dispatch-re (s c n)
           (declare (ignorable c n))
           (let ((delim (read-char s)))
             (compile-re (with-output-to-string (re)
                           (do ((c (read-char s t nil t)
                                   (read-char s t nil t)))
                               ((char= c delim))
                             (if (char= c #\\)
                                 (princ (read-char s t nil t) re)
                               (princ c re))))))))
    (set-dispatch-macro-character #\# #\r #'dispatch-re)))
