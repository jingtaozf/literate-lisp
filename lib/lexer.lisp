;;;; The following codes are from https://github.com/massung/lexer with some modifications
;;;; Lexing package for Common Lisp
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

(defpackage :massung.lexer
  (:use :cl :massung.re)
  (:nicknames :lex)
  (:export
   #:define-lexer

   ;; lexer state macros
   #:with-lexer
   #:with-token-reader

   ;; token functions
   #:read-token
   #:tokenize
   #:slurp

   ;; lexing functions
   #:read-from-lexer
   #:push-lexer
   #:pop-lexer
   #:swap-lexer

   ;; lexbuf readers
   #:lexbuf-string
   #:lexbuf-pos
   #:lexbuf-end
   #:lexbuf-line
   #:lexbuf-source

   ;; token readers
   #:token-line
   #:token-lexeme
   #:token-source
   #:token-class
   #:token-value))

(in-package :massung.lexer)

;;; ----------------------------------------------------

(defclass lexstate ()
  ((stack  :initarg :stack  :reader lexstate-stack)
   (lexbuf :initarg :lexbuf :reader lexstate-buf))
  (:documentation "The input parameter for DEFINE-LEXER."))

;;; ----------------------------------------------------

(defclass lexbuf ()
  ((string :initarg :string :reader lexbuf-string)
   (source :initarg :source :reader lexbuf-source)
   (pos    :initarg :pos    :reader lexbuf-pos)
   (end    :initarg :end    :reader lexbuf-end)
   (line   :initarg :line   :reader lexbuf-line))
  (:documentation "The source string being matched against."))

;;; ----------------------------------------------------

(defclass token ()
  ((line   :initarg :line   :reader token-line)
   (lexeme :initarg :lexeme :reader token-lexeme)
   (source :initarg :source :reader token-source)
   (class  :initarg :class  :reader token-class)
   (value  :initarg :value  :reader token-value))
  (:documentation "A parsed token from a lexbuf."))

;;; ----------------------------------------------------

(define-condition lex-error (error)
  ((reason :initarg :reason :reader lex-error-reason)
   (line   :initarg :line   :reader lex-error-line)
   (source :initarg :source :reader lex-error-source))
  (:documentation "Signaled during tokenizing when no matching pattern.")
  (:report (lambda (c s)
             (with-slots (reason line source)
                 c
               (format s "~a on line ~a~@[ of ~s~]" reason line source)))))

;;; ----------------------------------------------------

(defmethod initialize-instance :after ((buf lexbuf) &key &allow-other-keys)
  "Setup the range of the buffer and validate it."
  (with-slots (string pos end line)
      buf
    (let ((len (length string)))

      ;; set default values
      (unless pos (setf pos 0))
      (unless end (setf end len))

      ;; validate the buffer range for parsing
      (assert (<= 0 pos end len))

      ;; set the starting line
      (setf line (1+ (count #\newline string :end pos))))))

;;; ----------------------------------------------------

(defun make-lex-error (state reason)
  "Generate an error with an optional token."
  (let ((line (lexbuf-line (lexstate-buf state)))
        (source (lexbuf-source (lexstate-buf state))))
    (error (make-instance 'lex-error
                          :reason reason
                          :line line
                          :source source))))

;;; ----------------------------------------------------

(defmethod print-object ((token token) s)
  "Output a token to a stream."
  (print-unreadable-object (token s :type t)
    (with-slots (class value)
        token
      (format s "~a~@[ ~s~]" class value))))

;;; ----------------------------------------------------

(defmacro define-lexer (lexer (state) &body productions)
  "Create a tokenize function."
  (let ((s (gensym "string"))
        (src (gensym "source"))
        (i (gensym "pos"))
        (end (gensym "end"))
        (line (gensym "line"))
        (next-token (gensym "next-token"))
        (m (gensym "match"))
        (class (gensym "class"))
        (value (gensym "value")))
    `(defun ,lexer (,state)
       (with-slots ((,s string)
                    (,src source)
                    (,i pos)
                    (,end end)
                    (,line line))
           (lexstate-buf ,state)
         (tagbody
            ,next-token

            ;; try each of the patterns in order
            ,@(loop
                 for p in productions
                 collect
                   (with-re (re (pop p))
                     `(let ((,m (match-re ,re ,s :start ,i :end ,end)))
                        (when ,m
                          (let ((,s (match-string ,m)))

                            ;; count lines, update the parse offset
                            (incf ,line (count #\newline ,s))
                            (setf ,i (match-pos-end ,m))

                            ;; evaluate the production form
                            (multiple-value-bind (,class ,value)
                                (with-re-match (,m ,m) ,@p)
                              (if (eq ,class :next-token)
                                  (go ,next-token)
                                (return-from ,lexer
                                  (when ,class
                                    (make-instance 'token
                                                   :class ,class
                                                   :value ,value
                                                   :line ,line
                                                   :source ,src
                                                   :lexeme ,s))))))))))

            ;; no pattern matched; error if not at the end
            (when (< ,i ,end)
              (error "Lexing error")))))))

;;; ----------------------------------------------------

(defmacro with-lexer ((var lexer string &key source start end) &body body)
  "Create a new lexbuf and lexer list for tokenizing."
  (let ((buf (gensym "buf")))
    `(let* ((,buf (make-instance 'lexbuf
                                 :string ,string
                                 :source ,source
                                 :pos ,start
                                 :end ,end))

            ;; create the lexstate object as a stack of lexers
            (,var (make-instance 'lexstate
                                 :lexbuf ,buf
                                 :stack (list ,lexer))))

       ;; execute the body
       (progn ,@body))))

;;; ----------------------------------------------------

(defmacro with-token-reader ((var lexer) &body body)
  "Create a lexer state and a function to read tokens, then execute."
  (let ((token (gensym "token"))
        (state (gensym "state")))
    `(let ((,token nil)
           (,state ,lexer))
       (let ((,var #'(lambda ()
                       (when (setf ,token (read-token ,state))
                         (values (token-class ,token)
                                 (token-value ,token))))))
         (handler-bind
             ((warning #'muffle-warning)

              ;; on errors, show where the error took place
              (condition #'(lambda (c) (make-lex-error ,state c))))
           (progn ,@body))))))

;;; ----------------------------------------------------

(defun read-from-lexer (state class &key eval)
  "Reads from the lexbuf and returns it as the value."
  (with-slots (string pos end)
      (lexstate-buf state)
    (multiple-value-bind (value position)
        (read-from-string string t nil :start (1- pos) :end end)
      (multiple-value-prog1
          (values class (if eval (eval value) value))
        (setf pos position)))))

;;; ----------------------------------------------------

(defun push-lexer (state lexer class &optional value)
  "Push a new lexer and return a token."
  (multiple-value-prog1
      (values class value)
    (push lexer (slot-value state 'stack))))

;;; ----------------------------------------------------

(defun pop-lexer (state class &optional value)
  "Pop the top lexer and return a token."
  (multiple-value-prog1
      (values class value)
    (pop (slot-value state 'stack))))

;;; ----------------------------------------------------

(defun swap-lexer (state lexer class &optional value)
  "Set the current lexer and return a token."
  (multiple-value-prog1
      (values class value)
    (rplaca (slot-value state 'stack) lexer)))

;;; ----------------------------------------------------

(defun read-token (state)
  "Reads the next token in the current lexbuf using the top lexer."
  (if (null (lexstate-stack state))
      nil
    (funcall (first (lexstate-stack state)) state)))

;;; ----------------------------------------------------

(defun tokenize (lexer string &optional source)
  "Create a lexbuf and parse until there are not more tokens or lexer."
  (with-lexer (state lexer string :source source)
    (handler-case
        (loop for tok = (read-token state) while tok collect tok)
      (condition (err)
        (make-lex-error state err)))))

;;; ----------------------------------------------------

(defun slurp (pathname &key (element-type 'character))
  "Read a file into a string sequence."
  (with-open-file (stream pathname :element-type element-type)
    (let* ((len (file-length stream))
           (seq (make-array len :element-type element-type :fill-pointer t)))
      (prog1 seq
        (setf (fill-pointer seq) (read-sequence seq stream))))))
