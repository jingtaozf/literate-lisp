;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;; 
;; Filename: package.lisp
;; Description: 
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2018.11.11 11:02:26(+0800)
;; Last-Updated: 2018.11.14 15:16:20(+0800)
;;     Update #: 8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
(in-package :common-lisp-user)
(defpackage :literate-lisp 
  (:use :cl :named-readtables)
  (:documentation "a literate programming tool to write common lisp codes in org file."))
(pushnew :literate-lisp *features*)

(in-package :asdf)
(defclass org (cl-source-file)
  ((type :initform "org")))
(export '(org) :asdf)

