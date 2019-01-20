;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;; 
;; Filename: literate-demo.asd
;; Description: 
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2018.02.02 19:34:18(+0800)
;; Last-Updated: 2019.01.20 23:34:51(+0800)
;;     Update #: 56
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :literate-demo.system)
    (defpackage :literate-demo.system
      (:use :cl :asdf))))
(cl:eval-when (:load-toplevel :execute)
  #+quicklisp (ql:quickload :literate-lisp)
  #-quicklisp (asdf:load-system :literate-lisp))

(in-package :literate-demo.system)

(asdf:defsystem literate-demo
  :author "Xu Jingtao <jingtaozf@gmail.com>"
  :version "0.1"
  :licence "MIT"
  :serial t
  :description "an demo project of literate-lisp"
  :components ((:module org :pathname "./"
                        :components ((:org "readme"))))
  :properties ((version "0.1"))
  :depends-on (:literate-lisp))
