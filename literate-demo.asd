;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;;
;; Filename: literate-demo.asd
;; Description:
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2018.02.02 19:34:18(+0800)
;; Last-Updated: 2021.12.10 08:56:55(+0800)
;;     Update #: 69
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :literate-demo.system)
    (defpackage :literate-demo.system
      (:use :cl :asdf))))

(in-package :literate-demo.system)

(asdf:defsystem literate-demo
  :author "Xu Jingtao <jingtaozf@gmail.com>"
  :version "0.1"
  :licence "MIT"
  :serial t
  :description "a demo project of literate-lisp"
  :defsystem-depends-on ("literate-lisp")
  :depends-on (:iterate #+dev :clgplot)
  :components ((:module :demo :pathname "./"
                        :components ((:org "puzzle")
                                     (:org "readme"))))
  :properties ((version "0.1")))
