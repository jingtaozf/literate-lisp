;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;; 
;; Filename: literate-lisp.asd
;; Description: package definition of literate-lisp.
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2018.02.02 19:34:18(+0800)
;; Last-Updated: 2019.01.05 12:58:23(+0800)
;;     Update #: 55
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :literate-lisp.system)
    (defpackage :literate-lisp.system
      (:use :cl :asdf))))

(in-package :literate-lisp.system)

(asdf:defsystem literate-lisp
  :author "Xu Jingtao <jingtaozf@gmail.com>"
  :licence "MIT"
  :version "0.1"
  :serial t
  :description "a literate programming tool to write common lisp codes in org file."
  :components ((:module basics :pathname "./"
                        :components ((:file "tangle"))
                        :serial t)
               )
  :properties ((version "0.1")))
