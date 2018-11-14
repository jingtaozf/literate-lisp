;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;; 
;; Filename: org-reader-demo.asd
;; Description: 
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2018.02.02 19:34:18(+0800)
;; Last-Updated: 2018.11.14 14:54:37(+0800)
;;     Update #: 49
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :org-reader-demo.system)
    (defpackage :org-reader-demo.system
      (:use :cl :asdf))))

(in-package :org-reader-demo.system)

(asdf:defsystem org-reader-demo
  :author "Xu Jingtao <jingtaozf@gmail.com>"
  :version "0.1"
  :serial t
  :description "an demo project of org-reader"
  :components ((:module org :pathname "./"
                        :components ((:org "readme"))))
  :properties ((version "0.1"))
  :depends-on (:org-reader))
