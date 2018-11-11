;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;; 
;; Filename: org-reader.asd
;; Description: package definition of org-reader.
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2018.02.02 19:34:18(+0800)
;; Last-Updated: 2018.11.11 11:07:54(+0800)
;;     Update #: 49
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :org-reader.system)
    (defpackage :org-reader.system
      (:use :cl :asdf))))

(in-package :org-reader.system)

(asdf:defsystem org-reader
  :author "Xu Jingtao <jingtaozf@gmail.com>"
  :version "0.1"
  :serial t
  :description "a literate programming tool to write common lisp codes in org file."
  :components ((:module basics :pathname "./"
                        :components ((:file "package")
  
                                     (:file "org-reader"))
                        :serial t)
               )
  :properties ((version "0.1"))
  :depends-on (:named-readtables))
