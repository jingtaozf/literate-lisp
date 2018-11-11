;;; -*- encoding:utf-8 Mode: LISP; Syntax: COMMON-LISP; Base: 10  -*- ---
;; 
;; Filename: org-reader.lisp
;; Description: load source codes in a org file(literate programming).
;; Author: Jingtao Xu <jingtaozf@gmail.com>
;; Created: 2018.11.08 20:23:27(+0800)
;; Last-Updated: 2018.11.11 11:07:16(+0800)
;;     Update #: 45
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; The implementation is referenced from https://github.com/xtaniguchimasaya/papyrus
;; 
(in-package :org-reader)

(defvar old-plus-sign-reader (get-dispatch-macro-character #\# #\+))
(defvar debug-org-reader-p nil)
(declaim (type boolean debug-org-reader-p))
(defun org-reader-number-sign+space (stream a b)
  "ignore all lines after `# ' and before `#+BEGIN_SRC lisp'"
  (declare (ignore a b))
  (loop for line = (read-line stream nil nil) then (read-line stream nil nil)
        until (null line)
        for start1 = (loop for c of-type character across line
                           for i of-type fixnum from 0
                           until (not (find c '(#\Tab #\Space)))
                           finally (return i))
        do (when debug-org-reader-p
             (format t "ignore line ~a~%" line))
        until (equalp start1 (search "#+BEGIN_SRC lisp" line)))
  (values))

;;; like (MEMBER ITEM LIST :TEST #'EQ), this is from the source code of SBCL.
(defun memq (item list)
  "Return tail of LIST beginning with first element EQ to ITEM."
  ;; KLUDGE: These could be and probably should be defined as
  ;;   (MEMBER ITEM LIST :TEST #'EQ)),
  ;; but when I try to cross-compile that, I get an error from
  ;; LTN-ANALYZE-KNOWN-CALL, "Recursive known function definition". The
  ;; comments for that error say it "is probably a botched interpreter stub".
  ;; Rather than try to figure that out, I just rewrote this function from
  ;; scratch. -- WHN 19990512
  (do ((i list (cdr i)))
      ((null i))
    (when (eq (car i) item)
      (return i))))

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
    (symbol (not (null (memq x *features*))))
    (t
      (error "invalid feature expression: ~S" x))))

(defun org-sharp-plus-minus (stream sub-char numarg)
  (if (char= #\+ sub-char)
    (let ((feature (let ((*package* #.(find-package :keyword))
                         ;;(*reader-package* nil)
                         (*read-suppress* nil))
                     (read stream t nil t))))
      (cond ((eq :END_SRC feature)
             (when debug-org-reader-p
               (format t "found #+END_SRC,start read org part...~%"))
             (funcall #'org-reader-number-sign+space stream sub-char numarg))
            ((featurep feature)
             (read stream t nil t))
            (t
             (let ((*read-suppress* t))
               (read stream t nil t)
               (values)))))
    (funcall old-plus-sign-reader stream sub-char numarg)))

(defreadtable :org
  (:merge :standard)
  (:dispatch-macro-char #\# #\Space #'org-reader-number-sign+space)
  (:dispatch-macro-char #\# #\+ #'org-sharp-plus-minus))

(defmethod asdf:perform :around (o c)
  "after you load this package, then all org file will be supported to be loaded by asd automatically."
  (let ((*readtable* (ensure-readtable ':org)))
    (when (find-package :swank)
      (editor-hints.named-readtables::%frob-swank-readtable-alist *package* *readtable*))
    (call-next-method)))
