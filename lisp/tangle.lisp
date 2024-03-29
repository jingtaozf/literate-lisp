;;; This file is automatically generated from file `literate-lisp.org'.
;;; Please read file `literate-lisp.org' to find out the usage and implementation detail of this source file.

(in-package #:literate-lisp)

(defun tangle-org-file (org-file &key (features *features*)
                                   (header ";;; This file is automatically generated from file `~a.~a'.
;;; Please read file `~a.~a' to find out the usage and implementation detail of this source file.~%~%")
                                   (header-args (list (pathname-name org-file) (pathname-type org-file)
                                                      (pathname-name org-file) (pathname-type org-file)))
                                   (force-tangle nil)
                                   (output-name (format nil "~a.lisp" (pathname-name org-file))))
  (let ((*features* features)
        (*tangle-org-file* org-file)
        (*current-tangle-stream* nil)
        (*tangle-head-lines* (apply #'format nil header header-args))
        (*check-outside-modification-p* (not force-tangle))
        ;; reset org context
        (current-org-context (make-hash-table)))
    (setup-headline)
    (when output-name
      (setf *current-tangle-stream* (tangle-stream output-name)))
    (with-open-file (input org-file :direction :input
                                    :element-type uiop:*default-stream-element-type*
                                    :external-format uiop:*default-encoding*)
      (block read-org-files
        (iter
              ;; ignore all lines of org syntax.
              (sharp-org input)
              ;; start to read codes in code block until reach `#+END_SRC'
              (if (read-block-context-to-stream input *current-tangle-stream* "LISP" "#+END_SRC")
                  (write-line "" *current-tangle-stream*)
                  (return)))))
    (cleanup-tangle-streams)
    t))

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

