;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((toc-org-max-depth . 3)
         (org-link-file-path-type . relative)
         (dired-omit-extensions . (".bbl" ".toc" ".fdb_latexmk" ".aux" ".fls" ".out" ".log" ".tex"))
         (eval . (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))))
 (dired-mode . ((dired-omit-extensions . (".fasl" ".bbl" ".toc" ".fdb_latexmk" ".aux" ".fls" ".out" ".tex"))
                (dired-omit-files . ("*.tex")))))
