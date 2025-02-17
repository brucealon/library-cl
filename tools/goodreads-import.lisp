
(ql:quickload "cl-csv")

(load (merge-pathnames ".library-config.lisp" (user-homedir-pathname)))
(load (merge-pathnames "tools/goodreads-models.lisp" *project-dir*))
(load (merge-pathnames "tools/library-database.lisp" *project-dir*))

(defun read-goodreads-csv (csv-file)
  (mapcar (lambda (csv) (funcall 'new-goodreads csv))
                       (rest (cl-csv:read-csv csv-file))))
