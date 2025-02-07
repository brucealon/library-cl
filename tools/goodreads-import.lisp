
(ql:quickload "cl-csv")

(load "/home/brking/.library-config.lisp")
(load "goodreads-models.lisp")
(load "library-database.lisp")

(defun read-goodreads-csv (csv-file)
  (mapcar (lambda (csv) (funcall 'new-goodreads csv))
                       (rest (cl-csv:read-csv csv-file))))
