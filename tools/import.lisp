
(load (merge-pathnames ".library-config.lisp" (user-homedir-pathname)))
(load (merge-pathnames "tools/goodreads-import.lisp" *project-dir*))
(load (merge-pathnames "tools/wikindx-import.lisp" *project-dir*))

(if (and (boundp '*goodreads-export-file*) (uiop:file-pathname-p *goodreads-export-file*))
    (import-books (read-goodreads-csv *goodreads-export-file*)))

(if (boundp '*mysql-db*)
    (import-books (read-wikindx-db)))
