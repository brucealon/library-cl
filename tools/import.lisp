
(load "goodreads-import.lisp")
(load "wikindx-import.lisp")

(import-books (read-goodreads-csv *goodreads-export*))

(import-books (read-wikindx-db))
