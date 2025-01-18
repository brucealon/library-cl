
(ql:quickload "cl-csv")

(load "goodreads-models.lisp")
(load "library-database.lisp")
(load "/home/brking/.library-config.lisp")

;; NOTE: make sure the user is created in a new database before running this.
(with-connection (list *postgres-db* *postgres-user* *postgres-pass* *postgres-host* :pooled-p t)
  (let ((user-id (get-or-insert-user *user-email* *user-first* *user-last* *user-display* *user-admin*))
        (books (read-goodreads-csv *goodreads-export*)))
    (loop for book being the elements of books do
      (let ((creator-id (get-or-insert-creator book user-id))
            (publication-id (get-or-insert-publication book user-id)))
        (link-series book publication-id user-id)
        (link-creator creator-id publication-id user-id)))))
