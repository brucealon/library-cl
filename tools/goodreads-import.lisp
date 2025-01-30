
(ql:quickload "cl-csv")

(load "/home/brking/.library-config.lisp")
(load "goodreads-models.lisp")
(load "library-database.lisp")

(with-connection (list *postgres-db* *postgres-user* *postgres-pass* *postgres-host* :pooled-p t)
  (let ((user-id (get-or-insert-user *user-email* *user-first* *user-last* *user-display* *user-admin*))
        (books (read-goodreads-csv *goodreads-export*)))
    (loop for book being the elements of books do
      (let* ((creator-id (get-or-insert-creator book user-id))
             (publication-id (get-or-insert-publication book user-id))
             (edition-id (get-or-insert-edition book publication-id user-id)))
        (link-series book publication-id user-id)
        (get-or-insert-edition-creator book edition-id creator-id user-id)
        (and (> (length (book-date-read book)) 0)
             (let ((read-id (get-or-insert-read book edition-id user-id)))
               (get-or-insert-review book read-id user-id)))))))

