
(load (merge-pathnames ".library-config.lisp" (user-homedir-pathname)))

(ql:quickload "cl-yesql")
(ql:quickload "cl-yesql/postmodern")

(use-package :postmodern)

(overlord:set-package-base *project-dir*)
(yesql:import my-queries
  :from "sql/queries.pgsql"
  :as :cl-yesql/postmodern
  :binding :all-functions)

(defun get-or-insert-user (email first-name last-name display-name admin)
  (let ((users (or (user-id :email email)
                   (progn
                     (format t "Adding user ~a~%" display-name)
                     (add-user :email email
                               :first first-name
                               :last last-name
                               :display display-name
                               :admin admin)))))
    (first (first users))))

(defun get-or-insert-publication (book user-id inserter-id)
  (let ((publications (or (publication-by-title :title (book-title book) :subtitle (book-subtitle book))
                          (progn
                            (format t "Adding book ~a to database.~%" (book-title book))
                            (add-publication :title (book-title book)
                                             :subtitle (book-subtitle book)
                                             :private nil
                                             :inserter inserter-id)))))
    (first (first publications))))

(defun get-or-insert-creator (book user-id inserter-id)
  (let ((creators (or (creator-by-name :first (book-creator-first book)
                                       :middle (book-creator-middle book)
                                       :last (book-creator-last book))
                      (progn
                        (format t "Adding creator ~a to database.~%" (book-creator-first book))
                        (add-creator :first (book-creator-first book)
                                     :middle (book-creator-middle book)
                                     :last (book-creator-last book)
                                     :private nil
                                     :inserter inserter-id)))))
    (first (first creators))))

(defun get-or-insert-creator-role (book inserter-id)
  (let* ((role-name (book-creator-role book))
         (roles (or (creator-role-by-name :name role-name)
                    (add-creator-role :name role-name :inserter inserter-id))))
    (first (first roles))))

(defun get-or-insert-series (book series-title user-id inserter-id)
  (let ((series (or (series-id-by-title :title series-title)
                    (progn
                      (format t "Adding series ~a to database.~%" series-title)
                      (add-series :title series-title :private nil :inserter inserter-id)))))
    (first (first series))))

(defun get-or-link-series-entry (publication-id series-id series-number user-id inserter-id)
  (let ((series-entries (or (series-entry :series series-id :publication publication-id)
                            (add-series-entry :series series-id
                                              :publication publication-id
                                              :number series-number
                                              :private nil
                                              :inserter inserter-id))))
    (first (first series-entries))))

(defun link-series (book publication-id user-id inserter-id)
  (loop for series being the elements of (book-series book) do
    (let* ((series-title (first series))
           (series-number (second series)))
      (get-or-link-series-entry publication-id
                                (get-or-insert-series book series-title user-id inserter-id)
                                series-number
                                user-id
                                inserter-id))))

(defun get-or-insert-edition (book publication-id user-id inserter-id)
  (let ((editions (or (edition-by-publication :publication publication-id)
                      (add-edition :publication publication-id
                                   :pages (book-pages book)
                                   :isbn (book-isbn book)
                                   :private nil
                                   :inserter inserter-id))))
    (first (first editions))))

(defun get-or-insert-edition-creator (book edition-id creator-id user-id inserter-id)
  (let ((editions (or (edition-creator-by-id :edition edition-id)
                      (add-edition-creator :edition edition-id
                                           :creator creator-id
                                           :role (get-or-insert-creator-role book inserter-id)
                                           :private nil
                                           :inserter inserter-id))))
    (first (first editions))))

(defun get-or-insert-read (book edition-id user-id inserter-id)
  (let ((reads (or (user-publication-edition-read :edition edition-id :user user-id)
                   (progn
                     (format t "Setting read date for ~a to '~a'~%" (book-title book) (book-date-read book))
                     (add-user-publication-edition-read :edition edition-id
                                                        :user user-id
                                                        :read (book-date-read book)
                                                        :finished (book-read book)
                                                        :private nil
                                                        :inserter inserter-id)))))
    (first (first reads))))

(defun get-or-insert-review (book user-edition-read-id user-id inserter-id)
  (let ((reviews (or (user-publication-review :read user-edition-read-id)
                     (add-user-publication-review :read user-edition-read-id
                                                  :rating (book-rating book)
                                                  :review ""
                                                  :private nil
                                                  :inserter inserter-id))))
    (first (first reviews))))

(defun get-or-insert-quote (book edition-id user-id inserter-id)
  (let ((quotes (or (publication-edition-quote :edition edition-id :user user-id :page (book-quote-start book))
                    (progn
                      (format t "Inserting quote for ~a on page ~a~%" (book-title book) (book-quote-start book))
                      (add-publication-edition-quote :edition edition-id
                                                     :quote (book-quote-text book)
                                                     :page (book-quote-start book)
                                                     :user user-id
                                                     :private nil
                                                     :inserter inserter-id)))))
    (first (first quotes))))

(defun get-or-insert-quote-comment (book quote-id user-id inserter-id)
  (let ((comments (or (user-quote-comment :quote quote-id)
                      (progn
                        (format t "Inserting comment for quote ~a~%" quote-id)
                        (add-user-quote-comment :quote quote-id
                                                :comment (book-quote-comment book)
                                                :private nil
                                                :inserter inserter-id)))))
    (first (first comments))))

(defun import-book (book user-id inserter-id)
  (let* ((creator-id (get-or-insert-creator book user-id inserter-id))
         (publication-id (get-or-insert-publication book user-id inserter-id))
         (edition-id (get-or-insert-edition book publication-id user-id inserter-id)))
    (link-series book publication-id user-id inserter-id)
    (get-or-insert-edition-creator book edition-id creator-id user-id inserter-id)
    (and (> (length (book-date-read book)) 0)
         (let ((read-id (get-or-insert-read book edition-id user-id inserter-id)))
           (get-or-insert-review book read-id user-id inserter-id)))
    (if (book-quote-text book)
        (let ((quote-id (get-or-insert-quote book edition-id user-id inserter-id)))
          (if (book-quote-comment book)
              (progn
                (format t "Quote Comment ~a~%" (book-quote-comment book))
                (get-or-insert-quote-comment book quote-id user-id inserter-id)))))))

(defun import-books (books)
  (with-connection (list *postgres-db* *postgres-user* *postgres-pass* *postgres-host* :pooled-p t)
    (let ((user-id (get-or-insert-user *user-email* *user-first* *user-last* *user-display* *user-admin*))
          (inserter-id (get-or-insert-user *inserter-email* *inserter-first* *inserter-last* *inserter-display* *inserter-admin*)))
      (loop for book being the elements of books do
        (import-book book user-id inserter-id)))))
