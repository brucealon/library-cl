
(ql:quickload "cl-csv")
(ql:quickload "cl-ppcre")

(ql:quickload "cl-yesql")
(ql:quickload "cl-yesql/postmodern")

(use-package :postmodern)

(load "goodreads-models.lisp")

;; look for creator
;;   add if not present
;; look for publication
;;   add if not present
;; link publication to publication edition
;; link publication edition to creator

(yesql:import my-queries
  :from "/home/brking/Repos/library-cl/sql/queries.sql"
  :as :cl-yesql/postmodern
  :binding :all-functions)

(defun get-or-insert-publication (book user-id)
  (let ((publications (or (publication-by-title :title (goodreads-title book) :subtitle (goodreads-subtitle book))
                          (add-publication :title (goodreads-title book)
                                           :subtitle (goodreads-subtitle book)
                                           :private nil
                                           :user user-id))))
    (first (first publications))))

(defun get-or-insert-creator (book user-id)
  (let ((creators (or (creator-by-name :first (goodreads-author-first book)
                                       :middle (goodreads-author-middle book)
                                       :last (goodreads-author-last book))
                      (add-creator :first (goodreads-author-first book)
                                   :middle (goodreads-author-middle book)
                                   :last (goodreads-author-last book)
                                   :private nil
                                   :user user-id))))
    (first (first creators))))

(defun get-or-insert-series (book series-title user-id)
  (let ((series (or (series-id-by-title :title series-title)
                    (add-series :title series-title :private nil :user user-id))))
    (first (first series))))

(defun get-or-link-series-entry (publication-id series-id series-number user-id)
  (let ((series-entries (or (series-entry :series series-id :publication publication-id)
                            (add-series-entry :series series-id
                                              :publication publication-id
                                              :number series-number
                                              :private nil
                                              :user user-id))))
    (first (first series-entries))))

(defun link-series (book publication-id user-id)
  (loop for series being the elements of (goodreads-series book) do
    (let* ((series-title (first series))
           (series-number (second series)))
      (get-or-link-series-entry publication-id
                                (get-or-insert-series book series-title user-id)
                                series-number
                                user-id))))

(defun get-or-insert-edition (publication-id user-id)
  (let ((editions (or (edition-by-publication :publication publication-id)
                      (add-edition :publication publication-id
                                   :private nil
                                   :user user-id))))
    (first (first editions))))

(defun get-or-insert-edition-creator (edition-id creator-id user-id)
  (let ((editions (or (edition-creator-by-id :edition edition-id)
                      (add-edition-creator :edition edition-id
                                           :creator creator-id
                                           :private nil
                                           :user user-id))))
    (first (first editions))))

(defun link-creator (creator-id publication-id user-id)
  ;; publication_editions
  ;; publication_edition_creators
  (let ((edition-id (get-or-insert-edition publication-id user-id)))
    (get-or-insert-edition-creator edition-id creator-id user-id)))

(defparameter *postgres-user* "brking")
(defparameter *postgres-pass* "")
(defparameter *postgres-host* "/var/run/postgresql/")
(defparameter *postgres-db* "library_cl")

(defparameter *user-email* "bruce@minuteproductions.com")
(defparameter *goodreads-export* #P"/home/brking/Downloads/goodreads_library_export.csv")

;; (defparameter book nil) (setf book (nth 4 books)) will set the book to a series book (Ancillary Justice)
;; (defparameter book nil) (setf book (nth 5 books)) will set the book to a non-series book (American Fascists)
;; NOTE: make sure the user is created in a new database before running this.
(with-connection (list *postgres-db* *postgres-user* *postgres-pass* *postgres-host* :pooled-p t)
  (let ((user-id (user-id :email *user-email*))
        (books (mapcar (lambda (csv) (apply 'new-goodreads csv))
                       (rest (cl-csv:read-csv *goodreads-export*)))))
    (loop for book being the elements of books do
      (let ((creator-id (get-or-insert-creator book user-id))
            (publication-id (get-or-insert-publication book user-id)))
        (link-series book publication-id user-id)
        (link-creator creator-id publication-id user-id)))))
