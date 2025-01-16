
(ql:quickload "cl-csv")

(ql:quickload "cl-yesql")
(ql:quickload "cl-yesql/postmodern")

(use-package :postmodern)

;;=> ("Book Id" "Title" "Author" "Author l-f" "Additional Authors" "ISBN"
;;    "ISBN13" "My Rating" "Average Rating" "Publisher" "Binding"
;;    "Number of Pages" "Year Published" "Original Publication Year" "Date Read"
;;    "Date Added" "Bookshelves" "Bookshelves with positions" "Exclusive Shelf"
;;    "My Review" "Spoiler" "Private Notes" "Read Count" "Owned Copies")
(defstruct (goodread
            (:constructor new-goodread
                (id
                 grtitle
                 author author-lf add-authors
                 isbn isbn13
                 rating avg-rating
                 publisher
                 binding
                 pages
                 year-published org-pub-year
                 date-read date-add
                 shelves shelves-with-pos exclusive-shelf
                 review
                 spoiler
                 notes
                 read-count
                 owned)))
  id
  grtitle
  author author-lf add-authors
  isbn isbn13
  rating avg-rating
  publisher
  binding
  pages
  year-published org-pub-year
  date-read date-add
  shelves shelves-with-pos exclusive-shelf
  review
  spoiler
  notes
  read-count
  owned)

(defun goodread-author-first (book)
  (first (uiop:split-string (goodread-author book) :separator " ")))

(defun goodread-author-middle (book)
  (let ((names (uiop:split-string (goodread-author book) :separator " ")))
    (if (= 3 (length names))
        (second names)
        "")))

(defun goodread-author-last (book)
  (first (last (uiop:split-string (goodread-author book) :separator " "))))

;; DEBUG need to remove series titles
(defun goodread-title (book)
  (let* ((fulltitle (goodread-grtitle book))
         (pos (search ": " fulltitle)))
    (if (null pos)
        fulltitle
        (subseq fulltitle 0 pos))))

(defun goodread-subtitle (book)
  (let* ((fulltitle (goodread-grtitle book))
         (pos (search ": " fulltitle)))
    (if (null pos)
        ""
        (subseq fulltitle (+ pos 2)))))

(defun goodread-series-title (book)
  "") ;; DEBUG finish this

(defparameter book nil)


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
  (let ((publication (or (publication-by-title :title (goodread-title book) :subtitle (goodread-subtitle book))
                         (add-publication :title (goodread-title book)
                                          :subtitle (goodread-subtitle book)
                                          :private nil
                                          :user user-id))))
    (first (first publication))))

(defun get-or-insert-creator (book user-id)
  (let ((creator (or (creator-by-name :first (goodread-author-first book)
                                      :middle (goodread-author-middle book)
                                      :last (goodread-author-last book))
                     (add-creator :first (goodread-author-first book)
                                  :middle (goodread-author-middle book)
                                  :last (goodread-author-last book)
                                  :private nil
                                  :user user-id))))
    (first (first creator))))

(goodread-grtitle book)

(with-connection '("library_cl" "brking" "" "/var/run/postgresql/" :pooled-p t)
  (let ((user-id (first (first (library-user :email "bruce@minuteproductions.com"))))
        (books (mapcar (lambda (csv) (apply 'new-goodread csv))
                       (rest (cl-csv:read-csv #P"/home/brking/Downloads/goodreads_library_export.csv")))))
    (loop for book being the elements of books do
      (get-or-insert-publication book user-id))))
