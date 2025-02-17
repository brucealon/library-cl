
(ql:quickload "cl-ppcre")

(load (merge-pathnames ".library-config.lisp" (user-homedir-pathname)))
(load (merge-pathnames "libs/library-models.lisp" *project-dir*))

;; 00 "Book Id"
;; 01 "Title"
;; 02 "Author"
;; 03 "Author l-f"
;; 04 "Additional Authors"
;; 05 "ISBN"
;; 06 "ISBN13"
;; 07 "My Rating"
;; 08 "Average Rating"
;; 09 "Publisher"
;; 10 "Binding"
;; 11 "Number of Pages"
;; 12 "Year Published"
;; 13 "Original Publication Year"
;; 14 "Date Read"
;; 15 "Date Added"
;; 16 "Bookshelves"
;; 17 "Bookshelves with positions"
;; 18 "Exclusive Shelf"
;; 19 "My Review"
;; 20 "Spoiler"
;; 21 "Private Notes"
;; 22 "Read Count"
;; 23 "Owned Copies"

(defclass goodreads-book ()
  (data))

(defun new-goodreads (csv-row)
  (let ((instance (make-instance 'goodreads-book)))
    (setf (slot-value instance 'data) csv-row)
    instance))

(defmethod book-creator-first (goodreads-book)
  (first (uiop:split-string (nth 2 (slot-value goodreads-book 'data)) :separator " ")))

(defmethod book-creator-last (goodreads-book)
  (first (last (uiop:split-string (nth 2 (slot-value goodreads-book 'data)) :separator " "))))

(defmethod book-creator-middle (goodreads-book)
  (let ((names (uiop:split-string (nth 2 (slot-value goodreads-book 'data)) :separator " ")))
    (if (= 3 (length names))
        (second names)
        "")))

(defmethod book-creator-role (goodreads-book)
  "Author")

(defmethod book-date-read (goodreads-book)
  (nth 14 (slot-value goodreads-book 'data)))

(defmethod book-isbn (goodreads-book)
  (let* ((isbn (nth 6 (slot-value goodreads-book 'data)))
         (match (ppcre:all-matches "([0-9-]+)" isbn)))
    (and match (subseq isbn (car match) (cadr match)))))

(defmethod book-pages (goodreads-book)
  (let ((pages (nth 11 (slot-value goodreads-book 'data))))
    (if (typep pages 'integer)
        pages
        0)))

(defmethod book-quote-comment (goodreads-book)
  nil)

(defmethod book-quote-end (goodreads-book)
  nil)

(defmethod book-quote-start (goodreads-book)
  nil)

(defmethod book-quote-text (goodreads-book)
  nil)

(defmethod book-rating (goodreads-book)
  (nth 7 (slot-value goodreads-book 'data)))

(defmethod book-read (goodreads-book)
  (let* ((count (nth 22 (slot-value goodreads-book 'data)))
         (intcount (cond ((typep count 'integer) count)
                         ((typep count 'string) (parse-integer count))
                         (t 0))))
    (> intcount 0)))

(defmethod book-series (goodreads-book)
  (labels ((parse-one-series (series)
             (multiple-value-bind (match groups)
                 (ppcre:scan-to-strings "^([^,]+),? +#([\\d\.-]+)" series)
               (if match
                   (list (aref groups 0) (aref groups 1))
                   nil)))
           (parse-series (series)
             (if (null (search ";" series))
                 (if (null (search "#" series))
                     (list (list series))
                     (list (parse-one-series series)))
                 (mapcar (lambda (s) (parse-one-series s)) (ppcre:split "; " series)))))
    (let* ((fulltitle (nth 1 (slot-value goodreads-book 'data))))
      (multiple-value-bind (match groups)
          (ppcre:scan-to-strings "\\((.*)\\)" fulltitle)
        (if match
            (parse-series (aref groups 0))
            nil)))))

(defmethod book-subtitle (goodreads-book)
  (let* ((fulltitle (nth 1 (slot-value goodreads-book 'data)))
         (pos (search ": " fulltitle)))
    (if (null pos)
        ""
        (subseq fulltitle (+ pos 2)))))

(defmethod book-title (goodreads-book)
  (let* ((fulltitle (nth 1 (slot-value goodreads-book 'data)))
         (pos (search ": " fulltitle))
         (title (if (null pos)
                    fulltitle
                    (subseq fulltitle 0 pos))))
    (multiple-value-bind (match groups)
        (ppcre:scan-to-strings "^([^\\(]+) \\(" title)
      (if match
          (aref groups 0)
          title))))
