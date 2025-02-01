
(load "library-models.lisp")

(defclass wikindx-book ()
  (data))

(defun new-wikindx (db-row)
  (let ((instance (make-instance 'wikindx-book)))
    (setf (slot-value instance 'data) db-row)
    instance))

(defmethod book-creator-first (wikindx-book)
  (nth 12 (slot-value wikindx-book 'data)))

(defmethod book-creator-last (wikindx-book)
  (nth 14 (slot-value wikindx-book 'data)))

(defmethod book-creator-middle (wikindx-book)
  (nth 13 (slot-value wikindx-book 'data)))

(defmethod book-creator-role (wikindx-book)
  (let ((role-id (nth 15 (slot-value wikindx-book 'data))))
    (case role-id
      (2 "Editor")
      (3 "Translator")
      (t "Author"))))

(defmethod book-date-read (wikindx-book)
  nil)

(defmethod book-isbn (wikindx-book)
  (nth 9 (slot-value wikindx-book 'data)))

(defmethod book-pages (wikindx-book)
  (let ((pages (nth 8 (slot-value wikindx-book 'data))))
    (if (typep pages 'integer)
        pages
        0)))

(defmethod book-quote-comment (wikindx-book)
  (nth 19 (slot-value wikindx-book 'data)))

(defmethod book-quote-end (wikindx-book)
  (nth 17 (slot-value wikindx-book 'data)))

(defmethod book-quote-start (wikindx-book)
  (nth 16 (slot-value wikindx-book 'data)))

(defmethod book-quote-text (wikindx-book)
  (format t "Quote: ~a~%" (nth 18 (slot-value wikindx-book 'data))) ;; DEBUG
  (nth 18 (slot-value wikindx-book 'data)))

(defmethod book-rating (wikindx-book)
  nil)

(defmethod book-read (wikindx-book)
  nil)

(defmethod book-series (wikindx-book)
  (if (equal "book" (nth 2 (slot-value wikindx-book 'data)))
      (list (list (nth 5 (slot-value wikindx-book 'data))
                  (nth 7 (slot-value wikindx-book 'data))))))

(defmethod book-subtitle (wikindx-book)
  (nth 4 (slot-value wikindx-book 'data)))

(defmethod book-title (wikindx-book)
  (nth 3 (slot-value wikindx-book 'data)))
