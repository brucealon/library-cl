
(load (merge-pathnames ".library-config.lisp" (user-homedir-pathname)))
(load (merge-pathnames "libs/library-models.lisp" *project-dir*))

(defclass wikindx-book ()
  (data))

(defun new-wikindx (db-row)
  (let ((instance (make-instance 'wikindx-book)))
    (setf (slot-value instance 'data) db-row)
    instance))

(defmethod book-creator-first ((book wikindx-book))
  (nth 12 (slot-value book 'data)))

(defmethod book-creator-last ((book wikindx-book))
  (nth 14 (slot-value book 'data)))

(defmethod book-creator-middle ((book wikindx-book))
  (nth 13 (slot-value book 'data)))

(defmethod book-creator-role ((book wikindx-book))
  (let ((role-id (nth 15 (slot-value book 'data))))
    (case role-id
      (2 "Editor")
      (3 "Translator")
      (t "Author"))))

(defmethod book-date-read ((book wikindx-book))
  nil)

(defmethod book-isbn ((book wikindx-book))
  (nth 9 (slot-value book 'data)))

(defmethod book-pages ((book wikindx-book))
  (let ((pages (nth 8 (slot-value book 'data))))
    (if (typep pages 'integer)
        pages
        0)))

(defmethod book-quote-comment ((book wikindx-book))
  (nth 19 (slot-value book 'data)))

(defmethod book-quote-end ((book wikindx-book))
  (nth 17 (slot-value book 'data)))

(defmethod book-quote-start ((book wikindx-book))
  (nth 16 (slot-value book 'data)))

(defmethod book-quote-text ((book wikindx-book))
  (nth 18 (slot-value book 'data)))

(defmethod book-rating ((book wikindx-book))
  nil)

(defmethod book-read ((book wikindx-book))
  nil)

(defmethod book-series ((book wikindx-book))
  (if (equal "book" (nth 2 (slot-value book 'data)))
      (list (list (nth 5 (slot-value book 'data))
                  (nth 7 (slot-value book 'data))))))

(defmethod book-subtitle ((book wikindx-book))
  (nth 4 (slot-value book 'data)))

(defmethod book-title ((book wikindx-book))
  (nth 3 (slot-value book 'data)))
