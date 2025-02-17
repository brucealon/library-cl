
(ql:quickload :hunchentoot)
(ql:quickload :easy-routes)

(load (merge-pathnames ".library-config.lisp" (user-homedir-pathname)))
(load (merge-pathnames "libs/library-database.lisp" *project-dir*))

(defparameter *server* nil)
(defparameter *server-port* 4242)

(easy-routes:defroute root ("/") ()
  "hello app")

(easy-routes:defroute route-publications ("/publications") ()
  (with-connection (list *postgres-db* *postgres-user* *postgres-pass* *postgres-host* :pooled-p t)
    (with-output-to-string (out)
      (loop for publication being the elements of (all-publications) do
        (format out "Publication: ~a<br/>~%" (second publication)))
      out)))

(easy-routes:defroute route-creators ("/creators") ()
  (with-connection (list *postgres-db* *postgres-user* *postgres-pass* *postgres-host* :pooled-p t)
    (with-output-to-string (out)
      (loop for creator being the elements of (all-creators) do
        (format out "Creator: ~a ~a<br/>~%" (second creator) (third creator)))
      out)))

(if (not (null *server*))
    (format nil "Need to stop the current server first?")
    (setf *server* (make-instance 'easy-routes:easy-routes-acceptor :port *server-port*)))
