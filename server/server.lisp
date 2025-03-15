
(ql:quickload :hunchentoot)
(ql:quickload :easy-routes)
(ql:quickload :djula)

(load (merge-pathnames ".library-config.lisp" (user-homedir-pathname)))
(load (merge-pathnames "libs/library-database.lisp" *project-dir*))

(defparameter *server* nil)
(defparameter *server-port* 4242)

(djula:add-template-directory (merge-pathnames "html/" *project-dir*))

(easy-routes:defroute root ("/") ()
  (djula:render-template* (djula:compile-template* "library.html")))

(easy-routes:defroute route-publications ("/publications") ()
  (with-library-db
    (djula:render-template* (djula:compile-template* "publications.html")
                            nil
                            :publications (all-publications))))

(easy-routes:defroute route-publication ("/publications/:publication") ()
  (with-library-db
    (djula:render-template* (djula:compile-template* "publication.html")
                            nil
                            :publication (first (publication-by-id :id publication)))))

(easy-routes:defroute route-creators ("/creators") ()
  (with-library-db
    (djula:render-template* (djula:compile-template* "creators.html")
                            nil
                            :creators (all-creators))))

(easy-routes:defroute route-creator ("/creators/:creator") ()
  (with-library-db
    (djula:render-template* (djula:compile-template* "creator.html")
                            nil
                            :creator (first (creator-by-id :id creator)))))

(if (not (null *server*))
    (format nil "Need to stop the current server first?")
    (setf *server* (make-instance 'easy-routes:easy-routes-acceptor :port *server-port*)))
