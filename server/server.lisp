
(ql:quickload :hunchentoot)
(ql:quickload :easy-routes)

(defvar *server*)
(defvar *server-port* 4242)

(easy-routes:defroute root ("/") ()
  "hello app")

(easy-routes:defroute publications ("/publications/:publication") ()
  (format nil "Showing publication ~a" publication))

(setq *server* (make-instance 'easy-routes:easy-routes-acceptor :port *server-port*))
