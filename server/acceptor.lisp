
(ql:quickload "hunchentoot")

(defvar *acceptor*)
(setq *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))

