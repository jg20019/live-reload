;;;; live-reload.lisp

(in-package #:live-reload)

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (ql:quickload :djula))


(djula:add-template-directory (asdf:system-relative-pathname :live-reload #p"templates/"))

(defparameter +index.html+ (djula:compile-template* "index.html"))


(defmacro with-page ((&key title) &body body)
  "This macro generates html string using title and body 
   according to the syntax defined by spinneret.

   It includes client.js which is used to create a websocket
   connection to enable live reloads.
   "
  `(spinneret:with-html-string
     (:doctype)
     (:html
       (:head
         (:title ,title))
       (:body 
         ,@body
         (:script :type "text/javascript" :src "client.js")))))

(hunchentoot:define-easy-handler (ws-client-js :uri "/client.js") ()
  "Create websocket client"
  (setf (hunchentoot:content-type*) "text/javascript")
  (parenscript:ps 
    (let ((client (ps:new (-web-socket "ws://localhost:8000/conn"))))
      (setf (ps:getprop client 'onmessage) 
            (lambda ()
              (ps:chain location (reload)))))))


(defvar *count* 0 "Variable to see when page reloads")
(hunchentoot:define-easy-handler (index :uri "/") ()
  "A test HTML page"
  (with-page (:title "Index") 
    (:h1 "Hello World")
    (:p *count*)))

(hunchentoot:define-easy-handler (djula-index :uri "/djula") () 
  "A test HTML page using Djula"
  (render-live-reload +index.html+ nil :count *count*))

(defun render-live-reload (template &optional stream &rest template-arguments)
  (apply #'djula:render-template* template stream template-arguments))

;;;; Websockets

(defvar *connection* (make-instance 'hunchensocket:websocket-resource))

;; All requests return the same *connection*
(pushnew (lambda (request) *connection*) hunchensocket:*websocket-dispatch-table*)

(defun send-reload-message ()
  "Send reload message to all connected clients."
  (incf *count*)
  (dolist (peer (hunchensocket:clients *connection*))
    (hunchensocket:send-text-message peer "reload")))

(defvar *server* 
  (make-instance 'hunchentoot:easy-acceptor :port 4242) "HTTP Listener")

(defvar *ws-server* 
  (make-instance 'hunchensocket:websocket-acceptor :port 8000)
  "Web Socket Listener")

(defun start () 
    (hunchentoot:start *server*)
    (hunchentoot:start *ws-server*))
