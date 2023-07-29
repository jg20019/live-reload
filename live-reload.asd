;;;; live-reload.asd

(asdf:defsystem #:live-reload
  :description "Describe live-reload here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:hunchentoot :parenscript :spinneret :hunchensocket)
  :components ((:file "package")
               (:file "live-reload")))
