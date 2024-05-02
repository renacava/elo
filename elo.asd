;;;; elo.asd

(asdf:defsystem #:elo
  :description ""
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop)
  :components ((:file "package")
               (:file "utilities")
               (:file "main")))
