;;;; nightmare.asd

(asdf:defsystem #:nightmare
  :description "Describe nightmare here"
  :author "Erik Edrosa <your.name@example.com>"
  :license "GPLv3"
  :depends-on (#:xelf)
  :serial t
  :components ((:file "package")
               (:file "nightmare")))

