 ;;;; ccltest2.asd

(asdf:defsystem #:ccltest2
  :serial t
  :description "Describe ccltest2 here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:bordeaux-threads
               #:usocket
               #:log4cl)
  :components ((:file "package")
               (:file "ccltest2")
               (:file "intro")
               (:file "basic-collections")
               (:file "unit-test")
               (:file "clos")))
