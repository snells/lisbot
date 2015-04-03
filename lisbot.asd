;;;; lisbot.asd

(asdf:defsystem #:lisbot
  :serial t
  :version "0.0.0"
  :description "Download packages from xdcc bots"
  :author "Sakari Snäll"
  :license ""
  :depends-on (#:cl-fad #:cl-ppcre #:usocket #:bordeaux-threads #:date-calc #:getopt)
  :components ((:file "package")
	       (:file "parse")
               (:file "lisbot")))

