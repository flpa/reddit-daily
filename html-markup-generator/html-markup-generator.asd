(in-package :asdf-user)

(defsystem "html-markup-generator"
  :description "Reddit challenge"
  :version "1.0.0"
  :author "Florian Patzl"
  :licence "Public Domain"
  :depends-on (:monkeylib-html :trivial-shell)
  :components ((:file "main")))
	       
