(in-package :asdf-user)

(defsystem "ascii-game-of-life"
  :description "Reddit challenge"
  :version "1.0.0"
  :author "Florian Patzl"
  :licence "Public Domain"
  :depends-on (:split-sequence :lisp-unit :cl-glut)
  :components ((:file "main"))
  :entry-point "com.github.flpa.daily-reddit.ascii-game-of-life:play-graphic-challenge")
	       
