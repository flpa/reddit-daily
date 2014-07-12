(in-package :cl-user)

(defpackage :com.github.flpa.daily-reddit.html-markup-generator
  (:use :cl :monkeylib-html :trivial-shell))

(in-package :com.github.flpa.daily-reddit.html-markup-generator)

(defun generate-page ()
  (format t "Please type your paragraph~%")
  (let ((parag (read-line)))
    (with-open-file
	(s "/tmp/generated.html"
	   :direction :output 
	   :if-exists :overwrite
	   :if-does-not-exist :create)
      (with-html-output (s)
	(html 
	  (:head (:title "Generated Page"))
	  (:body (:p parag)))))
    (shell-command "firefox /tmp/generated.html")))
