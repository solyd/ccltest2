(in-package #:ccltest2)

(defparameter *mylist* (list 1 2 3))
(defun hello-world (name)
  (format t "hello, ~a~%" name))



  ;; (with-open-file (fstream filepath
  ;;                          :direction :output
  ;;                          :if-exists :append
  ;;                          :if-does-not-exist :create))
