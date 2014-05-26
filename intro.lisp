(in-package #:ccltest2)

;; hello world
(format t "hello, world")

;; function definition
(defun hello-world (name)
  (format t "hello, ~a" name))


(defmacro alias-this (this &body body)
  `(let ((this ,this))
     ,@body))

(defmacro random-if (threshhold-probability true-block false-block)
  `(if (> (random 1.0) ,threshhold-probability)
       ,(progn true-block)
       ,(progn false-block)))

;;The other important feature of the where macro is the use of &rest
;;in the argument list. Like &key, &rest modifies the way arguments
;;are parsed. With a &rest in its parameter list, a function or macro
;;can take an arbitrary number of arguments, which are collected into
;;a single list that becomes the value of the variable whose name
;;follows the &rest.


(defun foo (x)
  (let ((mvar 10))
    (break)
    (format t "~a ~a~%" x mvar)))
