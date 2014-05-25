(in-package #:ccltest2)

;; hello world
(format t "hello, world")

;; function definition
(defun hello-world (name)
  (format t "hello, ~a" name))

;; macros
(defmacro backwards (expr) (reverse expr))

(defclass A ()
  ((x :initarg :x
      :initform 0
      :accessor A-x)))

;;The other important feature of the where macro is the use of &rest
;;in the argument list. Like &key, &rest modifies the way arguments
;;are parsed. With a &rest in its parameter list, a function or macro
;;can take an arbitrary number of arguments, which are collected into
;;a single list that becomes the value of the variable whose name
;;follows the &rest.

;; Eqaulity
;; ========================================-

;;(eq x y) is true if and only if x and y are the same identical
;;object. (Implementationally, x and y are usually eq if and only if
;;they address the same identical memory location.)
(eq 'a 'a) ;; is true
(eq 3 3) ;; is implementation dependant

;;The eql predicate is true if its arguments are eq, or if they are
;;numbers of the same type with the same value, or if they are
;;character objects that represent the same character. For example:
(eql 'a 'b) ;; is false.
(eql 'a 'a) ;; is true.

;;The equal predicate is true if its arguments are structurally
;;similar (isomorphic) objects. A rough rule of thumb is that two
;;objects are equal if and only if their printed representations are
;;the same.

;;Two objects are equalp if they are equal; if they are characters and
;;satisfy char-equal, which ignores alphabetic case and certain other
;;attributes of characters; if they are numbers and have the same
;;numerical value, even if they are of different types; or if they
;;have components that are all equalp.


;; function and variable namespaces are seperate


;; declaring types,
(defun replace-char (str ch)
  (declare (type character ch)
           (type string str))
  ;...
  (format t "~a" str))

;; Simple variable:    (setf x 10)
;; Array:              (setf (aref a 0) 10)
;; Hash table:         (setf (gethash 'key hash) 10)
;; Slot named 'field': (setf (field o) 10)
