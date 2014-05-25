(in-package #:ccltest2)

;; vectors
;; ========================================

;; fixed size vectors
(defparameter *vec1* (vector 1 2))

;; make-array
;; creates a resizable vector.
;; resizable vector keeps track of:
;; + number of elements actually stored in the vector (fill-pointer,
;;	 index of next position to be filled when you add and element to
;;	 the vector)
;; + memory used to hold the elements
;; + number of slots available
(defparameter *arr0* (make-array 5 :initial-element nil))

;; makes a vector with room for five elements, but it looks empty
;; because the fill pointer is zero
(defparameter *arr1* (make-array 5 :fill-pointer 0))

;; create a vector with adjustable size
(defparameter *arr2* (make-array 5 :fill-pointer 0 :adjustable t))

;; vector-push will insert an element, never extending vector beyond limit
;; vector-push-extend will also adjust vector size if possible
;; vector-pop

;; :element-type

;; hash tables
;; ========================================
(defparameter *htable* (make-hash-table))
(defun set-key-val (htable key val)
  (setf (gethash key htable) val))

;; multiple-value-bind
;; maphash
