(in-package #:ccltest2)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

;; attempt #1:
;; ========================================
(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

(defmacro check (form)
  `(report-result ,form ',form))

;; attempt #2:
;; ========================================
(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 5 9) 14)))


;; combining results
;; ========================================
;; (combine-results
;;   (foo)
;;   (bar)
;;   (baz))

;; should translate to

;; (let ((result t))
;;   (unless (foo) (setf result nil))
;;   (unless (bar) (setf result nil))
;;   (unless (baz) (setf result nil))
;;   result)

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
    ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defmacro check (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))


;; naming tests
;; ========================================
(defparameter *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-* ()
  (check
    (= (* 1 1) 1)
    (= (* 5 2) 10)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
