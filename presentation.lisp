(in-package #:ccltest2)

;; writing my own while
;; sensible syntax is (my-while test body)

(defmacro my-while (test &body body)
  (list* 'loop (list 'unless test '(return nil))
         body))


;; functions and symbols are seperated
;; so if i define function foo, to get the function object i need
;; to use "function" or "#'"
