;;;; package.lisp

(defpackage #:ccltest2
  (:use #:cl
        #:usocket
        #:bordeaux-threads
        #:log4cl
        #:local-time
        #:cl-fad))

;; (defpackage #:ccltest2.concurrent
;;   (:use #:cl
;;         #:bordeaux-threads))
