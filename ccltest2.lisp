(in-package #:ccltest2)

(defclass A ()
  ((x :initarg :x
      :initform 0
      :accessor A-x)))

(defclass A1 (A)
  ((y :initarg :y
      :initform 0
      :accessor A1-y)))

(defmethod initialize-instance :after ((a A) &key)
  (log:info ""))

(defmethod initialize-instance :after ((a1 A1) &key)
  (log:info ""))

(defgeneric foo-a (a))

(defmethod foo-a :before ((a A))
  (log:info "A before"))

(defmethod foo-a :around ((a A))
  (log:info "A around")
  (call-next-method))

(defmethod foo-a ((a A))
  (log:info "A"))

(defmethod foo-a :after ((a A))
  (log:info "A after"))

(defmethod foo-a :before ((a1 A1))
  (log:info "A1 before"))

(defmethod foo-a :around ((a1 A1))
  (log:info "A1 around")
  (call-next-method))

(defmethod foo-a ((a1 A1))
  (log:info "A1"))

(defmethod foo-a :after ((a1 A1))
  (log:info "A1 after"))

;; Generic interface for collections
;; ---------------------------------
(defgeneric size (coll))

(defgeneric insert-first (coll item))
(defgeneric insert-last (coll item))

(defgeneric get-nth (coll n))
(defgeneric get-first (coll))
(defgeneric get-last (coll))
(defgeneric items (coll))

(defgeneric remove-first (coll))
(defgeneric remove-last (coll))
(defgeneric remove-nth (coll n))

;; --------------------------------------------------------------------------------
;; List
;; --------------------------------------------------------------------------------
(defclass ListNode ()
  ((next :initform nil :initarg :next :accessor listnode-next)
   (prev :initform nil :initarg :prev :accessor listnode-prev)
   (data :initform nil :initarg :data :accessor listnode-data)))

(defmethod listnode-order-nodes ((before-node ListNode) (after-node ListNode))
  (setf (listnode-next before-node) after-node
        (listnode-prev after-node) before-node))

(defmethod listnode-order-nodes ((before-node ListNode) (after-node (eql nil)))
  (setf (listnode-next before-node) nil))

(defmethod listnode-order-nodes ((before-node (eql nil)) (after-node ListNode))
  (setf (listnode-prev after-node) nil))

(defmethod listnode-order-nodes ((before-node (eql nil)) (after-node (eql nil))))

(defmethod listnode-link-before ((new-node ListNode) (node ListNode))
  (listnode-order-nodes new-node node))

(defmethod listnode-link-after ((new-node ListNode) (node ListNode))
  (listnode-order-nodes node new-node))

(defmethod listnode-unlink-node ((node ListNode))
  (let ((prev-node (listnode-prev node))
        (next-node (listnode-next node)))
    (listnode-order-nodes prev-node next-node)
    (setf prev-node nil
          next-node nil)))

(defclass AlxList ()
  ((first-node :initform nil)
   (last-node :initform nil)
   (length :initform 0 :reader alxlist-length)))

(defun AlxList->list (alxlist)
  (with-slots (first-node) alxlist
    (loop for curr-node = first-node then (listnode-next curr-node) while curr-node
       collect (listnode-data curr-node))))

(defun list->AlxList (list)
  (let ((alxlist (make-instance 'AlxList)))
    (loop for item in list do
         (insert-last alxlist item))
    alxlist))

(defmethod print-object ((list AlxList) (stream stream))
  (print-object (AlxList->list list) stream))

(defmethod size ((list AlxList))
  (alxlist-length list))

(defmethod insert-first ((list AlxList) (item t))
  (with-slots (first-node last-node length) list
    (let ((new-node (make-instance 'ListNode :data item)))
      (if (= length 0)
          (setf first-node new-node
                last-node new-node)
          (progn
            (listnode-link-before new-node first-node)
            (setf first-node new-node)))
      (incf length)))
  list)

(defmethod insert-last ((list AlxList) (item t))
  (with-slots (first-node last-node length) list
    (if (= length 0)
        (insert-first list item)
        (let ((new-node (make-instance 'ListNode :data item)))
          (listnode-link-after new-node last-node)
          (setf last-node new-node)
          (incf length))))
  list)

(defmethod remove-first ((list AlxList))
  (with-slots (first-node last-node length) list
    (if (> length 0)
        (let ((new-first-node (listnode-next first-node)))
          (listnode-unlink-node first-node)
          (setf first-node new-first-node)
          (decf length))))
  list)

(defmethod remove-last ((list AlxList))
  (with-slots (first-node last-node length) list
    (if (> length 0)
        (let ((new-last-node (listnode-prev last-node)))
          (listnode-unlink-node last-node)
          (setf last-node new-last-node)
          (decf length))))
  list)

;; --------------------------------------------------------------------------------
;; Concurrent List
;; --------------------------------------------------------------------------------

(defclass ConcurrentList (AlxList)
  ((list-lock :initform (bt:make-recursive-lock "ConcurrentList lock"))))

;; --------------------------------------------------------------------------------
;; Tree
;; --------------------------------------------------------------------------------

(defclass TreeNode ()
  ((parent :initarg :parent
           :initform nil
           :accessor treenode-parent)
   (children :initarg :children
             :initform '()
             :accessor treenode-children)
   (data :initarg :data
         :initform nil
         :accessor treenode-data)))


;; --------------------------------------------------------------------------------
;; Graph
;; --------------------------------------------------------------------------------



;; --------------------------------------------------------------------------------


;; ================================================================================
;; fucking around with sockets
;; ================================================================================

(defconstant +default-server-port+ 8080)
(defconstant +default-server-address+ "127.0.0.1")
(defconstant +default-server-name+ "default server name")

(defparameter *servers* '())

(defclass server ()
  ((port :initarg :port
         :initform +default-server-port+
         :reader port)
   (name :initarg :name
         :initform +default-server-name+)))

(defun foo (arg1 &optional arg2)
  (list arg1 arg2))

(defun read-all (stream)
  (loop for char = (read-char-no-hang stream nil :eof)
     until (or (null char) (eq char :eof)) collect char into msg
     finally (return (values msg char))))

(defun run-server (port &optional name)
  (let* ((server-name (if name name +default-server-name+))
         (server (make-instance 'server :port port :name server-name))
         (connections '()))
    (push server *servers*)
    (with-slots (port name) server
      (push (socket-listen "127.0.0.1" port :reuse-address t) connections)
      (unwind-protect
           (loop (loop for ready in (wait-for-input connections) do
                      (if (typep ready 'stream-server-usocket)
                          (push (socket-accept ready) connections)
                          (let* ((stream (socket-stream ready))
                                 (recvd-msg (concatenate 'string "" (read-all stream))))
                            (write-string recvd-msg stream)))))
        (progn
          (format t "start of unwind")
          (loop for conn in connections do (loop while (socket-close conn))))))))

(defun handle-request (stream)
  (let ((line (read-line stream)))
    (log:info "got msg: ~a" line)
    (format stream "You said: ~a" line)
    (terpri stream)
    (force-output stream)))

(defun echo-server (port)
  (let ((server-socket (socket-listen #(127 0 0 1) port :reuse-address t)))
    (multiple-value-bind (addr port) (get-local-name server-socket)
      (log:info "start listening on ~a:~a" addr port))
    (wait-for-input server-socket)
    (let* ((client-socket (socket-accept server-socket))
           (client-stream (socket-stream client-socket)))
      (multiple-value-bind (addr port) (get-peer-name client-socket)
        (log:info "got connection from ~a:~a" addr port))
      (handle-request client-stream)
      (close client-stream)
      (socket-close server-socket))))

(defun echo-client (port msg)
  (let* ((socket (socket-connect #(127 0 0 1) port))
         (stream (socket-stream socket)))
    (write-line msg stream)
    (force-output stream)
    (let ((result (read-line stream)))
      (close stream)
      (socket-close socket)
      result)))

(defparameter *server* nil)

(defun eval-stream (stream)
  (let* ((line (read-line stream))
         (eval-result (eval (read-from-string line))))
    (log:info "evaluating: ~a" line)
    (log:info "result: ~a" eval-result)
    (format stream "~a" eval-result)
    (terpri stream)
    (force-output stream)))

(defun accept-loop (socket)
  (loop (wait-for-input socket)
     (let* ((client-sock (socket-accept socket))
            (stream (socket-stream client-sock)))
       (log:info "got connection from ~a:~a" (get-peer-address client-sock) (get-peer-port client-sock))
       (make-thread (lambda ()
                      (with-open-stream (stream stream)
                        (eval-stream stream)))))))

(defun start-server (port)
  (let ((serv-socket (socket-listen "127.0.0.1" port :reuse-address t)))
    (log:info "starting server on ~a:~a"
              (get-local-address serv-socket)
              (get-local-port serv-socket))
    (setf *server*
          (make-thread (lambda ()
                         (unwind-protect (accept-loop serv-socket)
                           (log:info "shutting down server ~a:~a"
                                     (get-local-address serv-socket)
                                     (get-local-port serv-socket))
                           (socket-close serv-socket)))))))

(defun stop-server ()
  (when *server*
    (destroy-thread *server*)
    (setf *server* nil)))


(defun write-to-file (filepath)
  (with-open-file (fstream filepath
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
    (format fstream "~a~%" (read-line))))


(defun singleton-set (elem)
  (let ((f (lambda (x) (= elem x))))
    f))

(defun foo (x y)
  (+ x y))


;; (defun default-tcp-handler (stream)
;;   (declare (type stream stream))
;;   (terpri stream)
;;   (format stream "hello from server?~%"))



;; (defun echo-server (port &optional (log-stream *standard-output*))
;;   (let ((connections (list (socket-listen "127.0.0.1" port :reuse-address t))))
;;     (unwind-protect
;;          (loop (loop for ready in (wait-for-input connections :ready-only t)
;;                   do (if (typep ready 'stream-server-usocket)
;;                          (push (socket-accept ready) connections)
;;                          (let* ((stream (socket-stream ready))
;;                                 (msg (concatenate 'string "You said: " (read-all stream))))
;;                            (format log-stream "Got message...~%")
;;                            (write-string msg stream)
;;                            (socket-close ready)
;;                            (setf connections (remove ready connections))))))
;;       (loop for c in connections do (loop while (socket-close c))))))


;; ================================================================================
;; log parsing (error handling example)
;; ================================================================================

(defclass LogEntry ()
  ((time 	:initform nil :initarg :time 	:accessor LogEntry-time)
   (level 	:initform nil :initarg :level 	:accessor LogEntry-level)
   (entry 	:initform nil :initarg :entry 	:accessor LogEntry-entry)))

(defmethod print-object ((log-entry LogEntry) (stream stream))
  (with-slots (time level entry) log-entry
    (format stream "\"~a\"" entry)))

(define-condition malformed-log-entry-error (error)
  ((text :initarg :text :reader text)))

(defun well-formed-log-entry? (entry-str)
  (string-equal entry-str "ent " :start1 0 :end1 (min (length "ent ") (length entry-str))))

;; Version without restarts
;; ----------------------------------------
(defun parse-log-entry (entry-str)
  (if (well-formed-log-entry? entry-str)
      (make-instance 'LogEntry :entry entry-str)
      (error 'malformed-log-entry-error :text entry-str)))

(defun parse-log-file (log-file-path)
  (with-open-file (log-file-stream log-file-path ;; error when file does not exist
                                   :direction :input)
    (loop
       for entry-txt = (read-line log-file-stream nil nil) while entry-txt
       for entry = (handler-case (parse-log-entry entry-txt)
                     (malformed-log-entry-error (err)
                       (progn
                         (log:warn "bad log entry: ~a" (text err))
                         nil)))
       when entry collect it)))


;; Version with restarts
;; ----------------------------------------
(defun parse-log-entry (entry-str)
  (if (well-formed-log-entry? entry-str)
      (make-instance 'LogEntry :entry entry-str)
      (restart-case (error 'malformed-log-entry-error :text entry-str)
        (use-value (value) value)
        (reparse-entry (fixed-entry-str) (parse-log-entry fixed-entry-str)))))

(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
    (loop
       for text = (read-line in nil nil) while text
       for entry = (restart-case (parse-log-entry text)
                     (skip-log-entry () nil))
       when entry collect it)))

(defparameter *log-files* '("~/.tmp/stam.txt"))

(defun analyze-entry (log-entry)
  log-entry)

(defun analyze-log (log-filepath)
  (let ((entries (parse-log-file log-filepath)))
    (log:info "log entries for ~a: ~a" log-filepath entries)
    (dolist (entry entries)
      (analyze-entry entry))))

(defun skip-log-entry (err)
  (let ((restart (find-restart 'skip-log-entry)))
    (when restart (invoke-restart restart))))

(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error
                  (lambda (err)
                    (use-value (make-instance 'LogEntry
                                              :entry (concatenate 'string "[INVALID entry] " (text err)))))))
    (dolist (log *log-files*)
      (log:info "analyzing ~a" log)
      (analyze-log log))))

(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error #'skip-log-entry))
    (dolist (log *log-files*)
      (analyze-log log))))


;; ================================================================================

(defmacro foomac (code)
  (pprint code))

(defun foo (&rest args)
  (format t "foo ~a~%" args))

(defun bar (&rest args)
  (format t "bar ~a~%" args))

(defmacro foo-to-bar (expr)
  (loop :for ele :in expr :collect
     (if (atom ele)
         (if (eql ele 'foo) 'bar ele)
         (foo-to-bar ele))))

(defmacro replace-function (orig-f new-f &body body)
  `(progn
     ,@(loop :for expression :in body :collect
          (if (atom expression)
              (if (eql expression orig-f) new-f expression)
              (replace-function orig-f new-f expression)))))

(defmacro replace-function-expression (orig-f new-f expression)

  (cond ((atom expression) (if (eql expression orig-f) new-f expression))
        ((listp expression) `(loop :for ele :in expression :collect ,@(replace-function-expression ele)))))

(defmacro replace-function (orig-f new-f &body body)
  `(progn
     ,@(loop :for expression :in body :collect
          (if (listp expression)
              (if (eql (first expression) orig-f)
                  `(,new-f ,@(replace-function orig-f new-f (rest expression)))
                  `(,@(replace-function orig-f new-f (rest expression))))
              expression))))


(defmacro blat (&body body)
  `(progn ,@(loop :for exp :in body :collect exp)))


(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))


(defmacro tmp (&body body)
  body)
