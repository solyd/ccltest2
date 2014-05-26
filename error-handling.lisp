(in-package #:ccltest2)

(defclass LogEntry ()
  ((time 	:initform (local-time:now)
            :initarg :time
            :accessor LogEntry-time)

   (level 	:initform :regular
            :initarg :level
            :accessor LogEntry-level)

   (entry 	:initform nil
            :initarg :entry
            :accessor LogEntry-entry)))

(defmethod print-object ((log-entry LogEntry) (stream stream))
  (with-slots (time level entry) log-entry
    (format stream "~a \"~a\"" time entry)))

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
  (with-open-file (log-file-stream log-file-path :direction :input)
    (loop
       :for entry-txt = (read-line log-file-stream nil nil) while entry-txt
       :for entry = (handler-case (parse-log-entry entry-txt)
                     (malformed-log-entry-error (err)
                       (progn
                         (log:warn "bad log entry: ~a" (text err))
                         nil)))
       :when entry :collect it)))
