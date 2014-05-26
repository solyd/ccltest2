;; Version with restarts
;; ----------------------------------------
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

(defun log-analyzer-skipper ()
  (handler-bind ((malformed-log-entry-error #'skip-log-entry))
    (dolist (log *log-files*)
      (analyze-log log))))
