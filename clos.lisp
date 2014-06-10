(in-package #:ccltest2)

(defclass A ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defclass A1 (A)
  ((z :initarg :z :initform 0 :accessor z)))

(defclass A2 (A)
  ((w :initarg :w :initform 0 :accessor w)))

;; multiple inheritance
(defclass B (A1 A2)
  ((b :initarg :b :initform 0 :accessor b)))

;; "constructor" for A: this is called after the instance is created
(defmethod initialize-instance :after ((a A) &key)
  (log:info ""))

(defmethod initialize-instance :after ((a1 A1) &key)
  (log:info ""))

(defgeneric foo-a (a))

(defmethod foo-a :before ((a A))
  (log:info "A before"))

(defmethod foo-a :around ((a A))
  (log:info "before")
  (call-next-method)
  (log:info "after"))

(defmethod foo-a ((a A))
  (log:info ""))

(defmethod foo-a :after ((a A))
  (log:info ""))

(defmethod foo-a :before ((a1 A1))
  (log:info ""))

(defmethod foo-a :around ((a1 A1))
  (log:info "before")
  (call-next-method)
  (log:info "after"))

(defmethod foo-a ((a1 A1))
  (log:info "")
;  (call-next-method)
  )

(defmethod foo-a :after ((a1 A1))
  (log:info ""))

;; ================================================================================

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun instance-of-chain (obj)
  (let ((instance-of-list '())
        (curr-class (class-of obj))
        (prev-class nil))
    (while (not (eql curr-class prev-class))
         (setf prev-class curr-class)
         (setf curr-class (class-of curr-class))
         (push prev-class instance-of-list))
    instance-of-list))

;; (defun inherits-from-chain (obj)
;;   (let ((inherits-from-list '())
;;         (curr-class (class-of obj))
;;         (prev-class nil))
;;     (while (not (eql curr-class prev-class))
;;          (setf prev-class curr-class)
;;          (setf curr-class (class-of curr-class))
;;          (push prev-class inherits-from-list))
;;     inherits-from-list))


;; ================================================================================

;; multimethods
(defgeneric collision (obj1 obj2))

(defclass Entity ()
  ((hitpoints :initarg :hp
              :initform 100
              :accessor hp)
   (name :initarg :name
         :initform (error "entity name must be set")
         :reader name)))

(defmethod print-object ((entity Entity) (stream stream))
  (format stream "[HP = ~a] ~a" (hp entity) (name entity)))

(defconstant +asteroid-hp-bonus+ 1.5)
(defclass Asteroid (Entity)
  ((name :initform "asteroid")))

(defmethod initialize-instance :after ((asteroid Asteroid) &key)
  (with-slots ((hp hitpoints)) asteroid
    (setf hp (floor (* hp +asteroid-hp-bonus+)))))

(defconstant +spaceship-hp-bonus+ 1.2)
(defclass Spaceship (Entity)
  ((name :initform "spaceship")))

(defmethod initialize-instance :after ((spaceship Spaceship) &key)
  (with-slots ((hp hitpoints)) spaceship
    (setf hp (floor (* hp +spaceship-hp-bonus+)))))

(defmethod collision ((spaceship Spaceship) (asteroid Asteroid))
    (decf (hp spaceship) 40)
    (decf (hp asteroid) 20))

(defmethod collision ((spaceship1 Spaceship) (spaceship2 Spaceship))
    (decf (hp spaceship1) 10)
    (decf (hp spaceship2) 10))


;; ================================================================================

;; (defvar *account-numbers* 0)

;; (defgeneric withdraw (account amount)
;;   (:documentation
;;    "Withdraw the specified amount from the account.
;; 	Signal an error if the current balance is less than amount."))

;; (defclass bank-account ()
;;   ((customer-name :initarg :customer-name
;;                   :initform (error "Must supply a customer name."))
;;    (balance :initarg :balance
;;             :initform 0)
;;    (account-number :initform (incf *account-numbers*))))

;; (defmethod withdraw ((account bank-account) amount)
;;   (when (< (balance account) amount)
;;     (error "Account overdrawn."))
;;   (decf (balance account) amount))

;; (defmethod withdraw :before ((account checking-account) amount)
;;   (let ((overdraft (- amount (balance account))))
;;     (when (plusp overdraft)
;;       (withdraw (overdraft-account account) overdraft)
;;       (incf (balance account) overdraft))))

;; (defmethod withdraw ((account checking-account) amount)
;;   (let ((overdraft (- amount (balance account))))
;;     (when (plusp overdraft)
;;       (withdraw (overdraft-account account) overdraft)
;;       (incf (balance account) overdraft)))
;;   (call-next-method))

;; (defmethod withdraw ((proxy proxy-account) amount)
;;   (withdraw (proxied-account proxy) amount))

;; (defmethod withdraw ((account (eql *account-of-bank-president*)) amount)
;;   (let ((overdraft (- amount (balance account))))
;;     (when (plusp overdraft)
;;       (incf (balance account) (embezzle *bank* overdraft)))
;;     (call-next-method)))
