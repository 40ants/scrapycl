(uiop:define-package #:scrapycl/task
  (:use #:cl))
(in-package #:scrapycl/task)


(defclass task ()
  ((object :initarg :object
           :reader task-object)
   (output-func :initarg :output-func
                :reader task-output-func)))


(defun make-task (object &key output-func)
  (make-instance 'task
                 :object object
                 :output-func output-func))


(defmethod print-object ((obj task) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A"
            (task-object obj))))
