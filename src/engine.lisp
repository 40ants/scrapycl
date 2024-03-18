(uiop:define-package #:scrapycl/engine
  (:use #:cl)
  (:import-from #:bt2)
  (:import-from #:log)
  (:import-from #:scrapycl/core
                #:spider
                #:start
                #:process
                #:enqueue
                #:request-url)
  (:import-from #:scrapycl/spider)
  (:import-from #:scrapycl/task
                #:task
                #:task-object
                #:task-spider
                #:make-task)
  (:import-from #:scrapycl/task)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:scrapycl/core
                #:stop-output)
  (:import-from #:serapeum
                #:->))
(in-package #:scrapycl/engine)


(declaim (ftype (function (scrapycl/core::spider)
                          (values (or null task) &optional))
                get-next-task))

(defun get-next-task (spider)
  (bt2:with-lock-held ((scrapycl/spider::%spider-queue-lock spider))
    (prog1 (car
            (scrapycl/spider::%spider-queue spider))
      (setf (scrapycl/spider::%spider-queue spider)
            (cdr
             (scrapycl/spider::%spider-queue spider))))))



(-> process-tasks
    (scrapycl/core::spider &key (:on-finish (or null function)))
    (values &optional))

(defun process-tasks (spider &key on-finish)
  (let ((seen-urls (make-hash-table :test 'equal)))
    (flet ((seen-url-p (url)
             (gethash url seen-urls))
           (register-url (url)
             (setf (gethash url seen-urls)
                   t)))
      (declare (dynamic-extent #'seen-url-p))
      
      (loop for task = (get-next-task spider)
            while task
            do (let* (;; It is important to have this var
                      ;; bound around EXECUTE and consequent
                      ;; ENQUEUE calls:
                      (*output-func* (scrapycl/task::task-output-func task))
                      (to-enqueue (execute spider task)))
                 ;; We allow to return a list of list or a list of arrays
                 ;; or some tree from arrays and lists just to make it
                 ;; easy to return results from PROCESS method when mixing
                 ;; objects and requests.
                 ;; Here we will not create an intermediate data-structures
                 ;; from the TO-ENQUEUE and will walk the graph instead
                 (labels ((walk (object)
                            (typecase object
                              ((or list array)
                               (map nil #'walk object))
                              ;; We need this block to not visit same URL twice and
                              ;; to break link loops:
                              (scrapycl/core:request
                               (unless (seen-url-p (request-url object))
                                 (register-url (request-url object))
                                 (enqueue spider object)))
                              (t
                               (enqueue spider object)))))
                   (declare (dynamic-extent #'walk))
                   (funcall #'walk to-enqueue))))))
  (when on-finish
    (funcall on-finish))
  (values))


(-> ensure-thread-is-running
    (scrapycl/core::spider &key (:on-finish (or null function)))
    (values &optional))

(defun ensure-thread-is-running (spider &key on-finish)
  (when (or (null (scrapycl/spider::%spider-thread spider))
            (bt2:thread-alive-p (scrapycl/spider::%spider-thread spider)))
    (flet ((process-tasks-wrapper ()
             (process-tasks spider :on-finish on-finish)))
      (setf (scrapycl/spider::%spider-thread spider)
            (bt2:make-thread #'process-tasks-wrapper
                             :name "Scrapycl Processor"))))
  (values))


(defvar *output-func* nil
  "This variable will be bound to a current tasks's output function. This way, when somebody enqueues an object and don't specifies an output func, then the parent task output func will be used. This way you can specify output in the START func and it will be used to process all items which has no specific method for SCRAPYCL:PROCESS generic-function.")


(defun enqueue (spider object &key (output-func nil output-func-p))
  (log:debug "Enqueueing new object" spider object)
  (bt2:with-lock-held ((scrapycl/spider::%spider-queue-lock spider))
    (let ((task (make-task object
                           :output-func (if output-func-p
                                            output-func
                                            *output-func*))))
      (push task (scrapycl/spider::%spider-queue spider)))))


(declaim (ftype (function (scrapycl/core::spider
                           scrapycl/task::task)
                          (values t &optional))
                execute))

(defun execute (spider task)
  (let ((object (task-object task)))
    (with-simple-restart (continue "Skip processing ~A" object)
      (let ((result (process spider
                             object)))
        (values result)))))


(defgeneric process (spider object)
  (:method :around ((spider t) (object t))
    (log:debug "Processing object" spider object)
    (with-log-unhandled ()
      (let ((results (call-next-method)))
        (unless results
          (log:debug "Process didn't return new objects"))
        results)))
  
  (:method ((spider t) (object t))
    (cond
      (*output-func*
       (funcall *output-func* object))
      (t
       (log:error "Please, implement SCRAPYCL:PROCESS method for (~S ~S)"
                  (type-of spider)
                  (type-of object))))))


(defmethod start ((spider spider) &key wait (output :list))
  (bt2:with-lock-held ((scrapycl/spider::%spider-queue-lock spider))
    (setf (scrapycl/spider::%spider-queue spider)
          nil))
    
  (uiop:while-collecting (collect-item)
    (let* ((output-is-function
             (and output
                  (or (functionp output)
                      (and (symbolp output)
                           (fboundp output)))))
           (output-func
             (cond
               ((and (symbolp output)
                     (eql output :list))
                #'collect-item)
               (output-is-function
                output))))
      (loop for request in (scrapycl/spider::%initial-requests spider)
            do (enqueue spider request
                        :output-func output-func))

      (flet ((close-output ()
               (when output-is-function
                 (funcall output 'stop-output))))
        (if wait
            (scrapycl/engine::process-tasks spider
                                            :on-finish
                                            #'close-output)
            (ensure-thread-is-running spider
                                      :on-finish #'close-output))))))
