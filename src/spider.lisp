(uiop:define-package #:scrapycl/spider
  (:use #:cl)
  (:import-from #:scrapycl/core
                #:enqueue
                #:start
                #:spider
                #:request)
  (:import-from #:serapeum
                #:queue
                #:soft-list-of)
  (:import-from #:bt2
                #:make-lock)
  (:import-from #:40ants-doc/ignored-words
                #:ignore-words-in-package))
(in-package #:scrapycl/spider)


(ignore-words-in-package '%spider-queue
                         '%spider-queue-lock
                         '%spider-thread
                         '%initial-requests)


(defclass spider ()
  ((queue :initform (queue)
          :type queue
          :accessor %spider-queue)
   (queue-lock :initform (make-lock :name "Scrapycl Queue Lock")
               :accessor %spider-queue-lock)
   (thread :initform nil
           :accessor %spider-thread)
   (initial-requests :initarg :initial-requests
                     :type (soft-list-of request)
                     :initform nil
                     :reader %initial-requests)))


(defgeneric start (spider &key &allow-other-keys))



