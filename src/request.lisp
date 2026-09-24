(uiop:define-package #:scrapycl/request
  (:use #:cl)
  (:import-from #:scrapycl/core
                #:request
                #:request-url
                #:request-dont-filter-p
                #:url))
(in-package #:scrapycl/request)


(defclass request ()
  ((url :initarg :url
        :type url
        :initform (error "Please, provide :URL argument.")
        :reader request-url
        :documentation "URL to fetch data from.")
   (dont-filter :initarg :dont-filter
                :type boolean
                :initform nil
                :reader request-dont-filter-p
                :documentation "Process this request even if its URL was already processed.")))


(defmethod print-object ((obj request) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A"
            (request-url obj))))
