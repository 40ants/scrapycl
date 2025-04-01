(uiop:define-package #:scrapycl/request
  (:use #:cl)
  (:import-from #:scrapycl/core
                #:request
                #:request-url
                #:request-dont-filter
                #:url))
(in-package #:scrapycl/request)


(defclass request ()
  ((url :initarg :url
        :type url
        :initform (error "Please, provide :URL argument.")
        :reader request-url
        :documentation "URL to fetch data from.")
   (dont-filter :initarg :dont-filter
                :type (member t nil)
                :initform nil
                :reader request-dont-filter
                :documentation "Exclude request from filtering.")))


(defmethod print-object ((obj request) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A"
            (request-url obj))))
