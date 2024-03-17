(uiop:define-package #:scrapycl/request
  (:use #:cl)
  (:import-from #:scrapycl/core
                #:url
                #:request)
  (:export #:request-url))
(in-package #:scrapycl/request)


(defclass request ()
  ((url :initarg :url
        :type url
        :initform (error "Please, provide :URL argument.")
        :reader request-url)))


(defmethod print-object ((obj request) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A"
            (request-url obj))))
