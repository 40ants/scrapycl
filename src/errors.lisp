(uiop:define-package #:scrapycl/errors
  (:use #:cl)
  (:import-from #:scrapycl/core
                #:fetch-error
                #:scrapycl-error
                #:response-url
                #:response-status
                #:response-body
                #:response-headers)
  (:import-from #:quri))
(in-package #:scrapycl/errors)


(define-condition scrapycl-error (error)
  ()
  (:documentation "Base class for all ScrapyCL errors."))


(define-condition fetch-error (scrapycl-error)
  ((url :initarg :url
        :reader response-url)
   (status :initarg :status
           :reader response-status)
   (body :initarg :body
         :reader response-body)
   (headers :initarg :headers
            :reader response-headers))
  (:documentation "This condition is signalled when SCRAPYCL:FETCH generic-function gets non 200 status code.")
  (:report (lambda (condition stream)
             (format stream "Unable to fetch from '~A' (response code=~A)"
                     (quri:render-uri (response-url condition))
                     (response-status condition)))))

