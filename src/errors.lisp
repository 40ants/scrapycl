(uiop:define-package #:scrapycl/errors
  (:use #:cl)
  (:import-from #:scrapycl/core
                #:fetch-error
                #:scrapycl-error)
  (:import-from #:quri))
(in-package #:scrapycl/errors)


(define-condition scrapycl-error (error)
  ()
  (:documentation "Base class for all ScrapyCL errors."))


(define-condition fetch-error (scrapycl-error)
  ((url :initarg :url)
   (status :initarg :status)
   (body :initarg :body)
   (headers :initarg :headers))
  (:documentation "This condition is signalled when SCRAPYCL:FETCH generic-function gets non 200 status code.")
  (:report (lambda (condition stream)
             (with-slots (url status)
                 condition
               (format stream "Unable to fetch from '~A' (response code=~A)"
                       (quri:render-uri url)
                       status)))))

