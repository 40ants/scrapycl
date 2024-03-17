(uiop:define-package #:scrapycl/errors
  (:use #:cl)
  (:import-from #:quri)
  (:export
   #:fetch-error
   #:scrapycl-error))
(in-package #:scrapycl/errors)


(define-condition scrapycl-error (error)
  ())


(define-condition fetch-error (scrapycl-error)
  ((url :initarg :url)
   (status :initarg :status)
   (body :initarg :body)
   (headers :initarg :headers))
  (:report (lambda (condition stream)
             (with-slots (url status)
                 condition
               (format stream "Unable to fetch from '~A' (response code=~A)"
                       (quri:render-uri url)
                       status)))))

