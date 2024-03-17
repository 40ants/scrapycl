(uiop:define-package #:scrapycl/tutorial/step1
  (:use #:cl)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:str
                #:split)
  (:import-from #:alexandria
                #:write-string-into-file))
(in-package #:scrapycl/tutorial/step1)


(defclass quotes-page-request (scrapycl:request)
  ())


(defclass step1 (scrapycl:spider)
  ()
  (:default-initargs
   :initial-requests (list (make-instance 'quotes-page-request
                                          :url "https://quotes.toscrape.com/page/1/")
                           (make-instance 'quotes-page-request
                                          :url "https://quotes.toscrape.com/page/2/"))))


(defmethod scrapycl:process ((spider step1)
                             (request quotes-page-request))
  (multiple-value-bind (data url)
      (scrapycl:fetch spider request)
    (let* ((page-number (third (split "/" (quri:uri-path  url))))
           (filename (fmt "quotes-~A.html" page-number)))
      (write-string-into-file data filename
                              :if-exists :supersede)
      ;; return nothing, to stop processing
      (values))))
