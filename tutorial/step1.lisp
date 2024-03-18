(uiop:define-package #:scrapycl/tutorial/step1
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:str
                #:split)
  (:import-from #:alexandria
                #:write-string-into-file))
(in-package #:scrapycl/tutorial/step1)


(defclass quotes-page-request (scrapycl:request)
  ())


(defclass quotes-spider (scrapycl:spider)
  ()
  (:default-initargs
   :initial-requests (list (make-instance 'quotes-page-request
                                          :url "https://quotes.toscrape.com/page/1/")
                           (make-instance 'quotes-page-request
                                          :url "https://quotes.toscrape.com/page/2/"))))


(defmethod scrapycl:process ((spider quotes-spider)
                             (request quotes-page-request))
  (multiple-value-bind (data url)
      (scrapycl:fetch spider request)
    (let* ((page-number (third (str:split "/" (quri:uri-path  url))))
           (filename (format nil "quotes-~A.html" page-number)))
      (alexandria:write-string-into-file data filename
                                         :if-exists :supersede)
      (log:info "Page saved to" filename)
      ;; return nothing, to stop processing
      (values))))
