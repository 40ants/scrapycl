(uiop:define-package #:scrapycl/downloader
  (:use #:cl)
  (:import-from #:scrapycl/core
                #:fetch)
  (:import-from #:dex)
  (:import-from #:scrapycl/request
                #:request
                #:request-url)
  (:import-from #:scrapycl/errors
                #:fetch-error)
  (:import-from #:scrapycl/spider
                #:spider))
(in-package #:scrapycl/downloader)


(defgeneric fetch (spider request &key max-redirects timeout custom-headers)
  (:method ((spider spider) (request request) &key max-redirects
                                                   timeout
                                                   (custom-headers '(("User-Agent" . "ScrapyCL (https://40ants.com/scrapycl/)"))))
    (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
      (multiple-value-bind (body status headers last-uri)
          (dex:get (request-url request)
                   :max-redirects (or max-redirects 5)
                   :read-timeout timeout
                   :connect-timeout timeout
                   :headers custom-headers)
        (unless (= status 200)
          (error 'fetch-error
                 :url last-uri
                 :code status
                 :body body
                 :headers headers))
        (values body
                last-uri)))))
