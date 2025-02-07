(uiop:define-package #:scrapycl/downloader
  (:use #:cl)
  (:import-from #:scrapycl/core
                #:spider
                #:fetch
                #:request
                #:request-url
                #:fetch-error)
  (:import-from #:dex)
  (:export
   #:retry-request
   #:retry-if))
(in-package #:scrapycl/downloader)


(defvar *default-headers* '(("User-Agent" . "ScrapyCL (https://40ants.com/scrapycl/)")))


(defgeneric fetch (spider request &key method content max-redirects timeout custom-headers insecure)
  (:documentation "Fetches page from request's URL.

                   Returns a multiple values:

                   - A string with HTML response.
                   - URL from which response was received. Might be different from original URL because of redirects.
                   - A hash-table with reponse HTTP headers.")
  (:method ((spider spider) (request request) &rest args)
    (apply #'fetch
           spider
           (request-url request)
           args))
  
  (:method ((spider spider) (request-url string) &rest args
                                                 &key (method :get)
                                                      max-redirects
                                                      content
                                                      timeout
                                                      (custom-headers *default-headers*)
                                                      insecure)
    (restart-case
        (multiple-value-bind (body status headers last-uri)
            (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
              (dex:request request-url
                           :method method
                           :content content
                           :max-redirects (or max-redirects 5)
                           :read-timeout timeout
                           :connect-timeout timeout
                           :insecure insecure
                           :headers custom-headers))
          (unless (= status 200)
            (error 'fetch-error
                   :url last-uri
                   :status status
                   :body body
                   :headers headers))
          (values body
                  last-uri
                  headers))
      (retry-request ()
        :report "Retry fetch attempt"
        (apply #'fetch spider request-url
               args)))))


(defun retry-request (e)
  "Call retry-request restart unconditionally and without delay."
  (let ((restart (find-restart 'retry-request e)))
    (when restart
      (invoke-restart restart))))


(defun retry-if (predicate &key (times 3) (delay 1) (max-delay (* 10 60)) (multiplicator 2))
  "Call retry-request restart is predicate returns T and with exponential delay."
  (let ((attempts 0)
        (current-delay delay))
    (flet ((retry-handler (err)
             (let ((restart (find-restart 'retry-request err)))
               (when (and restart
                          (< attempts times)
                          (funcall predicate err))
                 (incf attempts)
                 (sleep (min current-delay
                             max-delay))

                 (log:debug "Retrying request, attempt number ~A" attempts)

                 (setf current-delay
                       (* current-delay multiplicator))
                 (invoke-restart restart)))))
      #'retry-handler)))
