(uiop:define-package #:scrapycl-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing)
  (:import-from #:scrapycl
                #:spider
                #:request
                #:request-dont-filter-p
                #:process
                #:start))
(in-package #:scrapycl-tests/core)


(defclass repeated-url-spider (spider)
  ((processed :initform 0 :accessor processed)))


(defmethod process ((spider repeated-url-spider) (request request))
  (declare (ignore request))
  (when (= 1 (incf (processed spider)))
    (list (make-instance 'request :url "https://example.com/repeated")
          (make-instance 'request :url "https://example.com/repeated")
          (make-instance 'request :url "https://example.com/repeated"
                                  :dont-filter t))))


(deftest request-dont-filter ()
  (testing "The option defaults to false and accepts true"
    (ok (not (request-dont-filter-p
              (make-instance 'request :url "https://example.com"))))
    (ok (request-dont-filter-p
         (make-instance 'request :url "https://example.com" :dont-filter t))))
  (testing "Repeated URLs are skipped unless the request opts out"
    (let ((spider (make-instance 'repeated-url-spider
                                 :initial-requests
                                 (list (make-instance 'request
                                                      :url "https://example.com/start")))))
      (start spider :wait t)
      (ok (= 3 (processed spider))))))
