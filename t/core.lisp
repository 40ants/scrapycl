(uiop:define-package #:scrapycl-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:scrapycl-tests/core)


(deftest test-example ()
  (ok t "Replace this test with something useful."))
