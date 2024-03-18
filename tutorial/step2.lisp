(uiop:define-package #:scrapycl/tutorial/step2
  (:use #:cl)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:str
                #:split)
  (:import-from #:alexandria
                #:write-string-into-file))
(in-package #:scrapycl/tutorial/step2)


(defclass quotes-page-request (scrapycl:request)
  ())


(defclass quotes-spider (scrapycl:spider)
  ()
  (:default-initargs
   :initial-requests (list (make-instance 'quotes-page-request
                                          :url "https://quotes.toscrape.com/"))))


(defclass quote-item ()
  ((text :initarg :text
         :type string
         :reader quote-text)
   (author :initarg :author
           :type string
           :reader quote-author)
   (tags :initarg :tags
         :type (serapeum:soft-list-of string)
         :reader quote-tags)))


(defmethod print-object ((obj quote-item) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A by ~A ~{#~A~^, ~}"
            (quote-text obj)
            (quote-author obj)
            (quote-tags obj))))


(defmethod scrapycl:process ((spider quotes-spider)
                             (request quotes-page-request))
  (let ((data (scrapycl:fetch spider request)))
    (lquery:$
      (initialize data)
      "div.quote"
      (combine
       (lquery:$1
         "span.text"
         (text))
       (lquery:$1
         "small.author"
         (text))
       (lquery:$
         "div.tags a.tag"
         (text)))
      (map-apply
       (lambda (text author tags)
         (make-instance 'quote-item
                        :text text
                        :author author
                        :tags (coerce tags 'list)))))))
