(uiop:define-package #:scrapycl/tutorial/step3
  (:use #:cl)
  (:import-from #:scrapycl)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:str
                #:split)
  (:import-from #:log)
  (:import-from #:alexandria
                #:write-string-into-file))
(in-package #:scrapycl/tutorial/step3)


(defclass quotes-page-request (scrapycl:request)
  ())


(defclass author-page-request (scrapycl:request)
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


(defclass author-item ()
  ((name :initarg :name
         :type string
         :reader author-name)
   (birthday :initarg :birthday
             :type string
             :reader author-birthday)
   (bio :initarg :bio
        :type string
        :reader author-bio)))


(defmethod print-object ((obj author-item) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A"
            (author-name obj))))


(defmethod scrapycl:process ((spider quotes-spider)
                             (request index-page-request))
  (multiple-value-bind (data base-url)
      (scrapycl:fetch spider request)
    (log:info "Fetched" base-url)
    
    (let ((quotes (lquery:$
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
                                      :tags (coerce tags 'list))))))
          (next-page-url (lquery:$1
                           (initialize data)
                           "ul.pager a"
                           (attr "href")
                           (merge-url-with base-url)))
          (author-urls (lquery:$
                         (initialize data)
                         ".author"
                         (next "a")
                         (attr "href")
                         (merge-url-with base-url))))
      ;; Now return objects and new requests
      (list quotes
            (map 'list (lambda (url)
                         (make-instance 'author-page-request
                                        :url url))
                 author-urls)
            (when next-page-url
              (make-instance 'index-page-request
                             :url next-page-url))))))


(defmethod scrapycl:process ((spider quotes-spider)
                             (request author-page-request))
  (multiple-value-bind (data base-url)
      (scrapycl:fetch spider request)
    (log:info "Fetched" base-url)

    (lquery:$1
      (initialize data)
      (combine
       (lquery:$1
         "h3.author-title"
         (text))
       (lquery:$1
         ".author-born-date"
         (text))
       (lquery:$1
         ".author-description"
         (text)
         (map #'str:trim)))
      (map-apply
       (lambda (name birthday bio)
         (make-instance 'author-item
                        :name name
                        :birthday birthday
                        :bio bio))))))


(defun start ()
  (scrapycl:start (make-instance 'quotes-spider)
                  :wait t
                  :output (scrapycl/output/typed:typed-output
                           (list (cons 'quote-item
                                       (scrapycl/output/json:json-lines #P"quotes.json"))
                                 (cons 'author-item
                                       (scrapycl/output/json:json-lines #P"authors.json"))))))
