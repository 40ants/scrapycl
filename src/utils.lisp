(uiop:define-package #:scrapycl/utils
  (:use #:cl)
  (:import-from #:alexandria)
  (:import-from #:str)
  (:import-from #:spinneret)
  (:import-from #:plump)
  (:import-from #:lquery)
  (:import-from #:quri)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:scrapycl/core
                #:preview))
(in-package #:scrapycl/utils)


(defun make-page-from (nodes)
  (let ((spinneret:*html-style* :human))
    (spinneret:with-html-string
      (:html
          (:head
            (:meta :charset "UTF-8")
            (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
            (:script :src "https://cdn.tailwindcss.com"))
          (:body
           (:div :class "flex flex-col gap-4"
                 (:h1 (if (= (length nodes)
                             1)
                          "1 node was found"
                          (fmt "~A nodes were found"
                               (length nodes))))
                 (:ol
                  (loop for node across nodes
                        for idx upfrom 0
                        do (:li
                            (:h2 (fmt "Node ~A"
                                      idx))
                            (etypecase node
                              (string
                               (:h3 "Code")
                               (:code (:pre node))
                               (:h3 "Preview")
                               (:raw node))
                              (plump:node
                               (let ((html (str:join #\Newline
                                             (map 'list
                                                  #'identity
                                                  (lquery:$
                                                    node
                                                    (serialize))))))
                                 (:h3 "Code")
                                 (:code (:pre html))
                                 (:h3 "Preview")
                                 (:raw html))
                               ;; (loop for html across (lquery:$ node
                               ;;                         (serialize))
                               ;;       do (:code (:pre html))
                               ;;          (:raw html))
                               )))))))))))


;; (defun make-page-from (nodes)
;;   (clip:process-to-string "
;; <html>
;;   <head>
;;     <script src=\"https://cdn.tailwindcss.com\"/>
;;   </head>
;;   <body>
;;     <ol iterate=\"nodes\" class=\"flex flex-col gap-4\">
;;       <li class=\"flex flex-col gap-2 border-2\">
;;         <h1>Preview</h1>
;;         <code><pre><c:splice lquery=\"(text *)\"></c:splice></pre></code>
;;         <h1>Code</h1>
;;         <div lquery=\"(text *)\"></div>
;;       </li>
;;     </ol>
;;   </body>
;; </html>
;; "
;;                           :nodes nodes))


(defun preview (nodes)
  (alexandria:with-output-to-file (stream "~/projects/mememo/tmp/index.html"
                                          :if-exists :supersede)
    (let ((html (make-page-from nodes)))
      (write-string html stream)
      ;; (plump:serialize new-nodes stream)
      )
    ;; (let ((spinneret:*html-style* :human))
    ;;   (spinneret:with-html
    ;;     (:html
    ;;         (:head
    ;;           (:meta :charset "UTF-8")
    ;;           (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
    ;;           (:script :src "https://cdn.tailwindcss.com"))
    ;;         (:body
    ;;          (:div :class "flex flex-col gap-4"
    ;;                (loop for node across nodes
    ;;                      do (etypecase node
    ;;                           (string
    ;;                            (:code (:pre node))
    ;;                            (:raw node))
    ;;                           (plump:node  (loop for html across (lquery:$ node
    ;;                                                                (serialize))
    ;;                                              do (:code (:pre html))
    ;;                                                 (:raw html))))))))))
    ))



(lquery:define-lquery-list-function debug (nodes ;; &key (filename #P"index.html")
                                           )
  (preview nodes)
  (values nodes))


(lquery:define-lquery-function merge-url-with (node base-url)
  (when node
    (check-type node string)
    (quri:render-uri
     (quri:merge-uris node base-url))))
