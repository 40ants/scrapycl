(uiop:define-package #:scrapycl-docs/tutorial
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:named-readtables
                #:in-readtable))
(in-package #:scrapycl-docs/tutorial)

(in-readtable pythonic-string-syntax)


(defsection @tutorial (:title "Tutorial"
                       :ignore-words ("CSS"
                                      "CLOS"
                                      "SCRAPYCL:STOP-OUTPUT"
                                      "NEXT"
                                      "AUTHOR-PAGE-REQUEST"
                                      "DSL"
                                      "AUTHOR-ITEM"
                                      "QUOTE-ITEM"
                                      "MERGE-WITH-URL"))
  (@intro section)
  (@our-first-scraper section)
  (@extracting-data section)
  (@extracting-quotes-and-authors section)
  (@storing-data section)
  (@following-links section))


(defsection @intro (:title "Introduction")
  """
In this tutorial we'll train our parsing skill on this toy site: https://quotes.toscrape.com/.
We will follow [Scrapy's tutorial](https://docs.scrapy.org/en/latest/intro/tutorial.html) and see if we can get all the data using ScrapyCL.

You will find whole code for this tutorial in the `tutorial/` folder.


Firstly Scrapy tutorial shows us how to experiment with HTTP response in the REPL. But with Common Lisp we have much more sofisticated REPL out of the box. So we skip this step:

```
scrapy shell "https://quotes.toscrape.com/page/1/"
```

Right to the Common Lisp REPL!
""")


(defsection @our-first-scraper (:title "Our First Scraper")
  """
ScrapyCL is built around CLOS. Every scraping pipeline in this framework operates on CLOS objects.
Most generic-functions accept a SCRAPYCL:SPIDER object as a first argument. Also, requests to HTML pages are typed.
This way you are telling to the framework how each page should be processed.

First thing we need to do is to define a class of the request to a page with quotes:

```lisp
CL-USER> (defclass quotes-page-request (scrapycl:request)
           ())
#<STANDARD-CLASS COMMON-LISP-USER::QUOTES-PAGE-REQUEST>

```

Next, we define a class for the spider:

```lisp
CL-USER> (defclass quotes-spider (scrapycl:spider)
           ()
           (:default-initargs
            :initial-requests (list (make-instance 'quotes-page-request
                                                   :url "https://quotes.toscrape.com/page/1/")
                                    (make-instance 'quotes-page-request
                                                   :url "https://quotes.toscrape.com/page/2/"))))
```

Here we tell the spider to start from two initial pages.

Now it is time to make our first HTTP request and to see content of the page.
I'll save the page's content to a variable to be able to play with parsing.


```lisp
CL-USER> (defparameter *response*
           (scrapycl:fetch (make-instance 'quotes-spider)
                           (make-instance 'quotes-page-request
                                          :url "https://quotes.toscrape.com/page/1/")))
*RESPONSE*
CL-USER> *response*
"<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="UTF-8">
	<title>Quotes to Scrape</title>
    <link rel="stylesheet" href="/static/bootstrap.min.css">
    <link rel="stylesheet" href="/static/m...[sly-elided string of length 11011]"
```

The first version of the spider in Scrapy tutorial just saves a page's content into the file. Let's do the same but with ScrapyCL!

First, try to start our scraper:

```
CL-USER> (scrapycl:start (make-instance 'quotes-spider) :wait t)
(#<QUOTES-PAGE-REQUEST https://quotes.toscrape.com/page/2/>
 #<QUOTES-PAGE-REQUEST https://quotes.toscrape.com/page/1/>)
```

It returns initial page requests as is because we didn't write a method for SCRAPYCL:PROCESS generic-function. Now we'll define it to save content into the files:

```
CL-USER> (defmethod scrapycl:process ((spider quotes-spider)
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
#<STANDARD-METHOD SCRAPYCL:PROCESS (QUOTES-SPIDER QUOTES-PAGE-REQUEST) {1003CCD523}>
```

Next attempt to start the scraper will output information that data was saved to the files:

```
CL-USER> (scrapycl:start (make-instance 'quotes-spider) :wait t)
 <INFO> [18:30:37] cl-user (process quotes-spider quotes-page-request) -
  Page saved to FILENAME: "quotes-2.html" 
 <INFO> [18:30:37] cl-user (process quotes-spider quotes-page-request) -
  Page saved to FILENAME: "quotes-1.html" 
NIL
```

Now it is time to extract useful information out from these HTML pages.
"""
  )


(defsection @extracting-data (:title "Extracting the Data")
  """
  Where Scrapy shows iPython REPL:

```
>>> response.css("title::text").getall()
['Quotes to Scrape']
```

We have a full-featured Common Lisp REPL. For HTML parsing we'll use great [lQuery library](https://shinmera.github.io/lquery/). Here is how we can reproduce python code in Lisp using lquery DSL:


```
CL-USER> (lquery:$
           (initialize *response*)
           "title"
           (text))
#("Quotes to Scrape")
```

lQuery is parses data in a functional way. It has [amazing documentation](https://shinmera.github.io/lquery/). Take a moment and read it to understand the basic principles.

Then Python tutorial shows us what `getall` method returns:

```
response.css("title").getall()
['<title>Quotes to Scrape</title>']
```

With lisp we could do the same. Just drop `(text)` form at the end of the lquery pipeline:

```
CL-USER> (lquery:$
           (initialize *response*)
           "title")
#(#<PLUMP-DOM:ELEMENT title {1004CAAF43}>)
```

But this code returns us a set of HTML nodes. If you want to see the actual HTML code behind it,
use `(serialize)` form:

```
CL-USER> (lquery:$
           (initialize *response*)
           "title"
           (serialize))
#("<title>Quotes to Scrape</title>")
```

To get only a single item in Python you use `get` method instead of `getall`:

```
>>> response.css("title::text").get()
'Quotes to Scrape'
```

In Lisp use `lquery:$1` macro instead of `lquery:$`:

```
CL-USER> (lquery:$1
           (initialize *response*)
           "title"
           (text))
"Quotes to Scrape"
```

You see, it returns a single object instead of an array!

As an alternative, you could’ve written in Python:

```
>>> response.css("title::text")[0].get()
'Quotes to Scrape'
```

In Lisp we can do the same:

```
CL-USER> (let* ((nodes (lquery:$
                         (initialize *response*)
                         "title"))
                (title-node (elt nodes 0)))
           (plump:text title-node))
"Quotes to Scrape"
```

There is no analogue to `re` from Scrapy in lQuery:

```
>>> response.css("title::text").re(r"Quotes.*")
['Quotes to Scrape']
```

but you can use a filter function:

```
CL-USER> (lquery:$
           (initialize *response*)
           "title"
           (text)
           (filter (lambda (text)
                     (cl-ppcre:scan "Quotes.*" text))))
#("Quotes to Scrape")
```

Another example from Python code:

```
>>> response.css("title::text").re(r"Q\w+")
['Quotes']
```

Becomes in Lisp:

```
CL-USER> (lquery:$
           (initialize *response*)
           "title"
           (text)
           (each (lambda (text)
                   (cl-ppcre:scan-to-strings "Q\\w+" text))
                 :replace t))
#("Quotes")
```


And this Python code:

```
>>> response.css("title::text").re(r"(\w+) to (\w+)")
['Quotes', 'Scrape']
```

becomes:


```
CL-USER> (lquery:$1
           (initialize *response*)
           "title"
           (text)
           (map (lambda (text)
                  (nth-value 1
                             (cl-ppcre:scan-to-strings "(\\w+) to (\\w+)"
                                                       text)))))
#("Quotes" "Scrape")
```
  """)


(defsection @extracting-quotes-and-authors (:title "Extracting Quotes and Authors (step2.lisp)")
  """
  We already have first page's content in the `*response*` variable. Now let's extract quotes!

Instead of this Python code:

```
response.css("div.quote")
[<Selector query="descendant-or-self::div[@class and contains(concat(' ', normalize-space(@class), ' '), ' quote ')]" data='<div class="quote" itemscope itemtype...'>,
<Selector query="descendant-or-self::div[@class and contains(concat(' ', normalize-space(@class), ' '), ' quote ')]" data='<div class="quote" itemscope itemtype...'>,
...]
```

We can do this in Lisp:

```
CL-USER> (lquery:$
           (initialize *response*)
           "div.quote")
#(#<PLUMP-DOM:ELEMENT div {1002769EC3}> #<PLUMP-DOM:ELEMENT div {100276B153}>
  #<PLUMP-DOM:ELEMENT div {100276C043}> #<PLUMP-DOM:ELEMENT div {100276D4A3}>
  #<PLUMP-DOM:ELEMENT div {100984F243}> #<PLUMP-DOM:ELEMENT div {1009860F43}>
  #<PLUMP-DOM:ELEMENT div {10098621D3}> #<PLUMP-DOM:ELEMENT div {1009862EF3}>)
```

Here is how we can to limit the number of items to not clutter the REPL. lQuery provides a `(function ...)` form where you can call any function you like. We'll use it to apply Serapeum's `take` function to cut only two first elements from the array of nodes:

```
CL-USER> (lquery:$
           (initialize *response*)
           "div.quote"
           (function
            (lambda (nodes)
             (serapeum:take 2 nodes))))
#(#<PLUMP-DOM:ELEMENT div {10028E1903}> #<PLUMP-DOM:ELEMENT div {10028E2B93}>)
```

Now it is easy to add `(serialize)` form and preview the extracted pieces:

```
CL-USER> (lquery:$
           (initialize *response*)
           "div.quote"
           (function
            (lambda (nodes)
             (serapeum:take 2 nodes)))
           (serialize))
#("<div class=\"quote\" itemscope=\"\" itemtype=\"http://schema.org/CreativeWork\">
        <span class=\"text\" itemprop=\"text\">“The world as we have created it is a process of our thinking. It cannot be changed without changing our thinking.”</span>
        <span>by <small class=\"author\" itemprop=\"author\">Albert Einstein</small>
        <a href=\"/author/Albert-Einstein\">(about)</a>
        </span>
        <div class=\"tags\">
            Tags:
            <meta class=\"keywords\" itemprop=\"keywords\" content=\"change,deep-thoughts,thinking,world\">   &gt; 
            
            <a class=\"tag\" href=\"/tag/change/page/1/\">change</a>
            
            <a class=\"tag\" href=\"/tag/deep-thoughts/page/1/\">deep-thoughts</a>
            
            <a class=\"tag\" href=\"/tag/thinking/page/1/\">thinking</a>
            
            <a class=\"tag\" href=\"/tag/world/page/1/\">world</a>
            
        </div>
    </div>"
  "<div class=\"quote\" itemscope=\"\" itemtype=\"http://schema.org/CreativeWork\">
        <span class=\"text\" itemprop=\"text\">“It is our choices, Harry, that show what we truly are, far more than our abilities.”</span>
        <span>by <small class=\"author\" itemprop=\"author\">J.K. Rowling</small>
        <a href=\"/author/J-K-Rowling\">(about)</a>
        </span>
        <div class=\"tags\">
            Tags:
            <meta class=\"keywords\" itemprop=\"keywords\" content=\"abilities,choices\">   &gt; 
            
            <a class=\"tag\" href=\"/tag/abilities/page/1/\">abilities</a>
            
            <a class=\"tag\" href=\"/tag/choices/page/1/\">choices</a>
            
        </div>
    </div>")
```

Now let's extract the first quote. Instead of this code in Python which sequentilly extracts quote node and then it's subelements:

```
>>> text = quote.css("span.text::text").get()
>>> text
'“The world as we have created it is a process of our thinking. It cannot be changed without changing our thinking.”'
>>> author = quote.css("small.author::text").get()
>>> author
'Albert Einstein'
```

We will use the power of functional approach and extract all needed data in a single pipeline using lQuery's form `(combine ...)`:

```
CL-USER> (lquery:$
           (initialize *response*)
           "div.quote"
           (function
            (lambda (nodes)
             (serapeum:take 2 nodes)))
           (combine
            (lquery:$1
              "span.text"
              (text))
            (lquery:$1
              "small.author"
              (text))))
#(("“The world as we have created it is a process of our thinking. It cannot be changed without changing our thinking.”"
   "Albert Einstein")
  ("“It is our choices, Harry, that show what we truly are, far more than our abilities.”"
   "J.K. Rowling"))
```

Note, this code we put after the `serapeum:take 2`:

```
(combine
 (lquery:$1
   "span.text"
   (text))
 (lquery:$1
   "small.author"
   (text))))
```

It allows us to extract two subelements of the `div.quote` node simultaneously, using function `combine`. These two pieces are combined into an array like:

```
#("“The world as we have created it is a process of our thinking. It cannot be changed without changing our thinking.”"
   "Albert Einstein")
```

But because of functional nature of lQuery, this `combine` operation is applied to all `div.quote` nodes on the page and we don't have to write explicit iteration loop.

After that, original Scrapy's tutorial shows us how to extract tags list for each quote:

```
>>> tags = quote.css("div.tags a.tag::text").getall()
>>> tags
['change', 'deep-thoughts', 'thinking', 'world']
```

but knowing how does `combine` work, we can just add another rule into the `combine` form:

```
CL-USER> (lquery:$
           (initialize *response*)
           "div.quote"
           (function
            (lambda (nodes)
             (serapeum:take 2 nodes)))
           (combine
            (lquery:$1
              "span.text"
              (text))
            (lquery:$1
              "small.author"
              (text))
            (lquery:$
              "div.tags a.tag"
              (text))))
#(("“The world as we have created it is a process of our thinking. It cannot be changed without changing our thinking.”"
   "Albert Einstein" #("change" "deep-thoughts" "thinking" "world"))
  ("“It is our choices, Harry, that show what we truly are, far more than our abilities.”"
   "J.K. Rowling" #("abilities" "choices")))
```

Note, for tags we are using `lquery:$` because there is a list of them.

Scrapy's tutorial creates a hash table for each quote, but ScrapyCL framework's pipeline operates on CLOS objects. So, we'll create a separate `QUOTE-ITEM` class:

```
CL-USER> (defclass quote-item ()
           ((text :initarg :text
                  :type string
                  :reader quote-text)
            (author :initarg :author
                    :type string
                    :reader quote-author)
            (tags :initarg :tags
                  :type (serapeum:soft-list-of string)
                  :reader quote-tags)))
#<STANDARD-CLASS COMMON-LISP-USER::QUOTE-ITEM>

CL-USER> (defmethod print-object ((obj quote-item) stream)
           (print-unreadable-object (obj stream :type t)
             (format stream "~A by ~A ~{#~A~^, ~}"
                     (quote-text obj)
                     (quote-author obj)
                     (quote-tags obj))))
```

Now we'll use `map-apply` to transform parsed data into these CLOS objects:

```
CL-USER> (lquery:$
           (initialize *response*)
           "div.quote"
           (function
            (lambda (nodes)
             (serapeum:take 2 nodes)))
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
                             :tags (coerce tags 'list)))))
#(#<QUOTE-ITEM “The world as we have created it is a process of our thinking. It cannot be changed without changing our thinking.” by Albert Einstein #change, #deep-thoughts, #thinking, #world>
  #<QUOTE-ITEM “It is our choices, Harry, that show what we truly are, far more than our abilities.” by J.K. Rowling #abilities, #choices>)
```

Put this piece of code into our method for SCRAPYCL:PROCESS generic-function:

```
CL-USER> (defmethod scrapycl:process ((spider quotes-spider)
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
```

And don't forget to remove this piece of code limiting the number of processed quotes:

```
(function
   (lambda (nodes)
     (serapeum:take 2 nodes)))
```

we needed it only for a debug purpose.

Now start our scraper:

```
CL-USER> (scrapycl:start (make-instance 'quotes-spider) :wait t)
(#<QUOTE-ITEM “Life is what happens to us while we are making other plans.” by Allen Saunders #fate, #life, #misattributed-john-lennon, #planning, #plans>
 #<QUOTE-ITEM “Good friends, good books, and a sleepy conscience: this is the ideal life.” by Mark Twain #books, #contentment, #friends, #friendship, #life>
...
```

As you can see, by default it returns a list of all items on the page, but in real world you will want this data to be saved or processed. In the next part we'll see how to do this.

"""
  )


(defsection @storing-data (:title "Storing the Scraped Data")
  """
Scrapy's tutorial shows this command as example on how to save scraped data to a json file:

```
scrapy crawl quotes -O quotes.json
```

With ScrapyCL we can do the similar but with OUTPUT argument to the SCRAPYCL:START generic-function and SCRAPYCL:JSON-LINES function:

```
CL-USER> (scrapycl:start (make-instance 'quotes-spider)
                         :wait t
                         :output (scrapycl:json-lines #P"items.json"))
```

And content of `items.json` file will look like:

```json
{"text":"“A day without sunshine is like, you know, night.”","author":"Steve Martin","tags":["humor","obvious","simile"]}
{"text":"“A woman is like a tea bag; you never know how strong it is until it's in hot water.”","author":"Eleanor Roosevelt","tags":["misattributed-eleanor-roosevelt"]}
```

Each object is on it's own line in a [JsonLines](https://jsonlines.org/) format. If you want to get a JSON file with a list, then use `SCRAPYCL:JSON-LIST` instead. Or use `SCRAPYCL:JSON-DICT` to get a file with JSON object. But beware, these two outputs can't work in :APPEND mode.


### Custom processing

Actually, OUTPUT argument accepts any lisp function. The only requirements are:

- This function should accept a single argument. The objects for which there is no specialized method of SCRAPYCL:PROCESS generic-function will be passed into this function.
- It should accept SCRAPYCL:STOP-OUTPUT symbol and flush buffer, closing a file or a transaction, because this symbol is sent when all hrefs were processed and there is no more data to process.

"""
  )


(defsection @following-links (:title "Following the Links")
  """
In Scrapy framework for following links you should yield a request object like this:

```
yield scrapy.Request(next_page, callback=self.parse)
```

With ScrapyCL to follow links, you need to return new request objects from your method for SCRAPYCL:PROCESS generic-function. And because you are returning an object of a customized request class, you can add more data slots to the request to make this additional data available during request processing. For example, such data might include a parent category of the page or an some piece of data available on other page.

Scrapy's test site contains quotes and their authors. Let's make our scraper parse not only quotes but also their authors. See `tutorial/step3.lisp` file for the full code for this part.

First, we need to add an AUTHOR-ITEM class:

```
CL-USER> (defclass author-item ()
           ((name :initarg :name
                  :type string
                  :reader author-name)
            (birthday :initarg :birthday
                      :type string
                      :reader author-birthday)
            (bio :initarg :bio
                 :type string
                 :reader author-bio)))
#<STANDARD-CLASS COMMON-LISP-USER::AUTHOR-ITEM>

CL-USER> (defmethod print-object ((obj author-item) stream)
           (print-unreadable-object (obj stream :type t)
             (format stream "~A"
                     (author-name obj))))
#<STANDARD-METHOD COMMON-LISP:PRINT-OBJECT (AUTHOR-ITEM T) {1003CD8723}>
```

Now let's make our spider do follow links leading to the next page and to the authors pages.

Here is how we can extract the link to the next page:

```
CL-USER> (lquery:$1
           (initialize *response*)
           "ul.pager a"
           (attr "href"))
"/page/2/"
```

But we need an absolute URL for request. So we have to merge this path with a base URL.

SCRAPYCL:FETCH generic-function returns current page's real URL as a second value. Also, ScrapyCL provides a MERGE-WITH-URL lquery form. Together they can be used like this:

```
CL-USER> (multiple-value-bind (response base-url)
             (scrapycl:fetch (make-instance 'quotes-spider)
                             (make-instance 'quotes-page-request
                                            :url "https://quotes.toscrape.com/"))
           (lquery:$1
             (initialize response)
             "ul.pager a"
             (attr "href")
             (merge-url-with base-url)))
"https://quotes.toscrape.com/page/2/"
```

It is better to use URL returned by SCRAPYCL:FETCH generic-function because of these two reasons:

- This URL can differ from original request URL because site might redirect request to the other page.
- Href attributes on the page can be relative, like `../quotes/page/1` and will not work if you'll hardcode base url.

Let's figure out which author pages should be followed. Original Scrapy tutorial uses this CSS selector `.author + a`, but lquery does not support `+` selector. To find a siblings of `.author` element but we can use `NEXT` form to select subsequent element following the `author` node:

```
CL-USER> (multiple-value-bind (response base-url)
             (scrapycl:fetch (make-instance 'quotes-spider)
                             (make-instance 'quotes-page-request
                                            :url "https://quotes.toscrape.com/"))
           (lquery:$
             (initialize response)
             ".author"
             (next "a")
             (attr "href")
             (merge-url-with base-url)))
#("https://quotes.toscrape.com/author/Albert-Einstein"
  "https://quotes.toscrape.com/author/J-K-Rowling"
  "https://quotes.toscrape.com/author/Albert-Einstein"
  "https://quotes.toscrape.com/author/Jane-Austen"
  "https://quotes.toscrape.com/author/Marilyn-Monroe"
  "https://quotes.toscrape.com/author/Albert-Einstein"
  "https://quotes.toscrape.com/author/Andre-Gide"
  "https://quotes.toscrape.com/author/Thomas-A-Edison"
  "https://quotes.toscrape.com/author/Eleanor-Roosevelt"
  "https://quotes.toscrape.com/author/Steve-Martin")
```

Ok, now, when we have a URLs to follow, let's modify our processing function to return them as new requests:

```
CL-USER> (defclass author-page-request (scrapycl:request)
           ())
#<STANDARD-CLASS COMMON-LISP-USER::AUTHOR-PAGE-REQUEST>


CL-USER> (defmethod scrapycl:process ((spider quotes-spider)
                                      (request quotes-page-request))
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
                       (make-instance 'quotes-page-request
                                      :url next-page-url))))))
#<STANDARD-METHOD SCRAPYCL:PROCESS (QUOTES-SPIDER QUOTES-PAGE-REQUEST) {1005E33C53}>
```

We return objects of three types from this processing method: quote-items, quotes-page-requests and author-page-requests.

Now if we will run our scraper, then we'll see it walks only through quotes pages and ignores author pages:

```
CL-USER> (scrapycl:start (make-instance 'quotes-spider)
                         :wait t
                         :output (scrapycl:json-lines #P"items.json"))
 <INFO> [19:25:21] cl-user (process quotes-spider quotes-page-request) -
  Fetched BASE-URL: #<QURI.URI.HTTP:URI-HTTPS https://quotes.toscrape.com/page/1/>
  
 <INFO> [19:25:21] cl-user (process quotes-spider quotes-page-request) -
  Fetched BASE-URL: #<QURI.URI.HTTP:URI-HTTPS https://quotes.toscrape.com/page/2/>
  
NIL

```

But in the file `items.json` you might see interesting records:

```
{"url":"https://quotes.toscrape.com/author/Steve-Martin"}
{"url":"https://quotes.toscrape.com/author/Eleanor-Roosevelt"}
{"url":"https://quotes.toscrape.com/author/Thomas-A-Edison"}
{"url":"https://quotes.toscrape.com/author/Andre-Gide"}
...
```

This is because we forgot to define a processing method for our class AUTHOR-PAGE-REQUEST. ScrapyCL sees objects without a processing method and decides these are final objects to be serialized to the output. Let's write a method to extract information about authors as well.

Here I've just translated these Python rules:


```
def parse_author(self, response):
    def extract_with_css(query):
        return response.css(query).get(default="").strip()

    yield {
        "name": extract_with_css("h3.author-title::text"),
        "birthdate": extract_with_css(".author-born-date::text"),
        "bio": extract_with_css(".author-description::text"),
    }
```

into the lquery DSL:

```
CL-USER> (multiple-value-bind (response)
             (scrapycl:fetch (make-instance 'quotes-spider)
                             (make-instance 'author-page-request
                                            :url "https://quotes.toscrape.com/author/Thomas-A-Edison"))
           (lquery:$1
             (initialize response)
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
                (map #'str:trim)))))

("Thomas A. Edison" "February 11, 1847"
 "Thomas Alva Edison was an American inventor, scientist and businessman who developed many devices that greatly influenced life around the world, including the phonograph, the motion picture camera, and a long-lasting, practical electric light bulb. Dubbed \"The Wizard of Menlo Park\" (now Edison, New Jersey) by a newspaper reporter, he was one of the first inventors to apply the principles of mass production and large teamwork to the process of invention, and therefore is often credited with the creation of the first industrial research laboratory.Edison is considered one of the most prolific inventors in history, holding 1,093 U.S. patents in his name, as well as many patents in the United Kingdom, France and Germany. He is credited with numerous inventions that contributed to mass communication and, in particular, telecommunications. His advanced work in these fields was an outgrowth of his early career as a telegraph operator. Edison originated the concept and implementation of electric-power generation and distribution to homes, businesses, and factories – a crucial development in the modern industrialized world. His first power station was on Manhattan Island, New York.")
```

And here is the full processing method which will return an author object:


```
CL-USER> (defmethod scrapycl:process ((spider quotes-spider)
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
                                 
#<STANDARD-METHOD SCRAPYCL:PROCESS (QUOTES-SPIDER AUTHOR-PAGE-REQUEST) {10020C9733}>
```


Now, if you start the our spider again, you'll get quotes and authors mixed in the same `items.json` file.

But how to put different kinds of object into a different output files?

This is easy - just use a SCRAPYCL:TYPED-OUTPUT function. This kind of output redirects items into another outputs depending on their type.

To separate output into `quotes.json` and `authors.json`, execute our scraper like this:

```
CL-USER> (scrapycl:start (make-instance 'quotes-spider)
                         :wait t
                         :output (scrapycl:typed-output
                                  (list (cons 'quote-item
                                              (scrapycl:json-lines #P"quotes.json"))
                                        (cons 'author-item
                                              (scrapycl:json-lines #P"authors.json")))))
 <INFO> [19:29:43] cl-user (process quotes-spider quotes-page-request) -
  Fetched BASE-URL: #<QURI.URI.HTTP:URI-HTTPS https://quotes.toscrape.com/page/2/>
  
 <INFO> [19:29:43] cl-user (process quotes-spider quotes-page-request) -
  Fetched BASE-URL: #<QURI.URI.HTTP:URI-HTTPS https://quotes.toscrape.com/page/1/>

 ...
  
 <INFO> [19:29:48] cl-user (process quotes-spider quotes-page-request) -
  Fetched BASE-URL: #<QURI.URI.HTTP:URI-HTTPS https://quotes.toscrape.com/page/1/>
  
NIL
```

It will save each type of item in a separate file.

I hope this little introduction will urge you to try ScrapyCL for writing your own data scrapers! Feel free to share your ideas on the project's [discussions page](https://github.com/40ants/scrapycl/discussions).
"""
  )
