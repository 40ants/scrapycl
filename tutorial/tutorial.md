In this tutorial we'll train our parsing skill on this toy site: https://quotes.toscrape.com/.
We will follow [Scrapy's tutorial](https://docs.scrapy.org/en/latest/intro/tutorial.html) and see if we can get all the data using ScrapyCL.


Instead of:

```
scrapy shell "https://quotes.toscrape.com/page/1/"
```


```
SCRAPYCL/TUTORIAL/STEP1> (defparameter *response*
                           (scrapycl:fetch (make-instance 'step1)
                                           (make-instance 'quotes-page-request
                                                          :url "https://quotes.toscrape.com/page/1/")))
*RESPONSE*
SCRAPYCL/TUTORIAL/STEP1> *response*
"<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="UTF-8">
	<title>Quotes to Scrape</title>
    <link rel="stylesheet" href="/static/bootstrap.min.css">
    <link rel="stylesheet" href="/static/m...[sly-elided string of length 11011]"

```

Python:

```
>>> response.css("title::text").getall()
['Quotes to Scrape']
```

Lisp:


```
SCRAPYCL/TUTORIAL/STEP1> (lquery:$
                           (initialize *response*)
                           "title"
                           (text))
#("Quotes to Scrape")
```

Python:

```
response.css("title").getall()
['<title>Quotes to Scrape</title>']
```

Lisp:

```
SCRAPYCL/TUTORIAL/STEP1> (lquery:$
                           (initialize *response*)
                           "title")
#(#<PLUMP-DOM:ELEMENT title {1007348DF3}>)
SCRAPYCL/TUTORIAL/STEP1> (lquery:$
                           (initialize *response*)
                           "title"
                           (serialize))
#("<title>Quotes to Scrape</title>")
```

To get only a single item. Python:

```
>>> response.css("title::text").get()
'Quotes to Scrape'
```

In Lisp use `lquery:$1` instead of `lquery:$`:

```
SCRAPYCL/TUTORIAL/STEP1> (lquery:$1
                           (initialize *response*)
                           "title"
                           (text))
"Quotes to Scrape"
```


As an alternative, you could’ve written in Python:

```
>>> response.css("title::text")[0].get()
'Quotes to Scrape'
```

In Lisp:

```
SCRAPYCL/TUTORIAL/STEP1> (lquery:$
                           (initialize *response*)
                           "title")
#(#<PLUMP-DOM:ELEMENT title {1009300673}>)

SCRAPYCL/TUTORIAL/STEP1> (elt * 0)
#<PLUMP-DOM:ELEMENT title {1009300673}>

SCRAPYCL/TUTORIAL/STEP1> (plump:text *)
"Quotes to Scrape"
```

There is no analogue to `re`:

```
>>> response.css("title::text").re(r"Quotes.*")
['Quotes to Scrape']
```

but you can use a filter function;

```
SCRAPYCL/TUTORIAL/STEP1> (lquery:$
                           (initialize *response*)
                           "title"
                           (text)
                           (filter (lambda (text)
                                     (cl-ppcre:scan "Quotes.*" text))))
#("Quotes to Scrape")
```


```
>>> response.css("title::text").re(r"Q\w+")
['Quotes']
```

Lisp:

```
SCRAPYCL/TUTORIAL/STEP1> (lquery:$
                           (initialize *response*)
                           "title"
                           (text)
                           (each (lambda (text)
                                   (cl-ppcre:scan-to-strings "Q\\w+" text))
                                 :replace t))
#("Quotes")
```


Python:

```
>>> response.css("title::text").re(r"(\w+) to (\w+)")
['Quotes', 'Scrape']
```

Lisp:


```
SCRAPYCL/TUTORIAL/STEP1> (lquery:$1
                           (initialize *response*)
                           "title"
                           (text)
                           (map (lambda (text)
                                  (nth-value 1
                                             (cl-ppcre:scan-to-strings "(\\w+) to (\\w+)" text)))))
#("Quotes" "Scrape")
```


## Extracting quotes and authors (step2)


```
SCRAPYCL/TUTORIAL/STEP2> (defparameter *response*
                           (scrapycl:fetch (make-instance 'step2)
                                           (make-instance 'index-page-request
                                                          :url "https://quotes.toscrape.com/")))
*RESPONSE*
```

Now lets extract quotes. Instead of this Python code:

```
response.css("div.quote")
[<Selector query="descendant-or-self::div[@class and contains(concat(' ', normalize-space(@class), ' '), ' quote ')]" data='<div class="quote" itemscope itemtype...'>,
<Selector query="descendant-or-self::div[@class and contains(concat(' ', normalize-space(@class), ' '), ' quote ')]" data='<div class="quote" itemscope itemtype...'>,
...]
```

we can do this in Lisp:

```
SCRAPYCL/TUTORIAL/STEP2> (lquery:$
                           (initialize *response*)
                           "div.quote")
#(#<PLUMP-DOM:ELEMENT div {100982FCE3}> #<PLUMP-DOM:ELEMENT div {1009849CB3}>
  #<PLUMP-DOM:ELEMENT div {100984ABA3}> #<PLUMP-DOM:ELEMENT div {100984C003}>
  #<PLUMP-DOM:ELEMENT div {100984D293}> #<PLUMP-DOM:ELEMENT div {100984E183}>
  #<PLUMP-DOM:ELEMENT div {100984F243}> #<PLUMP-DOM:ELEMENT div {1009860F43}>
  #<PLUMP-DOM:ELEMENT div {10098621D3}> #<PLUMP-DOM:ELEMENT div {1009862EF3}>)
```

Here is how we can to limit the number of items to not clutter the REPL:

```
SCRAPYCL/TUTORIAL/STEP2> (lquery:$
                           (initialize *response*)
                           "div.quote"
                           (function
                            (lambda (nodes)
                             (serapeum:take 2 nodes))))
#(#<PLUMP-DOM:ELEMENT div {10098B3513}> #<PLUMP-DOM:ELEMENT div {10098B47A3}>)
```

And preview the extracted pieces:


```
SCRAPYCL/TUTORIAL/STEP2> (lquery:$
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

Now let's extract the first quote. Instead of this code in Python:

```
>>> text = quote.css("span.text::text").get()
>>> text
'“The world as we have created it is a process of our thinking. It cannot be changed without changing our thinking.”'
>>> author = quote.css("small.author::text").get()
>>> author
'Albert Einstein'
```

we can do:

```
SCRAPYCL/TUTORIAL/STEP2> (lquery:$
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

It allows us to extract two items from the `div.quote` node simultaneously, using function `combine`. These two pieces are combined into an array like:

```
#("“The world as we have created it is a process of our thinking. It cannot be changed without changing our thinking.”"
   "Albert Einstein")
```

But because of functional nature of lquery, this `combine` operation is applied to all `div.quote` nodes.

Next, original Scrapy's tutorial shows how to extract tags list for each quote:

```
>>> tags = quote.css("div.tags a.tag::text").getall()
>>> tags
['change', 'deep-thoughts', 'thinking', 'world']
```

but knowing how does `combine` work, we can just add another rule into the `combine` form:

```
SCRAPYCL/TUTORIAL/STEP2> (lquery:$
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

Scrapy's tutorial creates a hash table for each quote, but ScrapyCL prefers to work with CLOS objects. So, we'll create a separate
`QUOTE` class:

```
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
```

Now we'll use `map-apply` to transform parsed data into the CLOS objects:

```
SCRAPYCL/TUTORIAL/STEP2> (lquery:$
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

Put this piece of code into a PROCESS method:

```
(defmethod scrapycl:process ((spider step2)
                             (request index-page-request))
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

and don't forget to remove this piece with filter call:

```
(function
   (lambda (nodes)
     (serapeum:take 2 nodes)))
```

we needed it only for a debug purpose.

Now start our scraper:

```
SCRAPYCL/TUTORIAL/STEP2> (scrapycl:start (make-instance 'step2) :wait t)
(#<QUOTE-ITEM “A day without sunshine is like, you know, night.” by Steve Martin #humor, #obvious, #simile>
 #<QUOTE-ITEM “A woman is like a tea bag; you never know how strong it is until it's in hot water.” by Eleanor Roosevelt #misattributed-eleanor-roosevelt>
 #<QUOTE-ITEM “I have not failed. I've just found 10,000 ways that won't work.” by Thomas A. Edison #edison, #failure, #inspirational, #paraphrased>
 #<QUOTE-ITEM “It is better to be hated for what you are than to be loved for what you are not.” by André Gide #life, #love>
 #<QUOTE-ITEM “Try not to become a man of success. Rather become a man of value.” by Albert Einstein #adulthood, #success, #value>
 #<QUOTE-ITEM “Imperfection is beauty, madness is genius and it's better to be absolutely ridiculous than absolutely boring.” by Marilyn Monroe #be-yourself, #inspirational>
 #<QUOTE-ITEM “The person, be it gentleman or lady, who has not pleasure in a good novel, must be intolerably stupid.” by Jane Austen #aliteracy, #books, #classic, #humor>
 #<QUOTE-ITEM “There are only two ways to live your life. One is as though nothing is a miracle. The other is as though everything is a miracle.” by Albert Einstein #inspirational, #life, #live, #miracle, #miracles>
 #<QUOTE-ITEM “It is our choices, Harry, that show what we truly are, far more than our abilities.” by J.K. Rowling #abilities, #choices>
 #<QUOTE-ITEM “The world as we have created it is a process of our thinking. It cannot be changed without changing our thinking.” by Albert Einstein #change, #deep-thoughts, #thinking, #world>)
```

As you can see, by default it returns a list of all items on the page.

## Storing the scraped data¶

Scrapy's tutorial shows this command as example on how to save scraped data to a json file:

```
scrapy crawl quotes -O quotes.json
```

With ScrapyCL we can do the similar but with OUTPUT argument to the START generic-function:

```
SCRAPYCL/TUTORIAL/STEP2> (scrapycl:start (make-instance 'step2)
                                         :wait t
                                         :output (scrapycl/output/json:json-lines #P"items.json"))
```

And content of `items.json` file will look like:

```json
{"text":"“A day without sunshine is like, you know, night.”","author":"Steve Martin","tags":["humor","obvious","simile"]}
{"text":"“A woman is like a tea bag; you never know how strong it is until it's in hot water.”","author":"Eleanor Roosevelt","tags":["misattributed-eleanor-roosevelt"]}
```

Each object is on it's own line in a [JsonLines](https://jsonlines.org/) format. If you want to get a JSON file with a list, then use `SCRAPYCL/OUTPUT/JSON:JSON-LIST` instead. Or use `SCRAPYCL/OUTPUT/JSON:JSON-DICT` to get a file with JSON object. But beware, these two outputs can't work in :append mode.


### Custom processing

Actually, :OUTPUT argument accepts any lisp function. The only requirements are:

- it should accept a single argument. The objects for which there is no specialized method of PROCESS generic-function will be passed into this function.
- it should accept SCRAPYCL/OUTPUT:STOP-OUTPUT symbol and flush buffer, close file or transaction, because this symbol is sent when all links were processed and there is no more data to process.


## Following links

Scrapy's tutorial shows that for following links you should yield a request like this:

```
yield scrapy.Request(next_page, callback=self.parse)
```

With ScrapyCL to follow links, just return new request objects from your method for PROCESS generic-function. And because you are returning an object of a customized request class, you can add more data slots to the request to make this additional data available during request processing. For example, such data might include a parent category of the page or an some piece of data available on other page.

Scrapy's test site contains quotes and their authors. Let's make our scraper parse not only quotes but also their authors and to bind them by adding `author_id` to the quote. For simplicity I'll use a hash-table instead of the real database. See `tutorial/step3.lisp` file for full code for this part.

First, we need to add an author-item class:

```
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
            
```

Now let's make our spider follow links leading to the next page and to authors pages:

```
SCRAPYCL/TUTORIAL/STEP3> (defparameter *response*
                           (scrapycl:fetch (make-instance 'quotes-spider)
                                           (make-instance 'index-page-request
                                                          :url "https://quotes.toscrape.com/")))

SCRAPYCL/TUTORIAL/STEP3> (lquery:$1
                           (initialize *response*)
                           "ul.pager a"
                           (attr "href"))
"/page/2/"
```

To make absolute URL for request we need to merge this path with a base URL. ScrapyCL's FETCH function returns current pages real URL as the second value. Also, it provides as MERGE-WITH-URL lquery form. Together they can be used like this:

```
SCRAPYCL/TUTORIAL/STEP3> (multiple-value-bind (response base-url)
                             (scrapycl:fetch (make-instance 'quotes-spider)
                                             (make-instance 'index-page-request
                                                            :url "https://quotes.toscrape.com/"))
                           (lquery:$1
                             (initialize response)
                             "ul.pager a"
                             (attr "href")
                             (merge-url-with base-url)))
"https://quotes.toscrape.com/page/2/"
```

It is better to use URL returned by fetch because of these two reasons:

- this URL can differ from original request URL because site might redirect request to the other page.
- href attributes on the page can be relative, like `../quotes/page/1` and will not work if you hardcode base url.

Let's figure out which author pages should be followed. Original Scrapy tutorial uses this CSS selector `.author + a`, but lquery does not support it. To find a siblings of `.author` element but we can use NEXT form to select subsequent element following the `author` node:

```
SCRAPYCL/TUTORIAL/STEP3> (multiple-value-bind (response base-url)
                             (scrapycl:fetch (make-instance 'quotes-spider)
                                             (make-instance 'index-page-request
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

Ok, now, when we have a URLs to follow, let modify our processing function to return them as new requests:

```
(defclass author-page-request (scrapycl:request)
  ())
  

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
```

If we do just that and then run our scraper, then we'll see it walks only through quotes pages and ignores author pages:

```
SCRAPYCL/TUTORIAL/STEP3> (scrapycl:start (make-instance 'quotes-spider)
                                         :wait t
                                         :output (scrapycl/output/json:json-lines #P"items.json"))
 <INFO> [12:50:57] scrapycl/tutorial/step3 slime0lIMfb (process quotes-spider index-page-request) -
  Fetched BASE-URL: #<QURI.URI.HTTP:URI-HTTPS https://quotes.toscrape.com/> 
 <INFO> [12:50:57] scrapycl/tutorial/step3 slime0lIMfb (process quotes-spider index-page-request) -
  Fetched BASE-URL: #<QURI.URI.HTTP:URI-HTTPS https://quotes.toscrape.com/page/2/>
  
 <INFO> [12:50:57] scrapycl/tutorial/step3 slime0lIMfb (process quotes-spider index-page-request) -
  Fetched BASE-URL: #<QURI.URI.HTTP:URI-HTTPS https://quotes.toscrape.com/page/1/>
```

But in the file `items.json` you might see interesting records:

```
{"url":"https://quotes.toscrape.com/author/Steve-Martin"}
{"url":"https://quotes.toscrape.com/author/Eleanor-Roosevelt"}
{"url":"https://quotes.toscrape.com/author/Thomas-A-Edison"}
{"url":"https://quotes.toscrape.com/author/Andre-Gide"}
...
```

This is because we forgot to define a PROCESS method for our class AUTHOR-PAGE-REQUEST. ScrapyCL sees objects without a PROCESS method and decides these are final objects to be serialized to the output. Let's write a method to extract information about authors as well.

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
SCRAPYCL/TUTORIAL/STEP3> (multiple-value-bind (response)
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

And here is the full PROCESS method which will return an author object:


```
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
```


Now, if you start the spider again, you'll get quotes and authors mixed in the same `items.json` file. But how to put different kinds of object into a different output files? This is easy - just use a TYPED-OUTPUT. This kind of output redirects items into another outputs depending on their type.

To separate output into `quotes.json` and `authors.json`, execute scraper like this:

```
SCRAPYCL/TUTORIAL/STEP3> (scrapycl:start (make-instance 'quotes-spider)
                                         :wait t
                                         :output (scrapycl/output/typed:typed-output
                                                  (list (cons 'quote-item
                                                              (scrapycl/output/json:json-lines #P"quotes.json"))
                                                        (cons 'author-item
                                                              (scrapycl/output/json:json-lines #P"authors.json")))))
```
