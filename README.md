<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# scrapycl - The web scraping framework for writing crawlers in Common Lisp.

<a id="scrapycl-asdf-system-details"></a>

## SCRAPYCL ASDF System Details

* Version: 0.1.0
* Description: The web scraping framework for writing crawlers in Common Lisp.
* Licence: Unlicense
* Author: Alexander Artemenko <svetlyak.40wt@gmail.com>
* Homepage: [https://40ants.com/scrapycl/][174b]
* Bug tracker: [https://github.com/40ants/scrapycl/issues][1e8e]
* Source control: [GIT][66a8]
* Depends on: [40ants-doc][2c00], [alexandria][8236], [bordeaux-threads][3dbf], [dexador][8347], [log4cl][7f8b], [log4cl-extras][691c], [lquery][557a], [quri][2103], [serapeum][c41d], [spinneret][8175], [str][ef7f]

[![](https://github-actions.40ants.com/40ants/scrapycl/matrix.svg?only=ci.run-tests)][5d38]

![](http://quickdocs.org/badge/scrapycl.svg)

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :scrapycl)
```
<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

`TODO`: Write a library description. Put some examples here.

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-40SCRAPYCL-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### SCRAPYCL

<a id="x-28-23A-28-288-29-20BASE-CHAR-20-2E-20-22SCRAPYCL-22-29-20PACKAGE-29"></a>

#### [package](caca) `scrapycl`

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-7C-40SCRAPYCL-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-40SCRAPYCL-24REQUEST-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### REQUEST

<a id="x-28SCRAPYCL-3AREQUEST-20CLASS-29"></a>

###### [class](7bc4) `scrapycl:request` ()

**Readers**

<a id="x-28SCRAPYCL-2FREQUEST-3AREQUEST-URL-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20SCRAPYCL-3AREQUEST-29-29"></a>

###### [reader](8391) `scrapycl/request:request-url` (request) (:URL = (ERROR "Please, provide :URL argument."))

`URL` to fetch data from.

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-40SCRAPYCL-24SPIDER-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### SPIDER

<a id="x-28SCRAPYCL-3ASPIDER-20CLASS-29"></a>

###### [class](8ccb) `scrapycl:spider` ()

**Readers**

<a id="x-28SCRAPYCL-2FSPIDER-3A-3A-25INITIAL-REQUESTS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20SCRAPYCL-3ASPIDER-29-29"></a>

###### [reader](6531) `scrapycl/spider::%initial-requests` (spider) (:initial-requests = nil)

<a id="x-28SCRAPYCL-2FSPIDER-3A-3A-25SPIDER-QUEUE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20SCRAPYCL-3ASPIDER-29-29"></a>

###### [reader](8336) `scrapycl/spider::%spider-queue` (spider) (= nil)

<a id="x-28SCRAPYCL-2FSPIDER-3A-3A-25SPIDER-QUEUE-LOCK-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20SCRAPYCL-3ASPIDER-29-29"></a>

###### [reader](af53) `scrapycl/spider::%spider-queue-lock` (spider) (= (MAKE-LOCK :NAME "Scrapycl Queue Lock"))

<a id="x-28SCRAPYCL-2FSPIDER-3A-3A-25SPIDER-THREAD-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20SCRAPYCL-3ASPIDER-29-29"></a>

###### [reader](4003) `scrapycl/spider::%spider-thread` (spider) (= nil)

**Accessors**

<a id="x-28SCRAPYCL-2FSPIDER-3A-3A-25SPIDER-QUEUE-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20SCRAPYCL-3ASPIDER-29-29"></a>

###### [accessor](8336) `scrapycl/spider::%spider-queue` (spider) (= nil)

<a id="x-28SCRAPYCL-2FSPIDER-3A-3A-25SPIDER-QUEUE-LOCK-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20SCRAPYCL-3ASPIDER-29-29"></a>

###### [accessor](af53) `scrapycl/spider::%spider-queue-lock` (spider) (= (MAKE-LOCK :NAME "Scrapycl Queue Lock"))

<a id="x-28SCRAPYCL-2FSPIDER-3A-3A-25SPIDER-THREAD-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20SCRAPYCL-3ASPIDER-29-29"></a>

###### [accessor](4003) `scrapycl/spider::%spider-thread` (spider) (= nil)

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-7C-40SCRAPYCL-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28SCRAPYCL-3AFETCH-20GENERIC-FUNCTION-29"></a>

##### [generic-function](20d0) `scrapycl:fetch` spider request &key max-redirects timeout custom-headers

<a id="x-28SCRAPYCL-3APROCESS-20GENERIC-FUNCTION-29"></a>

##### [generic-function](efc3) `scrapycl:process` spider object

<a id="x-28SCRAPYCL-3ASTART-20GENERIC-FUNCTION-29"></a>

##### [generic-function](1d4c) `scrapycl:start` spider &key wait output &allow-other-keys

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-7C-40SCRAPYCL-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28SCRAPYCL-3AENQUEUE-20FUNCTION-29"></a>

##### [function](e0f5) `scrapycl:enqueue` spider object &key (output-func nil scrapycl/engine::output-func-p)

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-40SCRAPYCL-2FERRORS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### SCRAPYCL/ERRORS

<a id="x-28-23A-28-2815-29-20BASE-CHAR-20-2E-20-22SCRAPYCL-2FERRORS-22-29-20PACKAGE-29"></a>

#### [package](fa3c) `scrapycl/errors`

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-7C-40SCRAPYCL-2FERRORS-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-40SCRAPYCL-2FERRORS-24FETCH-ERROR-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### FETCH-ERROR

<a id="x-28SCRAPYCL-2FERRORS-3AFETCH-ERROR-20CONDITION-29"></a>

###### [condition](ece9) `scrapycl/errors:fetch-error` (scrapycl-error)

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-40SCRAPYCL-2FERRORS-24SCRAPYCL-ERROR-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### SCRAPYCL-ERROR

<a id="x-28SCRAPYCL-2FERRORS-3ASCRAPYCL-ERROR-20CONDITION-29"></a>

###### [condition](eb68) `scrapycl/errors:scrapycl-error` (error)

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-40SCRAPYCL-2FREQUEST-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### SCRAPYCL/REQUEST

<a id="x-28-23A-28-2816-29-20BASE-CHAR-20-2E-20-22SCRAPYCL-2FREQUEST-22-29-20PACKAGE-29"></a>

#### [package](27de) `scrapycl/request`

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-7C-40SCRAPYCL-2FREQUEST-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28SCRAPYCL-2FREQUEST-3AREQUEST-URL-20GENERIC-FUNCTION-29"></a>

##### [generic-function] `scrapycl/request:request-url` object

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-40SCRAPYCL-2FUTILS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### SCRAPYCL/UTILS

<a id="x-28-23A-28-2814-29-20BASE-CHAR-20-2E-20-22SCRAPYCL-2FUTILS-22-29-20PACKAGE-29"></a>

#### [package](5511) `scrapycl/utils`

<a id="x-28SCRAPYCL-DOCS-2FINDEX-3A-3A-7C-40SCRAPYCL-2FUTILS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28SCRAPYCL-2FUTILS-3APREVIEW-20FUNCTION-29"></a>

##### [function](147a) `scrapycl/utils:preview` nodes


[174b]: https://40ants.com/scrapycl/
[66a8]: https://github.com/40ants/scrapycl
[5d38]: https://github.com/40ants/scrapycl/actions
[caca]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/core.lisp#L1
[20d0]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/downloader.lisp#L16
[e0f5]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/engine.lisp#L106
[efc3]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/engine.lisp#L129
[fa3c]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/errors.lisp#L1
[eb68]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/errors.lisp#L10
[ece9]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/errors.lisp#L14
[27de]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/request.lisp#L1
[7bc4]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/request.lisp#L10
[8391]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/request.lisp#L11
[8ccb]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/spider.lisp#L24
[8336]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/spider.lisp#L25
[af53]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/spider.lisp#L27
[4003]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/spider.lisp#L29
[6531]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/spider.lisp#L31
[1d4c]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/spider.lisp#L37
[5511]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/utils.lisp#L1
[147a]: https://github.com/40ants/scrapycl/blob/49257e64a9f86ff94a98564e5a738fcd8f121723/src/utils.lisp#L80
[1e8e]: https://github.com/40ants/scrapycl/issues
[2c00]: https://quickdocs.org/40ants-doc
[8236]: https://quickdocs.org/alexandria
[3dbf]: https://quickdocs.org/bordeaux-threads
[8347]: https://quickdocs.org/dexador
[7f8b]: https://quickdocs.org/log4cl
[691c]: https://quickdocs.org/log4cl-extras
[557a]: https://quickdocs.org/lquery
[2103]: https://quickdocs.org/quri
[c41d]: https://quickdocs.org/serapeum
[8175]: https://quickdocs.org/spinneret
[ef7f]: https://quickdocs.org/str

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
