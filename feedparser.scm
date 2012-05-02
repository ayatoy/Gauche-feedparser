(define-module feedparser
  (use gauche.charconv)
  (use rfc.uri)
  (use rfc.http)
  (use rfc.zlib)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use util.list)
  (export <feedparser-error>
          feedparser-error?
          feedparser-from-port
          feedparser-from-string
          feedparser-from-file
          feedparser))
(select-module feedparser)

;;;; conditon

(define-condition-type <feedparser-error> <error>
  feedparser-error?)

;;;; constant

(define-constant FEED_NAMESPACE
  '((rss1.0  . "http://purl.org/rss/1.0/")
    (rdf     . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (atom1.0 . "http://www.w3.org/2005/Atom")
    (atom0.3 . "http://purl.org/atom/ns#")))

(define-constant FEED_USER_AGENT
  "Mozilla/5.0 (compatible; feedparser/0.0;)")

;;;; rss1.0

(define (feed-rss1.0? sxml)
  (and ((if-sxpath '(rdf:RDF rss1.0:channel rss1.0:title)) sxml)
       ((if-sxpath '(rdf:RDF rss1.0:channel rss1.0:link)) sxml)
       ((if-sxpath '(rdf:RDF rss1.0:channel rss1.0:description)) sxml)
       ((if-sxpath '(rdf:RDF rss1.0:channel rss1.0:items)) sxml)
       #t))

(define feed-rss1.0-title
  (if-car-sxpath '(rdf:RDF rss1.0:channel (rss1.0:title 1) *text*)))

(define feed-rss1.0-link
  (if-car-sxpath '(rdf:RDF rss1.0:channel (rss1.0:link 1) *text*)))

(define feed-rss1.0-entries
  (sxpath '(rdf:RDF rss1.0:item)))

(define feed-rss1.0-entry-title
  (if-car-sxpath '((rss1.0:title 1) *text*)))

(define feed-rss1.0-entry-link
  (if-car-sxpath '((rss1.0:link 1) *text*)))

;;;; rss2.0

(define (feed-rss2.0? sxml)
  (and ((if-sxpath '(rss channel title)) sxml)
       ((if-sxpath '(rss channel link)) sxml)
       ((if-sxpath '(rss channel description)) sxml)
       ((if-sxpath '(rss channel item)) sxml)
       #t))

(define feed-rss2.0-title
  (if-car-sxpath '(rss channel (title 1) *text*)))

(define feed-rss2.0-link
  (if-car-sxpath '(rss channel (link 1) *text*)))

(define feed-rss2.0-entries
  (sxpath '(rss channel item)))

(define feed-rss2.0-entry-title
  (if-car-sxpath '((title 1) *text*)))

(define feed-rss2.0-entry-link
  (if-car-sxpath '((link 1) *text*)))

;;;; atom0.3

(define (feed-atom0.3? sxml)
  (and ((if-sxpath '(atom0.3:feed atom0.3:title)) sxml)
       ((if-sxpath '(atom0.3:feed atom0.3:link)) sxml)
       ((if-sxpath '(atom0.3:feed atom0.3:entry)) sxml)
       #t))

(define feed-atom0.3-title
  (if-car-sxpath '(atom0.3:feed (atom0.3:title 1) *text*)))

(define feed-atom0.3-link
  (if-car-sxpath '(atom0.3:feed (atom0.3:link (@ type (equal? "text/html")) 1)
                                @ href *text*)))

(define feed-atom0.3-entries
  (sxpath '(atom0.3:feed atom0.3:entry)))

(define feed-atom0.3-entry-title
  (if-car-sxpath '((atom0.3:title 1) *text*)))

(define feed-atom0.3-entry-link
  (if-car-sxpath
   '((atom0.3:link (@ type (equal? "text/html")) 1) @ href *text*)))

;;;; atom1.0

(define (feed-atom1.0? sxml)
  (and ((if-sxpath '(atom1.0:feed atom1.0:title)) sxml)
       ((if-sxpath '(atom1.0:feed atom1.0:link)) sxml)
       ((if-sxpath '(atom1.0:feed atom1.0:entry)) sxml)
       #t))

(define feed-atom1.0-title
  (if-car-sxpath '(atom1.0:feed (atom1.0:title 1) *text*)))

(define feed-atom1.0-link
  (if-car-sxpath '(atom1.0:feed (atom1.0:link 1) @ href *text*)))

(define feed-atom1.0-entries
  (sxpath '(atom1.0:feed atom1.0:entry)))

(define feed-atom1.0-entry-title
  (if-car-sxpath '((atom1.0:title 1) *text*)))

(define feed-atom1.0-entry-link
  (if-car-sxpath '((atom1.0:link 1) @ href *text*)))

;;;; format

(define (feed-format feed-title feed-link feed-entries entry-title entry-link)
  (^[sxml]
    `(("title"   . ,(feed-title sxml))
      ("link"    . ,(feed-link sxml))
      ("entries" . ,(map (^[e] `(("title" . ,(entry-title e))
                                 ("link"  . ,(entry-link e))))
                         (feed-entries sxml))))))

(define (feed-sxml->alist sxml)
  (cond [(feed-rss1.0? sxml)
         ((feed-format feed-rss1.0-title
                       feed-rss1.0-link
                       feed-rss1.0-entries
                       feed-rss1.0-entry-title
                       feed-rss1.0-entry-link)
          sxml)]
        [(feed-rss2.0? sxml)
         ((feed-format feed-rss2.0-title
                       feed-rss2.0-link
                       feed-rss2.0-entries
                       feed-rss2.0-entry-title
                       feed-rss2.0-entry-link)
          sxml)]
        [(feed-atom0.3? sxml)
         ((feed-format feed-atom0.3-title
                       feed-atom0.3-link
                       feed-atom0.3-entries
                       feed-atom0.3-entry-title
                       feed-atom0.3-entry-link)
          sxml)]
        [(feed-atom1.0? sxml)
         ((feed-format feed-atom1.0-title
                       feed-atom1.0-link
                       feed-atom1.0-entries
                       feed-atom1.0-entry-title
                       feed-atom1.0-entry-link)
          sxml)]
        [else (error <feedparser-error> "unknown type")]))

;;;; feedparser

(define (feedparser-from-port port)
  (feed-sxml->alist (ssax:xml->sxml port FEED_NAMESPACE)))

(define (feedparser-from-string str)
   (call-with-input-string str feedparser-from-port))

(define (feedparser-from-file path)
  `(,@(call-with-input-file path feedparser-from-port) ("path" . ,path)))

(define (feedparser url . header-kv-list)
  (let*-values ([(scheme user host port path query frag) (uri-parse url)]
                [(status header body)
                 (apply http-get
                        (call-with-output-string
                          (^[out]
                            (display host out)
                            (when port (format out ":~A" port))))
                        (call-with-output-string
                          (^[out]
                            (format out "~A" (or path "/"))
                            (when query (format out "?~A" query))
                            (when frag (format out "#~A" frag))))
                        :secure (equal? scheme "https")
                        header-kv-list)])
    (unless (string=? status "200")
      (error <feedparser-error> "unexpected status:" status))
    (unless (string? body)
      (error <feedparser-error> "string required, but got:" body))
    (let1 header-alist (fold (^[x r] `(,@r ,(apply cons x))) '() header)
      `(,@(feedparser-from-string
           (ces-convert (if-let1 ce (assoc-ref header-alist "content-encoding")
                          (cond [(string=? ce "gzip") (gzip-decode-string body)]
                                [(string=? ce "deflate") (inflate-string body)])
                          body)
                        "*JP"))
        ("url"    . ,url)
        ("status" . ,status)
        ("header" . ,header-alist)))))
