(use gauche.test)
(use util.list)

(test-start "feedparser")
(use feedparser)
(test-module 'feedparser)

(test* "rss1.0" #t
       (let1 f (feedparser-from-file (sys-realpath "xml/rss1.0.xml"))
         (and (assoc-ref f "title")
              (assoc-ref f "link")
              #t)))

(test* "rss2.0" #t
       (let1 f (feedparser-from-file (sys-realpath "xml/rss2.0.xml"))
         (and (assoc-ref f "title")
              (assoc-ref f "link")
              #t)))

(test* "atom0.3" #t
       (let1 f (feedparser-from-file (sys-realpath "xml/atom0.3.xml"))
         (and (assoc-ref f "title")
              (assoc-ref f "link")
              #t)))

(test* "atom1.0" #t
       (let1 f (feedparser-from-file (sys-realpath "xml/atom1.0.xml"))
         (and (assoc-ref f "title")
              (assoc-ref f "link")
              #t)))

(test-end)
