;; Quick script to fetch the newest OOTS comic

(require 'regex)
(require 'http-client)
(require 'rss)
(require 'uri)
(require 'srfi-13)

(define oots-rss-location "http://www.giantitp.com/comics/oots.rss")

;; Reads the RSS, gets the first (most recent) entry and returns the title and the link
(define (fetch-latest-comic-item url)
  (with-input-from-string (http:GET url)
    (lambda ()
      (let* ((rss-data (rss:read))
             (rss-item (car (rss:feed-items rss-data)))
             (oots-title (rss:item-title rss-item))
             (oots-link (rss:item-link rss-item)))
        (list oots-title oots-link)))))

(define (check-existing comic-url destination-file)
  (let ((filename (last (uri-split-path comic-url))))
    (unless (file-exists? destination-file)
      #f
      (with-output-to-file destination-file
        (lambda ()
          (print (http:GET comic-url)))))))

(define (locate-comic-image-link input)
  (string-append
   (uri-host (uri oots-rss-location))
   (last (string-search "(<TD align=\"center\"><IMG src=\"(.*gif)\"></TD>)" input))))

(define (fetch-oots-comic)
  (let* ((rss-item (fetch-latest-comic-item oots-rss-location))
         (oots-title (car rss-item))
         (oots-link (cadr rss-item))
         (oots-html (http:GET oots-link))
         (destination-filename (string-append "oots" (first (string-split oots-title ":")) ".gif")))
    (check-existing (locate-comic-image-link oots-html) destination-filename)
    (print "Content-type: image/gif\n")
    (with-input-from-file destination-filename
      (lambda ()
        (print (read-string))))))

(fetch-oots-comic)