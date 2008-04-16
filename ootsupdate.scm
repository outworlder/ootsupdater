;; Quick script to fetch the newest OOTS comic, avoiding Atlantico's Great Firewall.

(require 'regex)
(require 'http-client)
(require 'rss)
(require 'uri)
(require 'srfi-13)
(require 'posix)

(define oots-rss-location "http://www.giantitp.com/comics/oots.rss")
(define oots-storage-dir "comics")
(define oots-config "ootsupdate.cfg")

(define (save-latest oots-comic)
  (with-output-to-file oots-config
    (lambda ()
      (write oots-comic))))

(define (load-latest)
  (if (file-exists? oots-config)
      (with-input-from-file oots-config
        (lambda ()
          (read)))))

;; Reads the RSS, gets the first (most recent) entry and returns the title and the link.
(define (fetch-latest-comic-item url)
  (with-exception-handler
   (lambda (exception)
          #f)
  (with-input-from-string (http:GET url)
    (lambda ()
      (let* ((rss-data (rss:read))
             (rss-item (car (rss:feed-items rss-data)))
             (oots-title (rss:item-title rss-item))
             (oots-link (rss:item-link rss-item)))
        (list oots-title oots-link))))))

;; Checks if the file given by destination-file exists.
;; If not, downloads from comic-url and saves it in the specified location
;; Also, creates the destination directory if it does not exist
(define (check-existing comic-url destination-file)
  (let ((filename (last (uri-split-path comic-url))))
    (unless (directory? oots-storage-dir)
      (create-directory oots-storage-dir))
    (unless (file-exists? destination-file)
      (with-output-to-file destination-file
        (lambda ()
          (print (http:GET comic-url)))))))

;; Finds the link to the comic in the page passed as input.
(define (locate-comic-image-link input)
  (string-append
   (uri-host (uri oots-rss-location))
   (last (string-search "(<TD align=\"center\"><IMG src=\"(.*gif)\"></TD>)" input))))

;; Fetches the newest comic from the website and outputs as an image
(define (fetch-oots-comic)
  (let* ((rss-item (fetch-latest-comic-item oots-rss-location))
         (oots-title (car rss-item))
         (oots-link (cadr rss-item))
         (oots-html (http:GET oots-link))
         (destination-filename (string-append oots-storage-dir "/" "oots" (first (string-split oots-title ":")) ".gif"))
         (latest-saved (load-latest)))
    (unless (equal? latest-saved rss-item)
      (save-latest rss-item)
      (check-existing (locate-comic-image-link oots-html) destination-filename))
    (print "Content-type: image/gif\n")
    (with-input-from-file destination-filename
      (lambda ()
        (print (read-string))))))

(fetch-oots-comic)
