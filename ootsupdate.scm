;; Quick script to fetch the newest OOTS comic, avoiding Atlantico's Great Firewall.

(use regex)
(use http-client)
(use rss)
(use uri)
(use srfi-13)
(use posix)

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
  (call/cc
   (lambda (exit-error)
     (with-exception-handler
      (lambda (exception)
        (exit-error (list)))
      (lambda ()
        (with-input-from-string (http:GET url)
          (lambda ()
            (let* ((rss-data (rss:read))
                   (rss-item (car (rss:feed-items rss-data)))
                   (oots-title (rss:item-title rss-item))
                   (oots-link (rss:item-link rss-item)))
              (list oots-title oots-link)))))))))

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

(define (dismember-rss-item rss-item)
  (let ((oots-title (car rss-item))
        (oots-link (cadr rss-item)))
    (list oots-title oots-link)))

(define (assemble-destination-filename oots-title)
  (string-append oots-storage-dir "/" "oots" (first (string-split oots-title ":")) ".gif"))

(define (output-image-data destination-filename)
  (with-input-from-file destination-filename
    (lambda ()
      (print "Content-type: image/gif\n")
      (print (read-string)))))

(define (abort-error-message exception)
  (if (condition? exception)
      (print "Exception: " ((condition-property-accessor 'exn 'message) exception))
      (print "Error: " exception))
  (exit))

(define (fetch-oots-html oots-link)
  (http:GET oots-link))

;; Fetches the newest comic from the website and outputs as an image
(define (fetch-oots-comic)
  (with-exception-handler
   (lambda (exception)
     (output-error-message exception))
   (lambda ()
     (let* ((rss-item (fetch-latest-comic-item oots-rss-location))
            (latest-saved (load-latest)))
       (if (and (null? rss-item)
                (null? latest-saved))
           (abort-error-message "RSS could not be retrieved and no previous comic downloaded. Aborting.")
           (let ((destination-filename
                  (assemble-destination-filename
                   (if (equal? latest-saved rss-item)
                       (car latest-saved)
                       (if (null? rss-item)
                           (car latest-saved)
                           (begin 
                             (check-existing
                              (locate-comic-image-link (fetch-oots-html (cadr rss-item))))
                             (save-latest rss-item)
                             (car rss-item)))))))
             (output-image-data destination-filename)))))))


(fetch-oots-comic)