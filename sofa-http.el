(defun sofa--http-recv (method url &optional buffer)
  "This function returns a form, (HEADER-ALIST . BODY). If buffer
is non-nil will insert body into buffer and return (HEADER-ALIST
. nil)."
  (let* ((url-request-method (upcase (if (stringp method) method (symbol-name method))))
	 (result-buffer (url-retrieve-synchronously url)))
    (with-current-buffer result-buffer
      (let ((body (string-trim
		   (buffer-substring-no-properties url-http-end-of-headers (point-max))))
	    (headers `(("Content-Encoding"    . ,(mail-fetch-field "content-encoding"))
		       ("Cache-Control"       . ,(mail-fetch-field "cache-control"))
		       ("Etag"                . ,(mail-fetch-field "etag"))
		       ("X-Couch-Request-ID"  . ,(mail-fetch-field "X-Couch-Request-ID"))
		       ("X-CouchDB-Body-Time" . ,(mail-fetch-field "X-CouchDB-Body-Time"))
		       ("Vary"                . ,(mail-fetch-field "vary"))
		       ("Connection"          . ,(mail-fetch-field "connection"))
		       ("Transfer-Encoding"   . ,url-http-transfer-encoding)
		       ("Content-Type"        . ,url-http-content-type)
		       ("Date"                . ,(mail-fetch-field "data"))
		       ("Server"              . ,(mail-fetch-field "server"))
		       ("Status"              . ,url-http-response-status))))
	(when buffer
	  (with-current-buffer buffer
	    (erase-buffer)
	    (insert body)))
	(cons headers (if buffer nil body))))))

(defun sofa--http-send (method url content &optional content-type)
  "Send `content' to `url' using `method'. `content-type'defaults to 'application/octet-stream'."
  (let ((url-request-extra-headers `(("Content-type" . ,(or content-type "application/octet-stream"))))
	(url-request-data (cond
			   ((bufferp content) (with-current-buffer content (buffer-string)))
			   ((stringp content) content)
			   (t (error "Invalid content")))))
    (sofa--http-recv method url)))

(provide 'sofa-http)
