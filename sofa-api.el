;;; sofa-api.el --- CouchDB API

;; Copyright (C) 2012  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: comm, data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


(require 'curl)
(require 'json)

;; TODO: the core API should be placed into this file.

(defvar sofa-user-name nil)
(defvar sofa-user-password nil)
(defvar sofa-server "localhost")
(defvar sofa-port 5984)

(defvar cloudant-user-name nil)
(defvar cloudant-user-password nil)
(defconst cloudant-endpoint-url ".cloudant.com/")

(defvar sofa-endpoint nil)
(defalias 'sofa-endpoint 'sofa-couchdb-url)

(defvar sofa-use-https nil
  "If non-nil, use HTTPS instead of HTTP.")

(defsubst assoc-value (key alist &optional default)
  "Return the first association for KEY in ALIST.  Otherwise DEFAULT."
  (let ((pair (assoc key alist)))
    (if pair
        (cdr pair)
      default)))

(defmacro sofa/doarray (spec &rest body)
  "Loop over an array, like `dolist'.

\(fn (VAR ARRAY [RESULT]) BODY...)"
  ;; spec = (VAR ARRAY RESULT)
  ;;(declare (indent 1))
  ;; Finally! I got some understanding of `(declare (debug ...))' !
  (declare (indent 1) (debug ((symbolp sexp &optional form) &rest form)))
  (let ((index (make-symbol "INDEX"))
        (arlen (make-symbol "ARRAY-LENGTH")))
    `(progn
       (setq ,index 0)
       (setq ,arlen (length ,(cadr spec)))
       (while (< ,index ,arlen)
         (setq ,(car spec) (aref ,(cadr spec) ,index))
         (progn ,@body)
         (setq ,index (1+ ,index)))
       ,(caddr spec))))

(defsubst strip-etag (etag)
  "Remove the enclosing double quotation mark."
  (let* ((lst (split-string etag "\""))
         (len (length lst)))
    (cond ((= len 1)
           (nth 0 lst))
          (t
           (nth 1 lst)))))

(defun sofa-view-endpoint (database &optional design view &rest params)
  "Return the endpoint URL for the database."
  (let ((url (sofa-endpoint database)))
    (cond ((and (null design) (null view))
           (setq url (concat url "/_all_docs"
                             (parse-query-params params))))
          ((and design (null view))
           (setq url (concat url "/_design/" (url-hexify-string design)
                             (parse-query-params params))))
          ((and design view)
           (setq url (concat url "/_design/" (url-hexify-string design)
                             "/_view/" (url-hexify-string view)
                             (parse-query-params params))))
          (t
           (error "invalid argument(s)")))))

      
(defun sofa-couchdb-url (&optional database)
  (let ((hex-nam (url-hexify-string sofa-user-name))
        (hex-pwd (url-hexify-string sofa-user-password)))
    (format "%s://%s%s%s/%s"
            (if sofa-use-https "https" "http")
            (if (string-equal hex-nam "")
                ""
              (format "%s:%s@" hex-nam hex-pwd))
            sofa-server
            (if sofa-port
                (format ":%d" sofa-port)
              "")
            (or database ""))))

(defun sofa-cloudant-url (&optional database)
  (let ((hex-nam (url-hexify-string cloudant-user-name))
        (hex-pwd (url-hexify-string cloudant-user-password)))
    (format "%s://%s:%s@%s%s%s"
            (if sofa-use-https "https" "http")
            hex-nam hex-pwd hex-nam
            cloudant-endpoint-url
            (or database ""))))

(defun sofa-databases (&optional https)
  "Get the list of the all databases."
  (let ((url (concat (sofa-endpoint) "_all_dbs"))
        result)
    (setq result (curl/http-recv 'get url))
    (let ((json-key-type 'string))
      (append (json-read-from-string (cdr result)) nil))))

(defun sofa-create-database (database)
  "Create database, DATABASE.

On error, raise `error' with the reason, otherwise return t."
  (let ((url (concat (sofa-endpoint) (url-hexify-string database)))
        result)
    (curl/with-temp-buffer
      (setq result (curl/http-send-buffer 'put url nil nil)))
    (let ((json-key-type 'string) 
          parsed)
      ;; TODO: check the HTTP status first.
      (setq parsed (json-read-from-string (cdr result)))
      (let ((ok (assoc "ok" parsed))
            (err (assoc "error" parsed)))
        (unless ok 
          (error (cdr err)))
        (cdr ok)))))

(defun sofa-database-exist-p (database)
  "Test if DATABASE existed."
  (let* ((url (concat (sofa-endpoint) (url-hexify-string database)))
         (headers (car (curl/http-recv 'HEAD url)))
         (status (string-to-int (assoc-value "Status" headers "404"))))
    (and (>= status 200) (< status 300))))

(defun sofa-get-database-info (database)
  "Get the information about DATABASE."
  (let* ((url (concat (sofa-endpoint) (url-hexify-string database)))
         (result (curl/http-recv 'GET url))
         (headers (car result))
         (status (string-to-int (assoc-value "Status" headers "404")))
         (body (cdr result)))
    (if (eq status 200)
        (json-read-from-string body))))

  
(defun sofa-get (url &optional buffer)
  "HTTP GET request to URL.

It returns a form (RESPONSE-HEADERS . BODY) where RESPONSE-HEADERS
is an alist of HTTP reponse headers with extra \"Status\" key for
HTTP response code, and BODY is the string of the response body.

If BUFFER is non-nil, this function will save the response body
into BUFFER, and returns a form (RESPONSE-HEADERS . nil)."
  (let ((result (curl/http-recv 'get url buffer)))
    ;; TODO: check http status first
    (if buffer
        (when sofa-json-prettifier
          (with-current-buffer buffer
            (copy-region-as-kill (point-min) (point-max))
            (unless (eq (shell-command-on-region 
                         (point-min) (point-max)
                         sofa-json-prettifier nil 'replace
                         shell-command-default-error-buffer t) 0)
              (erase-buffer)
              ;; TODO: need to refind below line for `undo' feature.
              (insert-for-yank (current-kill 0)))))
      (let ((json-key-type 'string))
        (json-read-from-string (cdr result))))))

    
(defun parse-query-params (params)
  (flet ((keyword-name (key)
                       (replace-regexp-in-string "-" "_"
                                                 (substring
                                                  (symbol-name key) 1) t t))
         (param-eval (value &optional json)
                     (cond ((stringp value) 
                            (if json (json-encode-string value) value))
                           ((listp value)
                            (json-encode-alist value))
                           ((arrayp value)
                            (json-encode-array value))
                           ((null value) "false")
                           ((eq t value) "true")
                           ((numberp value) 
                            (format "%S" value))))
                     
         (parse (key value params result)
                ;; (format "&%s=%s"
                ;;         (url-hexify-string
                ;;          (keyword-name key))
                ;;         (cond ((or (eq key :key)
                ;;                    (eq key :keys)
                ;;                    (eq key :startkey)
                ;;                    (eq key :endkey))
                ;;                (url-hexify-string (param-eval value 'json)))
                ;;               (t
                ;;                (url-hexify-string (param-eval value)))))
                (if key
                    (let ((pair (format "&%s=%s"
                                        (url-hexify-string
                                         (keyword-name key))
                                        (url-hexify-string 
                                         (param-eval value)))))
                      (parse (car params)
                             (cadr params)
                             (cddr params)
                             (concat result pair)))
                  (concat "?" (substring result 1)))))
    (if params
        (parse (car params) (cadr params) (cddr params) "")
      "")))

(defun sofa-get-design (database design &optional buffer)
  (let ((url (sofa-view-endpoint database design)))
    (message "GET %s" url)
    (if buffer
        (with-current-buffer buffer
          (setq sofa-source-url url)))
    (sofa-get url buffer)))


(defun sofa-get-all-designs (database)
  "Return a list of design document names in DATABASE."
  (let ((url (sofa-view-endpoint "hello" nil nil 
                                    :start-key "\"_design/\""
                                    :end-key "\"_design0\"")))
    (let* ((result nil)
           (json (sofa-get url))
           (err (assoc-value "error" json nil))
           (dvec (assoc-value "rows" json nil)))
      (if err
          (error err))
      (sofa/doarray (elem dvec)
        (let ((docname (assoc-value "id" elem nil)))
          (if docname
              (push (substring docname (length "_design/")) result))))
      result)))

(defun sofa-put-design (database design doc &optional rev-id)
  (let ((url (sofa-view-endpoint database design)))
    (if (bufferp doc)
        nil                             ;TODO: what if DOC is buffer?
      (when rev-id
        (if (assoc "_rev" doc)
            (setcdr (assoc "_rev" doc) rev-id)
          (setq doc (cons (cons "_rev" rev-id) doc )))))

    (curl/with-temp-buffer
      (if (bufferp doc)
          (insert-buffer doc)
        (insert (json-encode-alist doc)))
      (setq result 
            (curl/http-send-buffer 'put url (current-buffer)
                                   "application/json")))
    (let ((json-key-type 'string) 
          parsed)
      ;; TODO: check the HTTP status first.
      (setq parsed (json-read-from-string (cdr result)))
      (let ((ok (assoc "ok" parsed))
            (err (assoc "error" parsed)))
        (unless ok 
          (error (cdr err)))
        (cdr (assoc "rev" parsed))))))

  
(defun sofa-get-design-info (database design &optional buffer)
  (let ((url (concat (sofa-view-endpoint database design) "/_info")))
    (sofa-get url buffer)))

;; (curl/http-start-recv 'get
;;           "http://username:password@localhost:5984/fs/_design/fs/_view/path"
;;           nil
;;           (lambda (proc event)
;;             (curl/http-body (point-min) (point-max) nil 'utf-8-dos)))

(defun sofa-get-view (database design view &optional buffer &rest params)
  (let ((url (apply 'sofa-view-endpoint
                    (append (list database design view) params))))
    (sofa-get url buffer)))


(defun sofa-get-bulk-documents (database docs &optional buffer 
                                         noerror nocontent raw)
  "Read document(s) from DATABASE.

DOCS is a list of document IDs.

If BUFFER is non-nil, the document contents will be stored in BUFFER.

If NOERROR is non-nil, this function will not raise an error.

If NOCONTENT is non-nil, this function retrives ID and VALUE only.  Note
that this is only meaningful when DOC-ID is a list of document IDs.

If RAW is non-nil, this function will not prettify the document contents."
  ;; TODO: If RAW is non-nil, return a buffer containing the JSON doc.
  (let ((in (json-encode-alist (list (cons 'keys (vconcat keys)))))
        (url (sofa-view-endpoint database nil nil 
                                 :include-docs
                                 (if nocontent nil t)))
        result)
    (curl/with-temp-buffer
      (insert in)
      (setq result (curl/http-send-buffer 'POST url (current-buffer))))

    ;; TODO: check http status first

    (if buffer
        (when sofa-json-prettifier
          (with-current-buffer buffer
            (copy-region-as-kill (point-min) (point-max))
            (unless (eq (shell-command-on-region (point-min) (point-max)
                                     sofa-json-prettifier nil 'replace
                                     shell-command-default-error-buffer t) 0)
              (erase-buffer)
              ;; TODO: need to refind below line for `undo' feature.
              (insert-for-yank (current-kill 0)))))
      (let ((json-key-type 'string))
        (json-read-from-string (cdr result))))))


(defun sofa-get-document (database doc-id &optional buffer)
  ;; TODO: If RAW is non-nil, return a buffer containing the JSON doc.
  (let ((url (concat (sofa-endpoint database) "/" 
                     (url-hexify-string doc-id)))
        result)
    (setq result (curl/http-recv 'get url buffer))
    ;; TODO: check http status first

    (if buffer
        (when sofa-json-prettifier
          (with-current-buffer buffer
            (copy-region-as-kill (point-min) (point-max))
            (unless (eq (shell-command-on-region (point-min) (point-max)
                                     sofa-json-prettifier nil 'replace
                                     shell-command-default-error-buffer t) 0)
              (erase-buffer)
              ;; TODO: need to refind below line for `undo' feature.
              (insert-for-yank (current-kill 0)))))
      (let ((json-key-type 'string))
        (json-read-from-string (cdr result))))))


(defun sofa-put-attachment (database doc-id filename &optional rev-id)
)

(defun sofa-put-document (database doc-id doc &optional rev-id)
  (let ((url (concat (sofa-endpoint database) "/" 
                     (url-hexify-string doc-id)))
        result)
    ;; TODO: If doc is string or buffer, use the buffer contents.
    ;;       In this case, REV-ID will be ignored.

    ;; modify DOC if REV-ID is non-nil
    (when rev-id
      (if (assoc "_rev" doc)
          (setcdr (assoc "_rev" doc) rev-id)
        (setq doc (cons (cons "_rev" rev-id) doc ))))

    (curl/with-temp-buffer
      (insert (json-encode-alist doc))
      (setq result 
            (curl/http-send-buffer 'put url (current-buffer)
                                   "application/json")))
    (let ((json-key-type 'string) 
          parsed)
      ;; TODO: check the HTTP status first.
      (setq parsed (json-read-from-string (cdr result)))
      (let ((ok (assoc "ok" parsed))
            (err (assoc "error" parsed)))
        (unless ok 
          (error (cdr err)))
        (cdr (assoc "rev" parsed))))))

(defun sofa-document-revision (database doc-id)
  "Get the latest document revision of DOC-ID from DATABASE."
  (let ((url (concat (sofa-endpoint database) "/"
                     (url-hexify-string doc-id))))
    (setq result (curl/http-recv 'head url)
          rev-id (assoc-value "Etag" (car result)))
    (and rev-id
         (strip-etag rev-id))))


(defun sofa-delete-document (database doc-id &optional rev-id)
  "Delete the document, DOC-ID with revision REV-ID from DATABASE.

If REV-ID is nil, it will automatically retrived.
This function returns the revision id of the delete operation."
  (unless rev-id
    (setq rev-id (sofa-document-revision database doc-id)))
  (unless rev-id
    (error "Etag missing"))
  (let ((url (concat (sofa-endpoint database) "/"
                     (url-hexify-string doc-id) "?rev=" 
                     rev-id))
        result)
    (setq result (curl/http-recv 'delete url))
    ;; On success:
    ;;   {"ok":true,"id":"worldy","rev":"5-f1b2366c24f5d088ccd2a7e998f6c3a9"}
    ;;
    ;; On failure:
    ;;   {"error":"unknown_error","reason":"badarg"}
    
    ;; TODO: check HTTP status
    (let ((json-key-type 'string) parsed)
      (setq parsed (json-read-from-string (cdr result)))
      (let ((ok (assoc "ok" parsed))
            (err (assoc "error" parsed)))
        (unless ok 
          (error (cdr err)))
        (cdr (assoc "rev" parsed))))))
      



(provide 'sofa-api)
;;; sofa-api.el ends here
