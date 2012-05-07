;;; couchdb.el --- Cloudant API

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

(defvar couchdb-user-name nil)
(defvar couchdb-user-password nil)
(defvar couchdb-server "localhost")
(defvar couchdb-port 5984)

(defvar cloudant-user-name nil)
(defvar cloudant-user-password nil)
(defconst cloudant-endpoint-url ".cloudant.com/")

(defvar couch-endpoint nil)
(defalias 'couch-endpoint 'couchdb-url)

(defvar couchdb-json-prettifier "python -mjson.tool")
(defvar couchdb-json-error-buffer "*CouchDB JSON errors*")
(defvar couchdb-json-edit-buffer "*CouchDB Editing JSON*")

(defvar couchdb-document-buffer "*CouchDB Document*"
  "buffer name for view/editing one or multiple documents.")

(defvar couchdb-use-https nil
  "If non-nil, use HTTPS instead of HTTP.")

(defface couchdb-value-edited-face '((t :inverse-video t))
  "Face to mark the current editing value.")

(defface couchdb-revision-face '((t :foreground "magenta"))
  "Face to mark the current editing value.")

(defface couchdb-id-face '((t :foreground "cyan"))
  "Face to mark the current editing value.")

(defconst couchdb-json-error-regexp-for-compile
  "^\\(.*?line [0-9]+ column [0-9]+ (char [0-9]+)\\)$"
  "Regular expression for an error output from `couchdb-json-prettifier'.")

(defsubst assoc-value (key alist &optional default)
  "Return the first association for KEY in ALIST.  Otherwise DEFAULT."
  (let ((pair (assoc key alist)))
    (if pair
        (cdr pair)
      default)))

(defmacro couchdb/doarray (spec &rest body)
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

(defun couchdb-view-endpoint (database &optional design view &rest params)
  "Return the endpoint URL for the CouchDB."
  (let ((url (couch-endpoint database)))
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

      
(defun couchdb-url (&optional database)
  (let ((hex-nam (url-hexify-string couchdb-user-name))
        (hex-pwd (url-hexify-string couchdb-user-password)))
    (format "%s://%s%s%s/%s"
            (if couchdb-use-https "https" "http")
            (if (string-equal hex-nam "")
                ""
              (format "%s:%s@" hex-nam hex-pwd))
            couchdb-server
            (if couchdb-port
                (format ":%d" couchdb-port)
              "")
            (or database ""))))

(defun cloudant-url (&optional database)
  (let ((hex-nam (url-hexify-string cloudant-user-name))
        (hex-pwd (url-hexify-string cloudant-user-password)))
    (format "%s://%s:%s@%s%s%s"
            (if couchdb-use-https "https" "http")
            hex-nam hex-pwd hex-nam
            cloudant-endpoint-url
            (or database ""))))

(defun couchdb-databases (&optional https)
  "Get the list of the all databases."
  (let ((url (concat (couch-endpoint) "_all_dbs"))
        result)
    (setq result (curl/http-recv 'get url))
    (let ((json-key-type 'string))
      (append (json-read-from-string (cdr result)) nil))))

(defun couchdb-create-database (database)
  "Create database, DATABASE.

On error, raise `error' with the reason, otherwise return t."
  (let ((url (concat (couch-endpoint) (url-hexify-string database)))
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

(defun couchdb-database-exist-p (database)
  "Test if DATABASE existed."
  (let* ((url (concat (couch-endpoint) (url-hexify-string database)))
         (headers (car (curl/http-recv 'HEAD url)))
         (status (string-to-int (assoc-value "Status" headers "404"))))
    (and (>= status 200) (< status 300))))

(defun couchdb-get-database-info (database)
  "Get the information about DATABASE."
  (let* ((url (concat (couch-endpoint) (url-hexify-string database)))
         (result (curl/http-recv 'GET url))
         (headers (car result))
         (status (string-to-int (assoc-value "Status" headers "404")))
         (body (cdr result)))
    (if (eq status 200)
        (json-read-from-string body))))

  
(defun couchdb--documents-keywords ()
  (list (cons "^...#.*$" 'font-lock-comment-face)
        (list "^...[[:space:]]*\\(\\<[^[:space:]]*\\>\\)[[:space:]]+\\(.*\\)$" 
              '(1 'couchdb-revision-face)
              '(2 'couchdb-id-face))))

(defun couchdb-view-quit ()
  "Quit the current buffer"
  (interactive)
  ;; TODO: check if there is unsaved data
  (couchdb--hide-window-kill-buffer))


(defun couchdb--view-point-on-doc-p ()
  "Return t if the current line shows a document."
  (let ((oldpos (point)))
    (save-match-data
      (beginning-of-line)
      (let ((result (looking-at
                     "^...[[:space:]]*\\(\\<[^[:space:]]*\\>\\)[[:space:]]+\\(.*\\)$")))
        (goto-char oldpos)
        result))))

(defun couchdb--view-point-on-id ()
  (let ((oldpos (point)))
    (save-match-data
      (beginning-of-line)
      (if (looking-at
           "^...[[:space:]]*\\(\\<[^[:space:]]*\\>\\)[[:space:]]+\\(.*\\)$")
          (goto-char (match-beginning 2))
        (goto-char oldpos)))))

(defun couchdb-view-forward-line (&optional n)
  (interactive "p")
  (let ((remain (forward-line n)))
    (when (not (eq remain n))
      (couchdb--view-point-on-id))))

(defun couchdb-view-previous-line (&optional n)
  (interactive "p")
  (couchdb-view-forward-line (- n)))

(defun couchdb--marked-keys ()
  ;; Note that the first three characters on the document line is used
  ;; for marking, which may not contain the text property, 'couch-key
  (couchdb--map-over-marks
      (lambda () (get-text-property (+ 3 (point)) 'couch-key))))

(defmacro couchdb--map-over-marks (function)
  "Map a FUNCTION on every marked line.  Return a list of FUNCTION's result.

The point will be set before evaluating FUNCTION at the beginning
of the line.  If there is no marked line, it will evalute
FUNCTION on the current line iff the current line contains a
document."
  (declare (debug t))
  (let ((oldpos (make-symbol "OLDPOS"))
        (pos (make-symbol "POS"))
        (result (make-symbol "RESULT"))
        (processed (make-symbol "PROCESSED")))
    `(let ((,oldpos (point-marker))
           (,pos (make-marker))
           (,processed 0)
           ,result)
       ;; TODO: check the documentation of `save-restriction' for the
       ;;       order of `save-restriction' and `save-excursion'.
       (save-restriction
         (widen)
         (goto-char (point-min))
         (while (and (not (eobp)) (not (couchdb--view-point-on-doc-p)))
           (forward-line 1))

         (while (and (not (eobp)) (couchdb--view-point-on-doc-p))
           (if (progn (beginning-of-line) (looking-at "\\*"))
               (progn
                 (save-excursion
                   (forward-line 1)
                   (move-marker ,pos (1+ (point)))) ; why calls `1+'?
                 (save-excursion
                   ;; TODO: do we need to revert the point even on errors?
                   (setq ,processed (1+ ,processed)
                         ,result (cons (funcall ,function) ,result)))
                 (goto-char ,pos))
             (forward-line 1)))

         (when (eq ,processed 0)
           (goto-char ,oldpos)
           (when (couchdb--view-point-on-doc-p)
             (beginning-of-line)
             (save-excursion
               (setq ,processed (1+ ,processed)
                     ,result (cons (funcall ,function) ,result))))))
       (goto-char ,oldpos)
       (move-marker ,oldpos nil)
       (move-marker ,pos nil)
       (nreverse ,result))))
    
        

(defun couchdb-repeat-over-lines (arg function)
  "Works similar to `dired-repeat-over-lines'."
  (let ((pos (make-marker)))
    (beginning-of-line)
    (while (and (> arg 0) (not (eobp)))
      (setq arg (1- arg))
      (beginning-of-line)
      (while (and (not (eobp)) (not (couchdb--view-point-on-doc-p))
                  (forward-line 1)))
      (save-excursion
        (forward-line 1)
        (move-marker pos (1+ (point)))) ; don't understand why calls `1+'.
      (save-excursion (funcall function))
      (goto-char pos))
    (while (and (< arg 0) (not bobp))
      (setq arg (1+ arg))
      (forward-line -1)
      (while (and (not (bobp)) (not (couchdb--view-point-on-doc-p))
                  (forward-line -1)))
      (beginning-of-line)
      (save-excursion (funcall function)))
    (move-marker pos nil)
    (couchdb--view-point-on-id)))


(defun couchdb-view-mark (arg)
  (interactive "P")
  ;; TODO: handle ARG
  (let ((inhibit-read-only t))
    (couchdb-repeat-over-lines (prefix-numeric-value arg)
                               (lambda () (delete-char 1) (insert ?\*)))))
    ;; (when (couchdb--view-point-on-doc-p)
    ;;   (beginning-of-line)
    ;;   (delete-char 1)
    ;;   (insert-char ?\* 1)
    ;;   (couchdb-view-forward-line))))

(defun couchdb-view-unmark (arg)
  (interactive "P")
  ;; TODO: handle ARG
  (let ((inhibit-read-only t))
    (couchdb-repeat-over-lines (prefix-numeric-value arg)
                               (lambda () (delete-char 1) (insert " ")))))

(defun couchdb-view-backward-page (&optional count)
  (interactive "p")
  (couchdb-view-forward-page (- count)))

(defun couchdb-view-forward-page (&optional count)
  (interactive "p")
  (save-restriction
    (widen)
    (let ((oldpos (point)))
      (goto-char (point-min))
      (while (and (not (eobp)) (not (couchdb--view-point-on-doc-p)))
        (forward-line 1))
      ;; TODO: what if there is unsaved contents? (e.g. marks)
      (let ((inhibit-read-only t)
            (docs-begin (point))
            (docs-end (point-max))
            (skip couchdb-skip)
            (limit couchdb-limit))
        ;; TODO: bound check for `couchdb-skip'
        (setq skip (max (+ skip (* count limit)) 0))
        (goto-char (point-max))
        (let ((nread (couchdb--view-fill-page skip limit)))
          (message "%d document(s) read" nread)
          (if (<= nread 0)
              (goto-char oldpos)
            (delete-region docs-begin docs-end)
            (setq couchdb-skip skip
                  couchdb-limit limit)
            (goto-char docs-begin)))))))


(defun couchdb-view-load-doc-other-window ()
  (interactive)
  ;; TODO: if there's marked doc(s), load that in batch mode?
  (let ((keys (couchdb--marked-keys)))
    (when keys
      (if (> (length keys) 1)
          ;; TODO: multi doc load
          (error "not implemented yet.")
        (car doc)
        ))))

(defun couchdb--load-document (database key)
  "Load the document where the id is KEY from DATABASE, return the buffer."
  ;; TODO: I need to decide the mechanism of loading individual
  ;;       document.  If I use only one buffer to load document(s), it
  ;;       will be easy to maintain the consistency against dealing
  ;;       with multiple buffers.
  (let* ((bufname (concat "couchdb:" database "/" key))
         (buffer (get-buffer bufname)))
    (if buffer
      ;; TODO: what if there's already that buffer?
      (with-current-buffer buffer
        (if (not couchdb-source-url)
            ;; this buffer has nothing to do with CouchDB.
            (progn 
              (pop-to-buffer buffer)
              (error "The buffer \"%s\" is already used for other purpose!"))
          (let ((inhibit-read-only t))
            (erase-buffer))))
      (setq buffer (get-buffer-create bufname)))
    (with-current-buffer buffer
      ;; TODO: Is it safe to change the major mode if the buffer
      ;;       already existed?  What if there's unsaved contents?
      (couchdb-json-mode)
      ;; TODO: error handling of `couchdb-get-document'.
      (couchdb-get-document database key buffer)
      (setq couchdb-database-name database
            couchdb-document-name key))
    buffer))

(defun couchdb--view-fill-page (skip limit)
  "load LIMIT document(s) at the point with SKIP document(s) skipped.

This function loads document(s) according to the `couchdb-limit'
and `couchdb-skip'"
  (let ((url (couchdb-view-endpoint couchdb-database-name
                                    couchdb-design-name 
                                    couchdb-view-name
                                    :limit limit :skip skip))
        (count 0)
        result docs)
    ;; TODO: do we need to check for (and (bolp) (eolp))?
    (message "Loading view... (skip %d) (limit %d)" skip limit)

    (setq result (couchdb-get url)
          docs (assoc-value "rows" result []))

    (let ((inhibit-read-only t))
      (couchdb/doarray (doc docs)
        (let ((key (assoc-value "key" doc "N/A"))
              (rev (assoc-value "rev" (assoc-value "value" doc nil)
                                "N/A")))
          (let ((text (format "   %-40s %s" rev key)))
            (insert text)
            (put-text-property (line-beginning-position) (line-end-position)
                               'couch-key key)
            (newline))
          (setq count (1+ count)))))
    count))
      
    
(defun couchdb-load-view (&optional database design view limit skip)
  "Launch view mode like Futon"
  (interactive)
  (unless database
    (setq database (couchdb-read-database-name-from-minibuffer "Database: ")))
  (and (null limit)
       (setq limit 10))
  (and (null skip)
       (setq skip 0))
  (let ((dbinfo (couchdb-get-database-info database))
        docs)
    (let* ((url (couchdb-view-endpoint database design view 
                                       :limit limit :skip skip))
           (result (couchdb-get url))
           (bufname (concat "couchdb:" database))
           (buffer (get-buffer bufname)))
      (setq docs (assoc-value "rows" result []))
      (if buffer
          (with-current-buffer buffer
            ;; TODO: check if the buffer is couchdb-load-view mode and ...
            (setq buffer-read-only nil)
            (erase-buffer))
        (setq buffer (get-buffer-create (concat "couchdb:" database))))
      (with-current-buffer buffer
        (couchdb-view-mode)

        (setq couchdb-database-name database
              couchdb-design-name design
              couchdb-view-name view)

        (let ((inhibit-read-only t))
          (dolist (ent dbinfo)
            (insert (format "   # %s: %S\n"
                            (symbol-name (car ent)) (cdr ent)))))

        (couchdb--view-fill-page skip limit)
        (setq couchdb-skip skip
              couchdb-limit limit)
        
        (switch-to-buffer (current-buffer)))
    ;; TODO: dired-like mode?
      )))
      


(defun couchdb-load-database (&optional database limit)
  "Load the database in CouchDB mode."
  (interactive)
  (unless database
    (setq database (couchdb-read-database-name-from-minibuffer "Database: ")))
  (if (null limit)
      (setq limit 10))

  (let ((dbinfo (couchdb-get-database-info (url-hexify-string database)))
        docs)
    (let* ((url (concat (couch-endpoint) (url-hexify-string database)
                        "/_all_docs" (parse-query-params (list :limit limit))))
           (result (couchdb-get url)))
      (setq docs (assoc-value "rows" result []))
      ;; TODO: need to check if the buffer already exist.
      (with-current-buffer (get-buffer-create (concat "couchdb:" database))
        (setq font-lock-defaults '(couchdb--documents-keywords t nil nil nil))
        (font-lock-mode 1)
        (dolist (ent dbinfo)
          (insert (format "   # %s: %S\n" (symbol-name (car ent)) (cdr ent))))

        (couchdb/doarray (doc docs)
          (let ((key (assoc-value "key" doc "N/A"))
                (rev (assoc-value "rev" (assoc-value "value" doc nil) "N/A")))
            (insert (format "   %-40s %s\n" rev key))))
        (switch-to-buffer (current-buffer))))
    ;; TODO: dired-like mode?
    ))
      


(defun couchdb--prettify-buffer (buffer)
  "Prettify JSON text in BUFFER.

If the prettification failed (possibly, due to the malformed data), this
function will revert to the original text."
  ;; TODO: refine this.
  (with-current-buffer buffer
    (copy-region-as-kill (point-min) (point-max))
    (unless (eq (shell-command-on-region 
                 (point-min) (point-max)
                 couchdb-json-prettifier nil 'replace
                 shell-command-default-error-buffer t) 0)
      (erase-buffer)
      ;; TODO: need to refind below line for `undo' feature.
      (insert-for-yank (current-kill 0)))))

(defun couchdb-get (url &optional buffer)
  "HTTP GET request to URL.

It returns a form (RESPONSE-HEADERS . BODY) where RESPONSE-HEADERS
is an alist of HTTP reponse headers with extra \"Status\" key for
HTTP response code, and BODY is the string of the response body.

If BUFFER is non-nil, this function will save the response body
into BUFFER, and returns a form (RESPONSE-HEADERS . nil)."
  (let ((result (curl/http-recv 'get url buffer)))
    ;; TODO: check http status first
    (if buffer
        (when couchdb-json-prettifier
          (with-current-buffer buffer
            (copy-region-as-kill (point-min) (point-max))
            (unless (eq (shell-command-on-region 
                         (point-min) (point-max)
                         couchdb-json-prettifier nil 'replace
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

(defun couchdb-get-design (database design &optional buffer)
  (let ((url (couchdb-view-endpoint database design)))
    (message "GET %s" url)
    (if buffer
        (with-current-buffer buffer
          (setq couchdb-source-url url)))
    (couchdb-get url buffer)))


(defun couchdb--hide-window-kill-buffer (&optional buffer frame)
  "Hide BUFFER from any window, and kill BUFFER."
  ;; see also `calendar-hide-window' for the reference.
  (let ((buf (or buffer (current-buffer))))
    (mapc (lambda (win)
            (if (and (window-live-p win)
                     (eq (window-buffer win) buf))
                (if (not (and (select-window win) (one-window-p win)))
                    (delete-window win))))
          (window-list frame))
    ;; make sure no window (even in other frames) shows BUFFER.
    (replace-buffer-in-windows buf)
    (message "kill buffer, %S" buf)
    (kill-buffer buf)))


(defun couchdb-edit-kill-buffer ()
  "kill the current editing buffer"
  (interactive)
  (let ((buffer (current-buffer)))
    (when (or (not (buffer-modified-p buffer))
              (yes-or-no-p
               "The buffer modified. Are you sure to cancel editing?"))
      (let ((parent couchdb-parent-buffer)
            (reg    couchdb-parent-region))
        (when (and parent (buffer-live-p parent) reg)
          (with-current-buffer parent
            ;; If the user explictly wants to kill the editing buffer,
            ;; we need to clear the current editing status in the
            ;; parent buffer; removing overlays and markers before
            ;; killing the editing buffer, and make the parent
            ;; writable, etc.
            (setq buffer-read-only nil)
            (remove-overlays (car reg) (cdr reg)
                             'type 'couchdb)))
        (setq couchdb-parent-buffer nil
              couchdb-parent-region nil)

        (couchdb--hide-window-kill-buffer buffer)))))


(defun couchdb-kill-buffer ()
  "kill the current buffer"
  (interactive)
  (let ((buffer (current-buffer)))
    (if (or (not (buffer-modified-p buffer))
            (yes-or-no-p "The buffer modified.  Are you sure to kill?"))
        (kill-buffer buffer))))

(defun couchdb--bind-temp-file ()
  (if buffer-file-name
      (delete-file buffer-file-name))
  (setq buffer-file-name (make-temp-file "couchdb-"))
  (let ((old (buffer-modified-p (current-buffer))))
    (unwind-protect
        (progn (set-buffer-modified-p t)
               (save-buffer 0))
      (set-buffer-modified-p old))))

(defmacro couchdb-save-buffer-modified (&rest body)
  (declare (indent 0) (debug t))
  (let ((modified (make-symbol "BUFFER-MODIFIED")))
    `(let ((,modified (buffer-modified-p (current-buffer))))
       (unwind-protect
           (progn ,@body)
         (set-buffer-modified-p ,modified)))))

(add-to-list 'compilation-error-regexp-alist-alist
             '(couch-json
               "^\\(.*\\):: .*?: line \\([0-9]+\\) column \\([0-9]+\\) (char [0-9]+)$"
               1 2 3))

(add-to-list 'compilation-error-regexp-alist 'couch-json)

(defun couchdb-validate-buffer ()
  (interactive)
  (if (couchdb--validate-buffer)
      (message "No error")))

(defun couchdb--validate-buffer (&optional buffer)
  "Validate JSON document in the current CouchDB.

If found an error, pop-up the error buffer for the inspection, and returns nil.
If no error, returns t."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (couchdb-save-buffer-modified 
        (widen)
        (save-buffer 0)

        (let ((errbuf (get-buffer couchdb-json-error-buffer)))
          (when errbuf
            (with-current-buffer errbuf
              (erase-buffer))))

        (let ((json-buffer (current-buffer)))
          (with-temp-buffer
            (let ((outbuf (current-buffer)))
              (set-buffer json-buffer)
              (shell-command-on-region (point-min) (point-max)
                                       (concat couchdb-json-prettifier
                                               " >/dev/null ")
                                       outbuf nil
                                       couchdb-json-error-buffer nil))))

        (let ((file-name buffer-file-name)
              (errbuf (get-buffer couchdb-json-error-buffer)))
          (if (or (null errbuf) (= (buffer-size errbuf) 0))
              t
            (with-current-buffer errbuf
              (goto-char (point-min))
              (while (re-search-forward couchdb-json-error-regexp-for-compile
                                        nil t)
                (replace-match (format "%s:: \\1"
                                       (file-name-nondirectory file-name))))
              (goto-char (point-min))
              (compilation-minor-mode)
              (pop-to-buffer errbuf))
            nil))))))
  
(defun couchdb-commit-design ()
  (let ((database couchdb-database-name)
        (design couchdb-design-name))
        
    (couchdb-put-design database design (current-buffer))
    (set-buffer-modified-p nil)

    ;; reload the design doc.
    (couchdb-reload-design)))
  
  
(defun couchdb-commit-buffer ()
  "Commit the change into the CouchDB"
  (interactive)
  (if (not (buffer-modified-p (current-buffer)))
      (message "Nothing to commit")
    (if couchdb-commit-function
        (funcall couchdb-commit-function)
      (message "Don't know how to commit"))))

(defun couchdb--view-mode-map () 
  "create new CouchDB view mode map"
  (let ((map (make-sparse-keymap)))
    (define-key map [?n] #'couchdb-view-forward-line)
    (define-key map [?p] #'couchdb-view-previous-line)
    (define-key map [?m] #'couchdb-view-mark)
    (define-key map [?u] #'couchdb-view-unmark)
    (define-key map [?q] #'couchdb-view-quit)
    (define-key map [(control ?x) ?\]] #'couchdb-view-forward-page)
    (define-key map [(control ?x) ?\[] #'couchdb-view-backward-page)

    map))

(defvar couchdb-view-mode-map (couchdb--view-mode-map)
  "Keymap for CouchDB view mode")

(defun couchdb--edit-mode-map () 
  "create new CouchDB editing mode map"
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?x) ?k] #'couchdb-edit-kill-buffer)
    (define-key map [(control ?c) (control ?k)] #'couchdb-edit-kill-buffer)
    (define-key map [(control ?c) (control ?c)] #'couchdb-edit-commit-buffer)
    (define-key map [(control ?c) ?c] #'couchdb-validate-buffer)

    map))

(defvar couchdb-edit-mode-map (couchdb--edit-mode-map)
  "Keymap for CouchDB mode")


(defun couchdb--json-mode-map () 
  "create new CouchDB mode map"
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?x) ?k] #'couchdb-kill-buffer)
    (define-key map [(control ?c) (control ?k)] #'couchdb-kill-buffer)
    (define-key map [(control ?c) (control ?c)] #'couchdb-commit-buffer)
    (define-key map [(control ?c) (control ?e)] #'couchdb-design-edit-value)
    (define-key map [(control ?c) ?c] #'couchdb-validate-buffer)

    map))

(defvar couchdb-json-mode-map (couchdb--json-mode-map)
  "Keymap for CouchDB mode")

(defun couchdb--prepare-local-variables ()
  (make-variable-buffer-local 'couchdb-database-name)
  (make-variable-buffer-local 'couchdb-source-url)
  (make-variable-buffer-local 'couchdb-design-name)
  (make-variable-buffer-local 'couchdb-document-name)
  (make-variable-buffer-local 'couchdb-commit-function)
  ;; if `couchdb-edit-buffer' is non-nil, it should be the buffer which is
  ;; partially editing the current buffer.
  (make-variable-buffer-local 'couchdb-edit-buffer)
  (make-variable-buffer-local 'couchdb-parent-buffer)
  (make-variable-buffer-local 'couchdb-parent-region)
  (make-variable-buffer-local 'couchdb-skip)
  (make-variable-buffer-local 'couchdb-limit)
  )

(define-derived-mode couchdb-json-mode js-mode "CouchDB"
  "Major mode for CouchDB JSON editing

\\{couchdb-json-mode-map}"
  (couchdb--prepare-local-variables))

(define-derived-mode couchdb-view-mode fundamental-mode "CouchDB"
  "Major mode for CouchDB view mode"
  (couchdb--prepare-local-variables)
  (setq font-lock-defaults '(couchdb--documents-keywords t nil nil nil))
  (font-lock-mode 1)
  (setq buffer-read-only t)
  (use-local-map couchdb-view-mode-map))

(define-minor-mode couchdb-edit-mode
  "Couchdb JSON Edit mode"
  nil
  " CouchDB"
  couchdb-edit-mode-map)

(defun couchdb-reload-design ()
  (interactive)
  ;;(unless couchdb-source-url (error "couchdb-source-url missing"))
  (if (buffer-modified-p (current-buffer))
      ;; TODO: do we need to commit?
      nil
    (let ((oldpos (point)))
      (setq buffer-read-only nil)           ; is this needed?
      (erase-buffer)
      (couchdb-get-design couchdb-database-name couchdb-design-name
                          (current-buffer))
      ;; TODO: I don't know how to restore the point after reloading
      ;;       the doc.  The better way will be, save the current key
      ;;       position, and call `goto-char' around to that key
      ;;       position.
      (goto-char oldpos))))

(defun couchdb-load-design (&optional database design)
  (interactive)
  (unless database
    (setq database (couchdb-read-database-name-from-minibuffer "Database:")))
  (unless design
    (setq design (couchdb-read-design-name-from-minibuffer database
                                                           "Design Doc:")))
  (let ((bufname (concat "couchdb:" database "/_design/" design))
        buffer)
    (if (setq buffer (get-buffer bufname))
        (progn (pop-to-buffer buffer)
               (message "You already loaded the design document."))
      (with-current-buffer (setq buffer (get-buffer-create bufname))
        (couchdb-json-mode)
        ;;(message "buffer name: %S" (buffer-name))
        ;;(setq buffer-file-name-name (make-temp-file "couchdb-"))
        (set-visited-file-name (make-temp-file "couchdb-"))
        (rename-buffer bufname)
        (couchdb-get-design database design buffer)
        ;;(message "buffer name: %S" (buffer-name))
        (save-buffer 0)
        ;;(message "buffer name: %S" (buffer-name))
        
        (setq couchdb-database-name database
              couchdb-design-name design
              couchdb-commit-function 'couchdb-commit-design))
      (switch-to-buffer buffer))))

(defsubst couchdb-safe-document-buffer ()
  "Return a document buffer."
  (let ((buffer (get-buffer couchdb-document-buffer)))
    (if (not buffer)
        (get-buffer-create couchdb-document-buffer)
      buffer)))

(defun couchdb-load-documents (database keys)
  "Load the documents with KEYS into the buffer, and return it."

  ;; TODO: if KEYS contains just one key, set-up for editing,
  ;; otherwise just read-only mode.

  ;; curl -d '{"keys":["bar","baz"]}' -X POST http://127.0.0.1:5984/foo/_all_docs?include_docs=true
  (let ((body (json-encode-alist (list (cons "keys" (vconcat keys)))))
        (url (couchdb-view-endpoint database nil nil :include-docs t))
        (buffer (couchdb-safe-document-buffer))
        status
        result)

    (if (buffer-modified-p buffer)
        (error "not implemented yet."))

    (curl/with-temp-buffer
      (insert body)
      (setq result (curl/http-send-buffer 'POST
                                          url
                                          (current-buffer)
                                          "application/json")))
    (setq status (assoc-value "Status" (car result) "404"))
    (if (not (string-equal status "200"))
        (progn (message "CouchDB returns HTTP %s code" status)
               nil)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (cdr result))
          (couchdb--prettify-buffer buffer)
          (set-buffer-modified-p nil)
          (couchdb-json-mode)
          ;; TODO: set related local variables!!!
          ))
      
      buffer)))


(defun json-backward-syntax ()
  "Backward one syntactic element in JSON document."
  ;; TODO: implement this)
  (interactive)
  (if (= (point) (point-min))
      ;; TODO: handle beginning of the buffer!!
      (cons nil nil)
    (let ((ppss (syntax-ppss (point)))
          begin end)
      (if (nth 3 ppss)                    ; inside of a string
          (progn
            (setq begin (1+ (nth 8 ppss)))
            (while (null end)
              (skip-syntax-forward "^\"")
              (goto-char (1+ (point)))
              (if (null (nth 3 (syntax-ppss (point))))
                  (setq end (1- (point)))))
            (goto-char (nth 8 ppss))
            (cons 15 (buffer-substring-no-properties begin end)))

        (forward-comment -10000)
        ;; it is possible that we are at the end of the "// ..." style comment.
        ;; (skip-syntax-backward " <>")
        ;; (let ((cache (syntax-ppss (point))))
        ;;   (if (nth 4 cache)
        ;;       (progn (goto-char (1+ point))
        ;;              (forward-comment -10000))
        (goto-char (1- (point)))

        (let ((cls (syntax-class (syntax-after (point))))
              (pos (1+ (point)))
              value)
          (cond ((null cls)
                 ;; we already checked the beginning of the buffer already.
                 (cons nil nil))
                ((= cls 1)
                 ;; Event the first character of "/*" or "//" is parsed
                 ;; as a punctuation syntax class.
                 (skip-syntax-backward ".")
                 (setq value (buffer-substring-no-properties (point) pos))
                 (cons 1 value))
                ((= cls 2)
                 (skip-syntax-backward "w")
                 (setq value (buffer-substring-no-properties (point) pos))
                 ;; If VALUE is neither "true", "false", nor "null", it
                 ;; is a fatal error (possibly, the document has bad
                 ;; matched quotation or so.
                 (cons 2 value))
                ((= cls 4)
                 (skip-syntax-backward "(")
                 (setq value (buffer-substring-no-properties (point) pos))
                 (cons 4 value))
                ((= cls 5)
                 (skip-syntax-backward ")")
                 (setq value (buffer-substring-no-properties (point) pos))
                 (cons 5 value))
                ((= cls 7)
                 (json-backward-syntax))
))))))

(defun json-forward-syntax ()
  "Forward one syntactic element in JSON document.

This function returns a form (CLASS . VALUE) where CLASS is the
syntactic class of the current point, and VALUE is the token
value of the current point.

CLASS and VALUE are nil iff the end of buffer reached."
     ;; Integer Class          Integer Class          Integer Class
     ;; 0    whitespace        5    close parenthesis 10    character quote
     ;; 1    punctuation       6    expression prefix 11    comment-start
     ;; 2    word              7    string quote      12    comment-end
     ;; 3    symbol            8    paired delimiter  13    inherit
     ;; 4    open parenthesis  9    escape            14    generic comment
     ;; 15   generic string                           
  (interactive)
  (let ((ppss (syntax-ppss (point)))
        begin end)
    (if (nth 3 ppss)
        (progn
          (setq begin (1+ (nth 8 ppss)))
          (while (null end)
            (skip-syntax-forward "^\"")
            (goto-char (1+ (point)))
            (if (null (nth 3 (syntax-ppss (point))))
                (setq end (1- (point)))))
          (cons 15 (buffer-substring-no-properties begin end)))

      ;; TODO: use `forward-comment' to deal with comments and whitespaces.
      (skip-syntax-forward " <>")
      (let ((cls (syntax-class (syntax-after (point))))
            (pos (point))
            value)
        (cond ((null cls)
               (cons nil nil))
              ((= cls 1)
               ;; Event the first character of "/*" or "//" is parsed
               ;; as a punctuation syntax class.
               (skip-syntax-forward ".")
               (setq value (buffer-substring-no-properties pos (point)))
               (if (or (string-equal value "/*")
                       (string-equal value "//"))
                   (progn
                     (goto-char pos)
                     (forward-comment 10000)
                     (json-forward-syntax))
                 (cons 1 value)))
              ((= cls 2)
               (skip-syntax-forward "w")
               (setq value (buffer-substring-no-properties pos (point)))
               ;; If VALUE is neither "true", "false", nor "null", it
               ;; is a fatal error (possibly, the document has bad
               ;; matched quotation or so.
               (cons 2 value))
              ((= cls 4)
               (skip-syntax-forward "(")
               (setq value (buffer-substring-no-properties pos (point)))
               (cons 4 value))
              ((= cls 5)
               (skip-syntax-forward ")")
               (setq value (buffer-substring-no-properties pos (point)))
               (cons 5 value))
              ((= cls 7)
               (skip-syntax-forward "\"")
               (json-forward-syntax))               
              (t
               (error (format "class %S not implemented yet" cls))))))))

(defun json-backward-parent-key (&optional n)
  ;; TODO: this function should not ignore the current key
  (json-backward-key)
  (let ((current (car (syntax-ppss (point))))
        (doloop t) syn)
    (while doloop
      (setq syn (json-backward-key))
      (if (car syn)
          (let ((ppss (syntax-ppss (point))))
            (if (< (car ppss) current)
                (setq doloop nil)))
        (setq doloop nil)))))

(defun json-key-list ()
  "Return the list of the keys from the current point."
  (let (keylist keypair)
    (save-excursion
      (setq keypair (json-move-to-current-key))
      (if keypair
          (progn
            (setq keylist (cons (cdr keypair) keylist)
                  keypair nil)
            (goto-char (1+ (point)))
            (while (setq key (json-move-to-parent-key))
              (setq keylist (cons (cdr key) keylist)))))
      keylist)))
      

(defun json-move-to-parent-key ()
  (let* ((oldpos (point))
         (keypair (json-move-to-current-key))
         level)                         ; current indent level
    (if keypair
        (let ((ppss (syntax-ppss (point)))
              (cont t))
          (setq level (nth 0 ppss))
          (while cont
            (setq keypair (json-backward-key))
            (cond ((null keypair)
                   (goto-char oldpos)
                   (setq cont nil))
                  ((< (nth 0 (syntax-ppss (point))) level)
                   (setq cont nil))))
          keypair))))
                      

(defun json-move-to-current-key ()
  "Move the point to the cloest JSON key in backward.

If there is no key component in backward, this function returns nil."
  (let ((ppss (syntax-ppss (point)))
        (class (syntax-class (syntax-after (point)))))
    (if (and (not (nth 3 ppss))
             (eq class 7)               ; ?\" belongs to the string quote class
             (eq (char-after (point)) ?\"))
        (goto-char (1+ (point))))
    (json-backward-key)))
    
(defun json-backward-key ()
  "Move to the previous JSON key component."
  ;; TODO: json-move-to-current-key?
  (let ((org-pos (point)) pos key (doloop t))
    (while (and (not key) doloop)
      (let ((syn (json-backward-syntax)))
        (setq pos (point))
        (cond ((null (car syn))
               (setq doloop nil))
              ((= (car syn) 15)         ; string
               (let ((next-syn (and (car (json-forward-syntax))
                                    (json-forward-syntax))))
                 (goto-char pos)
                 (if (and (= (car next-syn) 1)
                          (string-equal (cdr next-syn) ":"))
                     ;; SYN is the key!
                     (setq key syn)))))))
    key))
                   

(defun couchdb-current-value-string-p ()
  "Return non-nil if the current value is a string type.

If the current value is the string type, the return value is a
form of (BEGIN . END) where BEGIN is the character position of
the beginning of the string, and END is the end of the string
position."
  (let ((oldpos (point))
        result pair)
    (if (null (json-move-to-current-key))
        nil
      (setq pair (json-forward-syntax)) ; key string
      (if (or (null pair)
              (null (cdr pair)))
          nil
        (setq pair (json-forward-syntax)) ; ?\:
        (if (or (null pair)
                (null (cdr pair)))
            nil
          (setq pair (json-forward-syntax)) ; should be string
          (if (or (null pair)
                  (not (eq (car pair) 15)))
              nil
            (let ((end (point))
                  begin)
            ;; (point) is the end of the string
              (json-backward-syntax)
              (setq result (cons (1+ (point)) (1- end))))))))
    (goto-char oldpos)
    result))

(defun couchdb-string-value-region ()
  (let ((reg (couchdb-current-value-string-p))
        begin end)
    (when reg
      (setq begin (copy-marker (car reg) nil)
            end (copy-marker (cdr reg) t))
      (cons begin end))))


(defun json-read-string-from-buffer (&optional buffer)
  "Return a JSON string value of BUFFER.

Note that the returned value does not contain the enclosing double-quote."
  ;; TODO: we need to test this function more.
  (if (null buffer)
      (setq buffer (current-buffer)))
  (with-temp-buffer
    (insert-buffer (get-buffer buffer))
    ;; replace all (\) to (\\)
    (goto-char (point-min))
    (while (search-forward "\\" nil t)
      (replace-match "\\\\" t t))
    ;; replace all double quote(") into escaped one (\")
    (goto-char (point-min))
    (while (search-forward "\"" nil t)
      (replace-match "\\\"" t t))
    ;; replace all new line into (\n)
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\\\\n" t nil))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun couchdb-edit-commit-buffer ()
  (interactive)

  ;; TODO: make sure that the parent buffer is not buffer-modified-p if the
  ;;       commit did not change anything.
  (let ((parent (buffer-local-value 'couchdb-parent-buffer (current-buffer)))
        (buffer (current-buffer)))
    (unless (and parent (buffer-live-p parent))
      (error "No live parent buffer existed."))

    (if (not (buffer-modified-p (current-buffer)))
        (progn
          (message "nothing changed.")
          ;; TODO: do we need to kill the editing buffer?
          )
      (widen)
      ;;(while (search-forward "\n" nil t)
      ;; (replace-match "\\n" nil t))
      
      (let ;; ((body (buffer-string)))
          ((body (json-read-string-from-buffer))
           (begin (car couchdb-parent-region))
           (end (cdr couchdb-parent-region)))

        (with-current-buffer parent
          (setq buffer-read-only nil)
          (remove-overlays begin end
                           'type 'couchdb)
          (delete-region begin end)
          (insert body)
          (setq couchdb-edit-buffer nil))

        (couchdb--hide-window-kill-buffer buffer)))))


(defun couchdb--language (doc)
  "Get the language of the JSON document.

If DOC is a buffer, it parses it and get the value of \"language\" key,
otherwise DOC should be JSON alist."
  (let ((json (cond ((bufferp doc)
                     (with-current-buffer doc
                       (save-excursion
                         (save-restriction
                           (widen)
                           (goto-char (point-min))
                           (json-read)))))
                    ((consp doc)
                     doc)
                    (t (error "invalid argument type")))))
    (assoc-value 'language json "javascript")))
      
(defun couchdb--set-major-mode (keys src-buffer dst-buffer)
  "Set the major mode of DST-BUFFER according to KEYS from SRC-BUFFER"
  ;; TODO: it is not a good idea to parse the whole buffer in this time.
  ;;       it's better to parse it after loading the design doc.
  (let ((lang (couchdb--language src-buffer)))
    (cond ((or (and (eq (length keys) 3)
                    (string-equal (nth 0 keys) "views"))
               (and (eq (length keys) 2)
                    (string-equal (nth 0 keys) "shows")))
           (with-current-buffer dst-buffer
             (javascript-mode)))
          (t 
           (with-current-buffer dst-buffer
             (text-mode))))))


(defun couchdb--clear-editing-status (&optional force)
  "Clear the editing status on the current buffer."
  (if (and couchdb-edit-buffer 
           (buffer-live-p couchdb-edit-buffer)
           force)
      (kill-buffer couchdb-edit-buffer))
  (setq couchdb-edit-buffer nil)
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) 'type 'couchdb))
  (setq buffer-read-only nil))

      
(defun couchdb-design-edit-value ()
  "Edit current JSON value in other buffer."
  (interactive)

  (if (and couchdb-edit-buffer (buffer-live-p couchdb-edit-buffer))
      (progn (pop-to-buffer couchdb-edit-buffer)
             (message "You need to commit or to cancel the on-going editing"))
    (couchdb--clear-editing-status)
    (couchdb--prepare-editing-value)))
    

(defun couchdb--prepare-editing-value ()
  (let ((reg (couchdb-string-value-region))
        (keys (json-key-list))
        overlay)
    (when reg
      ;; (remove-overlays (point-min) (point-max) 'type 'couchdb)
      (message "set invert face around (%d %d)" 
               (marker-position (car reg))
               (marker-position (cdr reg)))
      (setq overlay (make-overlay (car reg) (cdr reg)))
      (overlay-put overlay 'face 'couchdb-value-edited-face)
      (overlay-put overlay 'type 'couchdb)

      (setq buffer-read-only t)

      
      (let ((buffer (get-buffer-create couchdb-json-edit-buffer))
            (oldbuf (current-buffer))
            (body (concat "\""
                          (buffer-substring-no-properties (car reg) (cdr reg))
                          "\"")))
        (setq couchdb-edit-buffer buffer)
        (with-current-buffer buffer
          (couchdb--set-major-mode keys oldbuf buffer)

          (setq couchdb-parent-buffer oldbuf)
          (setq couchdb-parent-region reg)
          (widen)
          (erase-buffer)
          (couchdb-edit-mode)
          (insert (read body))
          (goto-char (point-min))
          (set-buffer-modified-p nil)
          ;; TODO: set the keymap for editing in the current buffer
          )
        (pop-to-buffer buffer)))))

(defun couchdb-get-all-designs (database)
  "Return a list of design document names in DATABASE."
  (let ((url (couchdb-view-endpoint "hello" nil nil 
                                    :start-key "\"_design/\""
                                    :end-key "\"_design0\"")))
    (let* ((result nil)
           (json (couchdb-get url))
           (err (assoc-value "error" json nil))
           (dvec (assoc-value "rows" json nil)))
      (if err
          (error err))
      (couchdb/doarray (elem dvec)
        (let ((docname (assoc-value "id" elem nil)))
          (if docname
              (push (substring docname (length "_design/")) result))))
      result)))

(defun couchdb-js-parse (buffer)
  "Return an ALIST of all global Javascript functions in BUFFER.

All functions in BUFFER must have the following form:

  function_name = function(...) { ... }

where, FUNCTION_NAME will be the function name of the CouchDB's registered
function such as \"map\", \"reduce\", \"validate_doc_update\", etc.
"
  (let (data oldend)
    (with-current-buffer buffer
      (save-excursion
        (save-match-data
          (goto-char (setq oldend (point-max)))

          (while (> oldend 0)
            (let (start (end (point-max)))
              (beginning-of-defun)
              (setq start (point))
              (end-of-defun)
              (setq end (point))
              
              (if (or (< end oldend) (and (= end oldend) (= end (point-max))))
                  (progn
                    (message "oldend: %d, region: %d-%d" oldend start end)
                    (goto-char start)
                    (when (looking-at 
                           "[[:space:]]*\\([a-zA-Z0-9_]*\\)[[:space:]]*=[[:space:]]*")
                      (push (cons (match-string-no-properties 1)
                                  (buffer-substring-no-properties
                                   (match-end 0) end))
                            data))
                    (setq oldend end)
                    (goto-char start))
                (setq oldend -1)))))))
    data))

(defun couchdb-js-function (buffer function-name)
  "Return a string that contains the function definition of FUNCTION-NAME."
  (with-current-buffer buffer
    (save-excursion
      (save-match-data
        (let (start end)
          (goto-char (point-min))
          (condition-case err
              (when (re-search-forward
                     (format "^[[:space:]]*%s[[:space:]]*=[[:space:]]*" 
                             function-name)
                     nil t)
                ;;(beginning-of-line)
                (setq start (point))
                (end-of-defun)
                (setq end (point))
                (buffer-substring-no-properties start end))
            (error nil)))))))


(defun couchdb-put-design (database design doc &optional rev-id)
  (let ((url (couchdb-view-endpoint database design)))
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

  
(defun couchdb-get-design-info (database design &optional buffer)
  (let ((url (concat (couchdb-view-endpoint database design) "/_info")))
    (couchdb-get url buffer)))

;; (curl/http-start-recv 'get
;;           "http://username:password@localhost:5984/fs/_design/fs/_view/path"
;;           nil
;;           (lambda (proc event)
;;             (curl/http-body (point-min) (point-max) nil 'utf-8-dos)))

(defun couchdb-get-view (database design view &optional buffer &rest params)
  (let ((url (apply 'couchdb-view-endpoint
                    (append (list database design view) params))))
    (couchdb-get url buffer)))

(defvar couchdb-history-database-name nil
  "History list of read couchdb database name")

(defvar couchdb-history-design-name nil
  "History list of read couchdb design document name")

(defun couchdb-read-database-name-from-minibuffer (prompt)
  "Completing read the database name from the minibuffer."
  (let* ((name-list (couchdb-databases))
         (default (car (member-if (lambda (name) (member name name-list))
                                  couchdb-history-database-name)))
         (choice ""))
    (setq prompt (if default
                     (format "%s [%s] " prompt default)
                   prompt))
    (while (string-equal choice "")
      (setq choice (completing-read prompt name-list 
                                    (lambda (name)
                                      (member name name-list))
                                    'valid-only nil
                                    'couchdb-history-database-name
                                    default)))
    choice))

(defun couchdb-read-design-name-from-minibuffer (database prompt)
  "Completing read the design document name from the minibuffer."
  (let* ((name-list (couchdb-get-all-designs database))
         (default (car (member-if (lambda (name) (member name name-list))
                                  couchdb-history-design-name)))
         (choice ""))
    (setq prompt (if default
                     (format "%s [%s] " prompt default)
                   prompt))
    (while (string-equal choice "")
      (setq choice (completing-read prompt name-list 
                                    (lambda (name)
                                      (member name name-list))
                                    'valid-only nil
                                    'couchdb-history-design-name
                                    default)))
    choice))


  
(defun couchdb-get-document (database doc-id &optional buffer)
  ;; TODO: If RAW is non-nil, return a buffer containing the JSON doc.
  (let ((url (concat (couch-endpoint database) "/" 
                     (url-hexify-string doc-id)))
        result)
    (setq result (curl/http-recv 'get url buffer))
    ;; TODO: check http status first

    (if buffer
        (when couchdb-json-prettifier
          (with-current-buffer buffer
            (copy-region-as-kill (point-min) (point-max))
            (unless (eq (shell-command-on-region (point-min) (point-max)
                                     couchdb-json-prettifier nil 'replace
                                     shell-command-default-error-buffer t) 0)
              (erase-buffer)
              ;; TODO: need to refind below line for `undo' feature.
              (insert-for-yank (current-kill 0)))))
      (let ((json-key-type 'string))
        (json-read-from-string (cdr result))))))


(defun couchdb-put-attachment (database doc-id filename &optional rev-id)
)

(defun couchdb-put-document (database doc-id doc &optional rev-id)
  (let ((url (concat (couch-endpoint database) "/" 
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

(defun couchdb-document-revision (database doc-id)
  "Get the latest document revision of DOC-ID from DATABASE."
  (let ((url (concat (couch-endpoint database) "/"
                     (url-hexify-string doc-id))))
    (setq result (curl/http-recv 'head url)
          rev-id (assoc-value "Etag" (car result)))
    (and rev-id
         (strip-etag rev-id))))
  
(defun couchdb-delete-document (database doc-id &optional rev-id)
  "Delete the document, DOC-ID with revision REV-ID from DATABASE.

If REV-ID is nil, it will automatically retrived.
This function returns the revision id of the delete operation."
  (unless rev-id
    (setq rev-id (couchdb-document-revision database doc-id)))
  (unless rev-id
    (error "Etag missing"))
  (let ((url (concat (couch-endpoint database) "/"
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
      
(defun walk-directories (directory function &optional norecursion match nosort)
  "Walk directories.

Starting from DIRECTORY, walking all files and call FUNCTION with the
absolute filename.  This function uses `directory-files' and it passes
the optional arguments MATCH and NOSORT along to `directory-files'.

If NORECURSION is non-nil, it does not follow into subdirectories.

Note that it ignores back-up files (e.g. \"filename~\") and tempory files (e.g. \"#filename#\")."
  (setq directory (expand-file-name directory))
  (let ((dirs (list directory)))
    (while dirs
      (let* ((dir (pop dirs))
             (files (directory-files dir nil match nosort)))
        (dolist (fname files)
          (unless (or (string-equal fname ".")
                      (string-equal fname "..")
                      ;; ignore either back-up files or temporary files
                      (string-match "\\`.*~\\'" fname)
                      (string-match "\\`#.*#\\'" fname))
            (when (and (not norecursion)
                       (file-directory-p fname))
              (push fname dirs))
            (funcall function (concat (file-name-as-directory dir)
                                      fname))))))))

(defun couchdb-import-directory (directory &optional recursive)
  (walk-directories directory
                    (lambda (name)
                      (let ((attr (file-attributes name 'integer))
                            json)
                        (unless (car attr) ; non-directory
                          (push (cons "uid" (nth 2 attr)) json)
                          (push (cons "gid" (nth 3 attr)) json)
                          (push (cons "atime" (format-time-string "%s" 
                                                                  (nth 4 attr)))
                                      json)
                          (push (cons "ctime" (format-time-string "%s" 
                                                                  (nth 5 attr)))
                                      json)
                          (push (cons "size" (nth 7 attr)) json)
                          (push (cons "inode" (nth 10 attr)) json)
                          (push (cons "mode" (file-modes name)) json)
                          (push (cons "filename" (file-name-nondirectory name))
                                json)
                          (princ (format "%S\n" json)))))
                    (not recursive)))
                             
                     

(provide 'couchdb)
;;; couchdb.el ends here
