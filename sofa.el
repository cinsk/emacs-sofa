;;; sofa.el --- CouchDB console (like Futon)

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


(require 'sofa-api)
(require 'curl)
(require 'json)

(defvar sofa-json-prettifier "python -mjson.tool")
(defvar sofa-json-error-buffer "*Sofa JSON errors*")
(defvar sofa-json-edit-buffer "*Sofa Editing JSON*")

(defvar sofa-document-buffer "*Sofa Document*"
  "buffer name for view/editing one or multiple documents.")

(defface sofa-value-edited-face '((t :inverse-video t))
  "Face to mark the current editing value.")

(defface sofa-revision-face '((t :foreground "magenta"))
  "Face to mark the current editing value.")

(defface sofa-id-face '((t :foreground "cyan"))
  "Face to mark the current editing value.")

(defconst sofa-json-error-regexp-for-compile
  "^\\(.*?line [0-9]+ column [0-9]+ (char [0-9]+)\\)$"
  "Regular expression for an error output from `sofa-json-prettifier'.")

(defun sofa--documents-keywords ()
  (list (cons "^...#.*$" 'font-lock-comment-face)
        (list "^...[[:space:]]*\\(\\<[^[:space:]]*\\>\\)[[:space:]]+\\(.*\\)$" 
              '(1 'sofa-revision-face)
              '(2 'sofa-id-face))))

(defun sofa-view-quit ()
  "Quit the current buffer"
  (interactive)
  ;; TODO: check if there is unsaved data
  (sofa--hide-window-kill-buffer))


(defun sofa--view-point-on-doc-p ()
  "Return t if the current line shows a document."
  (let ((oldpos (point)))
    (save-match-data
      (beginning-of-line)
      (let ((result (looking-at
                     "^...[[:space:]]*\\(\\<[^[:space:]]*\\>\\)[[:space:]]+\\(.*\\)$")))
        (goto-char oldpos)
        result))))

(defun sofa--view-point-on-id ()
  (let ((oldpos (point)))
    (save-match-data
      (beginning-of-line)
      (if (looking-at
           "^...[[:space:]]*\\(\\<[^[:space:]]*\\>\\)[[:space:]]+\\(.*\\)$")
          (goto-char (match-beginning 2))
        (goto-char oldpos)))))

(defun sofa-view-forward-line (&optional n)
  (interactive "p")
  (let ((remain (forward-line n)))
    (when (not (eq remain n))
      (sofa--view-point-on-id))))

(defun sofa-view-previous-line (&optional n)
  (interactive "p")
  (sofa-view-forward-line (- n)))

(defun sofa--marked-keys ()
  ;; Note that the first three characters on the document line is used
  ;; for marking, which may not contain the text property, 'sofa-key
  (sofa--map-over-marks
      (lambda () (get-text-property (+ 3 (point)) 'sofa-key))))

(defmacro sofa--map-over-marks (function)
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
         (while (and (not (eobp)) (not (sofa--view-point-on-doc-p)))
           (forward-line 1))

         (while (and (not (eobp)) (sofa--view-point-on-doc-p))
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
           (when (sofa--view-point-on-doc-p)
             (beginning-of-line)
             (save-excursion
               (setq ,processed (1+ ,processed)
                     ,result (cons (funcall ,function) ,result))))))
       (goto-char ,oldpos)
       (move-marker ,oldpos nil)
       (move-marker ,pos nil)
       (nreverse ,result))))
    
        

(defun sofa-repeat-over-lines (arg function)
  "Works similar to `dired-repeat-over-lines'."
  (let ((pos (make-marker)))
    (beginning-of-line)
    (while (and (> arg 0) (not (eobp)))
      (setq arg (1- arg))
      (beginning-of-line)
      (while (and (not (eobp)) (not (sofa--view-point-on-doc-p))
                  (forward-line 1)))
      (save-excursion
        (forward-line 1)
        (move-marker pos (1+ (point)))) ; don't understand why calls `1+'.
      (save-excursion (funcall function))
      (goto-char pos))
    (while (and (< arg 0) (not bobp))
      (setq arg (1+ arg))
      (forward-line -1)
      (while (and (not (bobp)) (not (sofa--view-point-on-doc-p))
                  (forward-line -1)))
      (beginning-of-line)
      (save-excursion (funcall function)))
    (move-marker pos nil)
    (sofa--view-point-on-id)))


(defun sofa-view-mark (arg)
  (interactive "P")
  ;; TODO: handle ARG
  (let ((inhibit-read-only t))
    (sofa-repeat-over-lines (prefix-numeric-value arg)
                               (lambda () (delete-char 1) (insert ?\*)))))
    ;; (when (sofa--view-point-on-doc-p)
    ;;   (beginning-of-line)
    ;;   (delete-char 1)
    ;;   (insert-char ?\* 1)
    ;;   (sofa-view-forward-line))))

(defun sofa-view-unmark (arg)
  (interactive "P")
  ;; TODO: handle ARG
  (let ((inhibit-read-only t))
    (sofa-repeat-over-lines (prefix-numeric-value arg)
                               (lambda () (delete-char 1) (insert " ")))))

(defun sofa-view-backward-page (&optional count)
  (interactive "p")
  (sofa-view-forward-page (- count)))

(defun sofa-view-revert-buffer ()
  "Refresh the current page."
  (interactive)
  (sofa-view-forward-page 0))

(defun sofa-view-forward-page (&optional count)
  (interactive "p")
  (save-restriction
    (widen)
    (let ((oldpos (point)))
      (goto-char (point-min))
      (while (and (not (eobp)) (not (sofa--view-point-on-doc-p)))
        (forward-line 1))
      ;; TODO: what if there is unsaved contents? (e.g. marks)
      (let ((inhibit-read-only t)
            (docs-begin (point))
            (docs-end (point-max))
            (skip sofa-skip)
            (limit sofa-limit))
        ;; TODO: bound check for `sofa-skip'
        (setq skip (max (+ skip (* count limit)) 0))
        (goto-char (point-max))
        (let ((nread (sofa--view-fill-page skip limit)))
          (message "%d document(s) read" nread)
          (if (<= nread 0)
              (goto-char oldpos)
            (delete-region docs-begin docs-end)
            (setq sofa-skip skip
                  sofa-limit limit)
            (goto-char docs-begin)))))))


(defun sofa-view-load-documents-other-window ()
  (interactive)
  ;; TODO: if there's marked doc(s), load that in batch mode?
  (let ((keys (sofa--marked-keys))
        buffer)
    ;; TODO: Do I need to refresh the current view page?
    ;;       It is possible to have outdated page for now...
    (message "Loading documents...")
<<<<<<< HEAD:sofa.el
    (setq buffer (sofa--load-documents sofa-database-name keys))
    ;; If `sofa--load-documents' failed, BUFFER will be nil.
=======
    (setq buffer (couchdb--load-documents couchdb-database-name keys))
>>>>>>> 42adb634f243f0504581ad2fee60ef1d8a4e9e62:couchdb.el
    (and buffer
         (pop-to-buffer buffer))))

  
(defun sofa--load-document (database key)
  "Load the document where the id is KEY from DATABASE, return the buffer."
  ;; TODO: I need to decide the mechanism of loading individual
  ;;       document.  If I use only one buffer to load document(s), it
  ;;       will be easy to maintain the consistency against dealing
  ;;       with multiple buffers.
  (let* ((bufname (concat "sofa:" database "/" key))
         (buffer (get-buffer bufname)))
    (if buffer
      ;; TODO: what if there's already that buffer?
      (with-current-buffer buffer
        (if (not sofa-source-url)
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
      (sofa-json-mode)
      ;; TODO: error handling of `sofa-get-document'.
      (sofa-get-document database key buffer)
      (setq sofa-database-name database
            sofa-document-name key))
    buffer))

(defun sofa--view-fill-page (skip limit)
  "load LIMIT document(s) at the point with SKIP document(s) skipped.

This function loads document(s) according to the `sofa-limit'
and `sofa-skip'"
  (let ((url (sofa-view-endpoint sofa-database-name
                                    sofa-design-name 
                                    sofa-view-name
                                    :limit limit :skip skip))
        (count 0)
        result docs)
    ;; TODO: do we need to check for (and (bolp) (eolp))?
    (message "Loading view... (skip %d) (limit %d)" skip limit)

    (setq result (sofa-get url)
          docs (assoc-value "rows" result []))

    (let ((inhibit-read-only t))
      (sofa/doarray (doc docs)
        (let ((key (assoc-value "key" doc "N/A"))
              (rev (assoc-value "rev" (assoc-value "value" doc nil)
                                "N/A")))
          (let ((text (format "   %-40s %s" rev key)))
            (insert text)
            (put-text-property (line-beginning-position) (line-end-position)
                               'sofa-key key)
            (put-text-property (line-beginning-position) (line-end-position)
                               'sofa-rev rev)
            (newline))
          (setq count (1+ count)))))
    count))
      
    
(defun sofa-load-view (&optional database design view limit skip)
  "Launch view mode like Futon"
  (interactive)
  (unless database
    (setq database (sofa-read-database-name-from-minibuffer "Database: ")))
  (and (null limit)
       (setq limit 10))
  (and (null skip)
       (setq skip 0))
  (let ((dbinfo (sofa-get-database-info database))
        docs)
    (let* ((url (sofa-view-endpoint database design view 
                                       :limit limit :skip skip))
           (result (sofa-get url))
           (bufname (concat "sofa:" database))
           (buffer (get-buffer bufname)))
      (setq docs (assoc-value "rows" result []))
      (if buffer
          (with-current-buffer buffer
            ;; TODO: check if the buffer is sofa-load-view mode and ...
            (setq buffer-read-only nil)
            (erase-buffer))
        (setq buffer (get-buffer-create (concat "sofa:" database))))
      (with-current-buffer buffer
        (sofa-view-mode)

        (setq sofa-database-name database
              sofa-design-name design
              sofa-view-name view)

        (let ((inhibit-read-only t))
          (dolist (ent dbinfo)
            (insert (format "   # %s: %S\n"
                            (symbol-name (car ent)) (cdr ent)))))

        (sofa--view-fill-page skip limit)
        (setq sofa-skip skip
              sofa-limit limit)
        
        (switch-to-buffer (current-buffer)))
    ;; TODO: dired-like mode?
      )))
      


(defun sofa-load-database (&optional database limit)
  "Load the database in sofa mode."
  (interactive)
  (unless database
    (setq database (sofa-read-database-name-from-minibuffer "Database: ")))
  (if (null limit)
      (setq limit 10))

  (let ((dbinfo (sofa-get-database-info (url-hexify-string database)))
        docs)
    (let* ((url (concat (sofa-endpoint) (url-hexify-string database)
                        "/_all_docs" (parse-query-params (list :limit limit))))
           (result (sofa-get url)))
      (setq docs (assoc-value "rows" result []))
      ;; TODO: need to check if the buffer already exist.
      (with-current-buffer (get-buffer-create (concat "sofa:" database))
        (setq font-lock-defaults '(sofa--documents-keywords t nil nil nil))
        (font-lock-mode 1)
        (dolist (ent dbinfo)
          (insert (format "   # %s: %S\n" (symbol-name (car ent)) (cdr ent))))

        (sofa/doarray (doc docs)
          (let ((key (assoc-value "key" doc "N/A"))
                (rev (assoc-value "rev" (assoc-value "value" doc nil) "N/A")))
            (insert (format "   %-40s %s\n" rev key))))
        (switch-to-buffer (current-buffer))))
    ;; TODO: dired-like mode?
    ))
      


<<<<<<< HEAD:sofa.el
(defun sofa--prettify-buffer (&optional buffer)
=======
(defun couchdb--prettify-buffer (&optional buffer)
>>>>>>> 42adb634f243f0504581ad2fee60ef1d8a4e9e62:couchdb.el
  "Prettify JSON text in BUFFER.

If BUFFER is nil, this function uses the current buffer.

If the prettification failed (possibly, due to the malformed data), this
function will revert to the original text."
  ;; TODO: refine this.
  (with-current-buffer (or buffer (current-buffer))
    (copy-region-as-kill (point-min) (point-max))
    (unless (eq (shell-command-on-region 
                 (point-min) (point-max)
                 sofa-json-prettifier nil 'replace
                 shell-command-default-error-buffer t) 0)
      (erase-buffer)
      ;; TODO: need to refind below line for `undo' feature.
      (insert-for-yank (current-kill 0)))))

(defun sofa--hide-window-kill-buffer (&optional buffer frame)
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


(defun sofa-edit-kill-buffer ()
  "kill the current editing buffer"
  (interactive)
  (let ((buffer (current-buffer)))
    (when (or (not (buffer-modified-p buffer))
              (yes-or-no-p
               "The buffer modified. Are you sure to cancel editing?"))
      (let ((parent sofa-parent-buffer)
            (reg    sofa-parent-region))
        (when (and parent (buffer-live-p parent) reg)
          (with-current-buffer parent
            ;; If the user explictly wants to kill the editing buffer,
            ;; we need to clear the current editing status in the
            ;; parent buffer; removing overlays and markers before
            ;; killing the editing buffer, and make the parent
            ;; writable, etc.
            (setq buffer-read-only nil)
            (remove-overlays (car reg) (cdr reg)
                             'type 'sofa)))
        (setq sofa-parent-buffer nil
              sofa-parent-region nil)

        (sofa--hide-window-kill-buffer buffer)))))


(defun sofa-kill-buffer ()
  "kill the current buffer"
  (interactive)
<<<<<<< HEAD:sofa.el
  ;; TODO: since by default, Emacs asks for killing if the buffer is
  ;;       modified.  So, I don't think that this function is
  ;;       necessary.
=======
  ;; TODO: since by default, Emacs asks for killing if the buffer is modified.
  ;; So, I don't think that this function is necessary.
>>>>>>> 42adb634f243f0504581ad2fee60ef1d8a4e9e62:couchdb.el
  (let ((buffer (current-buffer)))
    (if (or (not (buffer-modified-p buffer))
            (yes-or-no-p "The buffer modified.  Are you sure to kill?"))
        (kill-buffer buffer))))

(defun sofa--bind-temp-file ()
  (if buffer-file-name
      (delete-file buffer-file-name))
  (setq buffer-file-name (make-temp-file "sofa-"))
  (let ((old (buffer-modified-p (current-buffer))))
    (unwind-protect
        (progn (set-buffer-modified-p t)
               (save-buffer 0))
      (set-buffer-modified-p old))))

(defmacro sofa-save-buffer-modified (&rest body)
  (declare (indent 0) (debug t))
  (let ((modified (make-symbol "BUFFER-MODIFIED")))
    `(let ((,modified (buffer-modified-p (current-buffer))))
       (unwind-protect
           (progn ,@body)
         (set-buffer-modified-p ,modified)))))

(add-to-list 'compilation-error-regexp-alist-alist
             '(sofa-json
               "^\\(.*\\):: .*?: line \\([0-9]+\\) column \\([0-9]+\\) (char [0-9]+)$"
               1 2 3))

(add-to-list 'compilation-error-regexp-alist 'sofa-json)

(defun sofa-validate-buffer ()
  (interactive)
  (if (sofa--validate-buffer)
      (message "No error")))

(defun sofa--validate-buffer (&optional buffer)
  "Validate JSON document in the current buffer.

If found an error, pop-up the error buffer for the inspection, and returns nil.
If no error, returns t."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (sofa-save-buffer-modified 
        (widen)
        (save-buffer 0)

        (let ((errbuf (get-buffer sofa-json-error-buffer)))
          (when errbuf
            (with-current-buffer errbuf
              (erase-buffer))))

        (let ((json-buffer (current-buffer)))
          (with-temp-buffer
            (let ((outbuf (current-buffer)))
              (set-buffer json-buffer)
              (shell-command-on-region (point-min) (point-max)
                                       (concat sofa-json-prettifier
                                               " >/dev/null ")
                                       outbuf nil
                                       sofa-json-error-buffer nil))))

        (let ((file-name buffer-file-name)
              (errbuf (get-buffer sofa-json-error-buffer)))
          (if (or (null errbuf) (= (buffer-size errbuf) 0))
              t
            (with-current-buffer errbuf
              (goto-char (point-min))
              (while (re-search-forward sofa-json-error-regexp-for-compile
                                        nil t)
                (replace-match (format "%s:: \\1"
                                       (file-name-nondirectory file-name))))
              (goto-char (point-min))
              (compilation-minor-mode)
              (pop-to-buffer errbuf))
            nil))))))
  
(defun sofa-commit-design ()
  (let ((database sofa-database-name)
        (design sofa-design-name))
        
    (sofa-put-design database design (current-buffer))
    (set-buffer-modified-p nil)

    ;; reload the design doc.
    (sofa-reload-design)))
  

<<<<<<< HEAD:sofa.el
(defun sofa--remove-succeeded-docs (buffer response)
  "Remove the successfully processed documents in the current bulk operation.

BUFFER is `sofa-document-buffer', and RESPONSE is a string that contains
=======
(defun couchdb--remove-succeeded-docs (buffer response)
  "Remove the successfully processed documents in the current bulk operation.

BUFFER is `couchdb-document-buffer', and RESPONSE is a string that contains
>>>>>>> 42adb634f243f0504581ad2fee60ef1d8a4e9e62:couchdb.el
the returned HTTP resonse body.

The purpose of this function is to process remaining job after
commiting documents modification in bulk mode.

1. Remove the successfully committed document in the current buffer,
2. (TODO) display the error messages for each remaining document."
  (let ((json-input (json-read-from-string response))
        processed reasons)
<<<<<<< HEAD:sofa.el
    (sofa/doarray (elem json-input)
=======
    (couchdb/doarray (elem json-input)
>>>>>>> 42adb634f243f0504581ad2fee60ef1d8a4e9e62:couchdb.el
      (if (assoc-value 'error elem)
          (setq reasons (cons (list (assoc-value 'id elem)
                                    (assoc-value 'error elem)
                                    (assoc-value 'reason elem))
                              reasons))
        (setq processed (cons (assoc-value 'id elem) processed))))
    (setq reasons (nreverse reasons))

    ;; PROCESSED contains the list of ID that processed successfully.
    (with-current-buffer buffer
      (save-excursion
        (let* ((json-body (json-read-from-string
                           (buffer-substring-no-properties 
                            (point-min) (point-max))))
               (json-docs (assoc-value 'docs json-body []))
               errored-docs)
<<<<<<< HEAD:sofa.el
          (sofa/doarray (elem json-docs)
=======
          (couchdb/doarray (elem json-docs)
>>>>>>> 42adb634f243f0504581ad2fee60ef1d8a4e9e62:couchdb.el
            (unless (member (assoc-value '_id elem) processed)
              ;; TODO: how to let the user knows about the error message?
              (setq errored-docs (cons elem errored-docs))))
          (setq errored-docs (vconcat (nreverse errored-docs)))

          (if (eq (length errored-docs) 0)
              ;; all processed successfully!
              (progn (set-buffer-modified-p nil)
                     t)
            ;; Remove all successfully processed document in the current buffer.
            (setcdr (assoc 'docs json-body) errored-docs)
            (erase-buffer)
            (insert (json-encode-alist json-body))
<<<<<<< HEAD:sofa.el
            (sofa--prettify-buffer)
=======
            (couchdb--prettify-buffer)
>>>>>>> 42adb634f243f0504581ad2fee60ef1d8a4e9e62:couchdb.el
            (set-buffer-modified-p nil)

            (let ((msg ""))
              (mapc (lambda (err)
                      (setq msg (concat msg
                                        (format "%s: %s: %s\n"
                                                (nth 0 err) 
                                                (nth 1 err) 
                                                (nth 2 err)))))
                    reasons)
              (message (substring msg 0 -1))) ; remove trailing newline
            nil))))))

<<<<<<< HEAD:sofa.el
(defun sofa--commit-bulk-documents ()
=======
(defun couchdb--commit-bulk-documents ()
>>>>>>> 42adb634f243f0504581ad2fee60ef1d8a4e9e62:couchdb.el
  ;; TODO: validation of buffer?
  (if (not (buffer-modified-p (current-buffer)))
      (message "Nothing to commit")
    (let ((url (concat (sofa-endpoint sofa-database-name)
                       "/_bulk_docs"))
          (buffer (current-buffer))
          result)
      (curl/with-temp-buffer
        (insert-buffer buffer)
        (setq result (curl/http-send-buffer 'POST url (current-buffer)
                                            "application/json"))
        (let* ((headers (car result))
               (body (cdr result))
               (status (string-to-int (assoc-value "Status" headers "0"))))
<<<<<<< HEAD:sofa.el
          (setq sofa-body body
                sofa-status status)
=======
          (setq couchdb-body body
                couchdb-status status)
>>>>>>> 42adb634f243f0504581ad2fee60ef1d8a4e9e62:couchdb.el
          ;; if on error, show the error and exit.  if on success,
          ;; update the parent buffer, and exit if on partial success,
          ;; update hte parent buffer, and remove the succeeded
          ;; content from the buffer?
          (cond ((eq status 201)
                 ;; The doc is updated.
                 ;; TODO: STATUS will be still 201 if there's only conflict!!
<<<<<<< HEAD:sofa.el
                 (when (sofa--remove-succeeded-docs buffer body)
                   ;; everything is okay, so removing the edit buffer.
                   (sofa--hide-window-kill-buffer buffer)
=======
                 (when (couchdb--remove-succeeded-docs buffer body)
                   ;; everything is okay, so removing the edit buffer.
                   (couchdb--hide-window-kill-buffer buffer)
>>>>>>> 42adb634f243f0504581ad2fee60ef1d8a4e9e62:couchdb.el
                   ;; TODO: remove the marks!!
                   ))
                 
                (t
                 ;; If STATUS is 400, it means "bad request" (malformed JSON).

<<<<<<< HEAD:sofa.el
                 ;; something goes wrong; the BODY may contains
                 ;; additional information about the error.

                 ;; TODO: shorten 'reason or make it readable.
                 (let ((errinfo (json-read-from-string body)))
                   (message "sofa: %s: %s"
                            (assoc-value 'error errinfo "Unknown")
                            (assoc-value 'reason errinfo "Unknown")))
                 ))
=======
                 ;; something goes wrong
                 (message "CouchDB: %s: %s"
                          (assoc-value "error" headers "Unknown")
                          (assoc-value "reason" headers "Unknown")))
                 )
>>>>>>> 42adb634f243f0504581ad2fee60ef1d8a4e9e62:couchdb.el
        )))))

  
(defun sofa-commit-buffer ()
  "Commit the change into the remote"
  (interactive)
  (if (not (buffer-modified-p (current-buffer)))
      (message "Nothing to commit")
    (if sofa-commit-function
        (funcall sofa-commit-function)
      (message "Don't know how to commit"))))

(defun sofa--view-mode-map () 
  "create new sofa view mode map"
  (let ((map (make-sparse-keymap)))
    (define-key map [?n] #'sofa-view-forward-line)
    (define-key map [?p] #'sofa-view-previous-line)
    (define-key map [?m] #'sofa-view-mark)
    (define-key map [?u] #'sofa-view-unmark)
    (define-key map [?q] #'sofa-view-quit)
    (define-key map [?o] #'sofa-view-load-documents-other-window)
    (define-key map [?g] #'sofa-view-revert-buffer)
    (define-key map [(control ?x) ?\]] #'sofa-view-forward-page)
    (define-key map [(control ?x) ?\[] #'sofa-view-backward-page)

    map))

(defvar sofa-view-mode-map (sofa--view-mode-map)
  "Keymap for sofa view mode")

(defun sofa--edit-mode-map () 
  "create new sofa editing mode map"
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?x) ?k] #'sofa-edit-kill-buffer)
    (define-key map [(control ?c) (control ?k)] #'sofa-edit-kill-buffer)
    (define-key map [(control ?c) (control ?c)] #'sofa-edit-commit-buffer)
    (define-key map [(control ?c) ?c] #'sofa-validate-buffer)

    map))

(defvar sofa-edit-mode-map (sofa--edit-mode-map)
  "Keymap for sofa mode")


(defun sofa--json-mode-map () 
  "create new sofa mode map"
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?x) ?k] #'sofa-kill-buffer)
    (define-key map [(control ?c) (control ?k)] #'sofa-kill-buffer)
    (define-key map [(control ?c) (control ?c)] #'sofa-commit-buffer)
    (define-key map [(control ?c) (control ?e)] #'sofa-design-edit-value)
    (define-key map [(control ?c) ?c] #'sofa-validate-buffer)

    map))

(defvar sofa-json-mode-map (sofa--json-mode-map)
  "Keymap for sofa mode")

(defvar sofa-read-only-map (let ((map (make-keymap)))
                                (suppress-keymap map 'nodigits)
                                map))

(defun sofa--prepare-local-variables ()
  (make-variable-buffer-local 'sofa-database-name)
  (make-variable-buffer-local 'sofa-source-url)
  (make-variable-buffer-local 'sofa-design-name)
  (make-variable-buffer-local 'sofa-document-name)
  (make-variable-buffer-local 'sofa-commit-function)
  ;; if `sofa-edit-buffer' is non-nil, it should be the buffer which is
  ;; partially editing the current buffer.
  (make-variable-buffer-local 'sofa-edit-buffer)
  (make-variable-buffer-local 'sofa-parent-buffer)
  (make-variable-buffer-local 'sofa-parent-region)
  (make-variable-buffer-local 'sofa-skip)
  (make-variable-buffer-local 'sofa-limit)
  )

(define-derived-mode sofa-json-mode js-mode "sofa"
  "Major mode for sofa JSON editing

\\{sofa-json-mode-map}"
  (sofa--prepare-local-variables))

(define-derived-mode sofa-view-mode fundamental-mode "sofa"
  "Major mode for sofa view mode"
  (sofa--prepare-local-variables)
  (setq font-lock-defaults '(sofa--documents-keywords t nil nil nil))
  (font-lock-mode 1)
  (setq buffer-read-only t)
  (use-local-map sofa-view-mode-map))

(define-minor-mode sofa-edit-mode
  "Sofa JSON Edit mode"
  nil
  " sofa"
  sofa-edit-mode-map)

(defun sofa-reload-design ()
  (interactive)
  ;;(unless sofa-source-url (error "sofa-source-url missing"))
  (if (buffer-modified-p (current-buffer))
      ;; TODO: do we need to commit?
      nil
    (let ((oldpos (point)))
      (setq buffer-read-only nil)           ; is this needed?
      (erase-buffer)
      (sofa-get-design sofa-database-name sofa-design-name
                          (current-buffer))
      ;; TODO: I don't know how to restore the point after reloading
      ;;       the doc.  The better way will be, save the current key
      ;;       position, and call `goto-char' around to that key
      ;;       position.
      (goto-char oldpos))))

(defun sofa-load-design (&optional database design)
  (interactive)
  (unless database
    (setq database (sofa-read-database-name-from-minibuffer "Database:")))
  (unless design
    (setq design (sofa-read-design-name-from-minibuffer database
                                                           "Design Doc:")))
  (let ((bufname (concat "sofa:" database "/_design/" design))
        buffer)
    (if (setq buffer (get-buffer bufname))
        (progn (pop-to-buffer buffer)
               (message "You already loaded the design document."))
      (with-current-buffer (setq buffer (get-buffer-create bufname))
        (sofa-json-mode)
        ;;(message "buffer name: %S" (buffer-name))
        ;;(setq buffer-file-name-name (make-temp-file "sofa-"))
        (set-visited-file-name (make-temp-file "sofa-"))
        (rename-buffer bufname)
        (sofa-get-design database design buffer)
        ;;(message "buffer name: %S" (buffer-name))
        (save-buffer 0)
        ;;(message "buffer name: %S" (buffer-name))
        
        (setq sofa-database-name database
              sofa-design-name design
              sofa-commit-function 'sofa-commit-design))
      (switch-to-buffer buffer))))

(defsubst sofa-safe-document-buffer ()
  "Return a document buffer."
  (let ((buffer (get-buffer sofa-document-buffer)))
    (if (not buffer)
        (get-buffer-create sofa-document-buffer)
      buffer)))


(defun sofa--load-documents (database keys)
  "Load the documents with KEYS into the buffer, and return it."

  ;; TODO: if KEYS contains just one key, set-up for editing,
  ;; otherwise just read-only mode.

  ;; curl -d '{"keys":["bar","baz"]}' -X POST http://127.0.0.1:5984/foo/_all_docs?include_docs=true

  ;; TODO: the received document should be transformed to the form for
  ;;       modification.

  ;; TODO: need to load multiple docs (id, rev only)
  (let ((body (json-encode-alist (list (cons "keys" (vconcat keys)))))
        (url (sofa-view-endpoint database nil nil :include-docs t))
        (buffer (sofa-safe-document-buffer))
        status
        result)

    (when nil
      ;; huh?? even mark/unmark could change the buffer-modified-p.
      ;; TODO: what's the purpose here??
      (if (buffer-modified-p buffer)
          (error "not implemented yet.")))

    (curl/with-temp-buffer
      (insert body)
      (setq result (curl/http-send-buffer 'POST
                                          url
                                          (current-buffer)
                                          "application/json")))
    (setq status (assoc-value "Status" (car result) "404"))
    (if (not (string-equal status "200"))
        (progn (message "Remote returns HTTP %s code" status)
               nil)


      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;;(insert (cdr result))
          (let* ((json (json-read-from-string (cdr result)))
                 (rows (assoc-value 'rows json []))
                 (res  nil))
            ;; The purpose of this `let*' is to convert the JSON
            ;; output from CouchDB Bulk fetching API to the JSON data
            ;; for bulk doc modification.
            (sofa/doarray (elem rows)
              (let ((doc (assoc-value 'doc elem nil)))
                (setq res (cons doc res))))
            (insert (json-encode-alist (list (cons 'docs 
                                                   (vconcat (nreverse res)))))))
          
          (sofa--prettify-buffer buffer)

          (with-current-buffer buffer
            ;; I need to think carefully here.  Should I make some
            ;; JSON syntactic components read-only?  For example, text
            ;; outside of user's interest:
            ;;
            ;; {
            ;;   "docs": [
            ;;     ...
            ;;   ]
            ;; }

            (when nil
              ;; this causes `sofa--language' failed.
              (save-excursion
                (goto-char (point-min))
                (if (not (eq (car (json-forward-syntax)) 4))
                    ;; `json-forward-syntax' should return (4 . "{")
                    (error "something is wrong"))
                (if (not (eq (car (json-forward-syntax)) 15))
                    ;; `json-forward-syntax' should return (15 . "docs")
                    (error "something is wrong"))
                (if (not (eq (car (json-forward-syntax)) 1))
                    ;; `json-forward-syntax' should return (1 . ":")
                    (error "something is wrong"))
                (if (not (eq (car (json-forward-syntax)) 4))
                    ;; `json-forward-syntax' should return (4 . "[")
                    (error "something is wrong"))
                (put-text-property (point-min) (point)
                                   'keymap sofa-read-only-map)
                (put-text-property (point-min) (point)
                                   'intangible t)
                )))

          (set-buffer-modified-p nil)
          (set-visited-file-name (make-temp-file "sofa-"))
          ;; `set-visited-file-name' changes the buffer name so revert it.
          (rename-buffer sofa-document-buffer)
          (save-buffer 0)
          (sofa-json-mode)
          ;; TODO: set related local variables!!!

          ;; (setq sofa-database-name database ...)
          (setq sofa-database-name database
                sofa-source-url url
                sofa-commit-function #'sofa--commit-bulk-documents)))
      buffer)))


(defun sofa--load-documents/old (database keys)
  "Load the documents with KEYS into the buffer, and return it."

  ;; TODO: if KEYS contains just one key, set-up for editing,
  ;; otherwise just read-only mode.

  ;; curl -d '{"keys":["bar","baz"]}' -X POST http://127.0.0.1:5984/foo/_all_docs?include_docs=true

  ;; TODO: the received document should be transformed to the form for
  ;;       modification.
  (let ((body (json-encode-alist (list (cons "keys" (vconcat keys)))))
        (url (sofa-view-endpoint database nil nil :include-docs t))
        (buffer (sofa-safe-document-buffer))
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
        (progn (message "Remote returns HTTP %s code" status)
               nil)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (cdr result))
          (sofa--prettify-buffer buffer)
          (set-buffer-modified-p nil)
          (set-visited-file-name (make-temp-file "sofa-"))
          ;; `set-visited-file-name' changes the buffer name so revert it.
          (rename-buffer sofa-document-buffer)
          (save-buffer 0)
          (sofa-json-mode)
          ;; TODO: set related local variables!!!

          ;; (setq sofa-database-name database ...)
                
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
                   

(defun sofa-current-value-string-p ()
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

(defun sofa-string-value-region ()
  (let ((reg (sofa-current-value-string-p))
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

(defun sofa-edit-commit-buffer ()
  (interactive)

  ;; TODO: make sure that the parent buffer is not buffer-modified-p if the
  ;;       commit did not change anything.
  (let ((parent (buffer-local-value 'sofa-parent-buffer (current-buffer)))
        (buffer (current-buffer)))
    (unless (and parent (buffer-live-p parent))
      (error "No live parent buffer existed."))

    (if (not (buffer-modified-p (current-buffer)))
        (progn
          (message "nothing changed.")
          ;; TODO: do we need to kill the editing buffer?
          (sofa-edit-kill-buffer)
          )
      (widen)
      ;;(while (search-forward "\n" nil t)
      ;; (replace-match "\\n" nil t))
      
      (let ;; ((body (buffer-string)))
          ((body (json-read-string-from-buffer))
           (begin (car sofa-parent-region))
           (end (cdr sofa-parent-region)))

        (with-current-buffer parent
          (setq buffer-read-only nil)
          (remove-overlays begin end
                           'type 'sofa)
          (delete-region begin end)
          (insert body)
          (setq sofa-edit-buffer nil))

        (sofa--hide-window-kill-buffer buffer)))))


(defun sofa--language (doc)
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
      
(defun sofa--set-major-mode (keys src-buffer dst-buffer)
  "Set the major mode of DST-BUFFER according to KEYS from SRC-BUFFER"
  ;; TODO: it is not a good idea to parse the whole buffer in this time.
  ;;       it's better to parse it after loading the design doc.

  (if (buffer-local-value 'sofa-design-name src-buffer)
      ;; If SRC-BUFFER reflects a design document, use "language" property
      (let ((lang (sofa--language src-buffer)))
        (cond ((or (and (eq (length keys) 3)
                        (string-equal (nth 0 keys) "views"))
                   (and (eq (length keys) 2)
                        (string-equal (nth 0 keys) "shows")))
               (with-current-buffer dst-buffer
                 (javascript-mode)))
              (t 
               (with-current-buffer dst-buffer
                 (text-mode)))))
    ;; TODO: what major mode?
    (with-current-buffer dst-buffer
      (text-mode))))

(defun sofa--clear-editing-status (&optional force)
  "Clear the editing status on the current buffer."
  (if (and sofa-edit-buffer 
           (buffer-live-p sofa-edit-buffer)
           force)
      (kill-buffer sofa-edit-buffer))
  (setq sofa-edit-buffer nil)
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) 'type 'sofa))
  (setq buffer-read-only nil))

      
(defun sofa-design-edit-value ()
  "Edit current JSON value in other buffer."
  (interactive)

  (if (and sofa-edit-buffer
           (buffer-live-p sofa-edit-buffer)
           (buffer-modified-p sofa-edit-buffer))
      (progn (pop-to-buffer sofa-edit-buffer)
             (message "You need to commit or to cancel the on-going editing"))
    (sofa--clear-editing-status)
    (sofa--prepare-editing-value)))
    

(defun sofa--prepare-editing-value ()
  (let ((reg (sofa-string-value-region))
        (keys (json-key-list))
        overlay)
    (when reg
      ;; (remove-overlays (point-min) (point-max) 'type 'sofa)
      (message "set invert face around (%d %d)" 
               (marker-position (car reg))
               (marker-position (cdr reg)))
      (setq overlay (make-overlay (car reg) (cdr reg)))
      (overlay-put overlay 'face 'sofa-value-edited-face)
      (overlay-put overlay 'type 'sofa)

      (setq buffer-read-only t)

      
      (let ((buffer (get-buffer-create sofa-json-edit-buffer))
            (oldbuf (current-buffer))
            (body (concat "\""
                          (buffer-substring-no-properties (car reg) (cdr reg))
                          "\"")))
        (setq sofa-edit-buffer buffer)
        (with-current-buffer buffer
          (sofa--set-major-mode keys oldbuf buffer)

          (setq sofa-parent-buffer oldbuf)
          (setq sofa-parent-region reg)
          (widen)
          (erase-buffer)
          (sofa-edit-mode)
          (insert (read body))
          (goto-char (point-min))
          (set-buffer-modified-p nil)
          ;; TODO: set the keymap for editing in the current buffer
          )
        (pop-to-buffer buffer)))))

(defun sofa-js-parse (buffer)
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

(defun sofa-js-function (buffer function-name)
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



(defvar sofa-history-database-name nil
  "History list of read sofa database name")

(defvar sofa-history-design-name nil
  "History list of read sofa design document name")

(defun sofa-read-database-name-from-minibuffer (prompt)
  "Completing read the database name from the minibuffer."
  (let* ((name-list (sofa-databases))
         (default (car (member-if (lambda (name) (member name name-list))
                                  sofa-history-database-name)))
         (choice ""))
    (setq prompt (if default
                     (format "%s [%s] " prompt default)
                   prompt))
    (while (string-equal choice "")
      (setq choice (completing-read prompt name-list 
                                    (lambda (name)
                                      (member name name-list))
                                    'valid-only nil
                                    'sofa-history-database-name
                                    default)))
    choice))

(defun sofa-read-design-name-from-minibuffer (database prompt)
  "Completing read the design document name from the minibuffer."
  (let* ((name-list (sofa-get-all-designs database))
         (default (car (member-if (lambda (name) (member name name-list))
                                  sofa-history-design-name)))
         (choice ""))
    (setq prompt (if default
                     (format "%s [%s] " prompt default)
                   prompt))
    (while (string-equal choice "")
      (setq choice (completing-read prompt name-list 
                                    (lambda (name)
                                      (member name name-list))
                                    'valid-only nil
                                    'sofa-history-design-name
                                    default)))
    choice))


  
(defun sofa-view-delete-documents ()
  (interactive)
  (let ((docs (sofa--map-over-marks 
               (lambda () 
                 (let ((pos (+ 3 (point))))
                   (cons (get-text-property pos 'sofa-key)
                         (get-text-property pos 'sofa-rev))))))
        (url (concat (sofa-endpoint sofa-database-name)
                     "/_bulk_docs"))
        result)
    (when (> (length docs) 0)
      (curl/with-temp-buffer
        (insert "{ \"docs\": [\n")
        (dolist (d docs)
          (insert (format
                   "{\"_id\":\"%s\",\"_rev\":\"%s\",\"_deleted\":true},\n"
                   (car d) (cdr d))))
        (delete-char -2)
        (insert "\n]}")

        ;; Now, current buffer is filled with JSON data for bulk deletion.
        (setq result (curl/http-send-buffer 'POST url (current-buffer)
                                            "application/json"))

        ;; TODO: DEAL RESULT HERE!!!!!!!!!!!!!!!
        ;; 
        ;; If successful, "201" returned with the body, "[ { id: rev }, ... ]"

        ;; If partially successful (even all conflicted), "201" with the body,
        ;;
        ;; [ { "id" : "xxx", "rev" : "yyy" },
        ;;   { "id" : "www", "error" : "conflict", "reason" : "..." } ]
        ;;

        ;; TODO: remove the successfully deleted documents from the view,
        ;;       For the failed document, refresh the revision, and
        ;;       show message, "updated revisions, try again", and quit.
        (error "not fully implemented yet.")
        ))))


            
(defun sofa-view-refresh-marked-revision ()
  "Refresh the marked entries for the updated status.

If the marked document is deleted, delete the entry from the buffer,
If the marked document has different revision, refresh the buffer and property.

Except updating for deletion, mark should stay the same."
  (let ((in (json-encode-alist (list (cons 'keys 
                                           (vconcat (sofa--marked-keys))))))
        (url (sofa-view-endpoint sofa-database-name)))

    
)


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

(defun sofa-import-directory (directory &optional recursive)
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
                             
                     

(provide 'sofa)
;;; sofa.el ends here
