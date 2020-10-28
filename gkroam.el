;;; gkroam.el --- A lightweight org-mode roam replica  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 2.3.7
;; Keywords: org, convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/gkroam.el
;; Package-Requires: ((emacs "26.3") (db "0.0.6") (company "0.9.10"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Gkroam is a lightweight roam repica, built on top of Org-mode.

;;; ChangeLog:

;; v1.0 - Auto update link references at the buttom of page buffer.

;; v2.0 - Use overlay to hide and show gkroam brackets accordingly
;; and fix some bugs.

;; v2.0.1 - Fix 'hide and show brackets' problems in some main occasion.
;; Such as newline, etc.

;; v2.0.2 - Fix 'gkroam-publish-current-file' and 'gkroam-preview-current',
;; automatically convert gkroam link to org link and convert it back after
;; published (use 'undo', not reliable). But it has problem with publishing
;; the whole project.

;; v2.0.3 - Fix 'gkroam-publish-site' and 'gkroam-preview'.
;; Now you can publish and preview the whole roam site.

;; v2.0.4 - Many bugs fixed and code improvement.

;; v2.1.0 - A more powerful linked references system.

;; v2.1.1 - Change package name to 'gkroam' from 'gk-roam'.

;; v2.2.0 - Edit many pages in one side window and save changes separately.

;; v2.2.1 - Many bug fixed and misc code optimization.

;; v2.3.0 - Implement headline references, add a new minor mode
;; 'gkroam-dynamic-brackets-mode' and rename 'gkroam-edit' to `gkroam-capture'.

;; v2.3.1 - A more resonable way to insert link.
;; Press "C-p RET" or "C-M-j" to skip headline completion for ivy user
;; or just press "RET" for vanilla user.

;; v2.3.2 - Beautify page: unify org list bullet and beautify org checkbox.
;; Better to turn it off when editing the page.

;; v2.3.3 - Make page filename customizable, delete index file
;; and show index in buffer.

;; v2.3.4 - Delete 'gkroam-dynamic-brackets-mode'
;; and add `gkroam-toggle-dynamic' function.

;; v2.3.5 - Optimize gkroam page prettification, change 'gkroam-toggle-beautify'
;; to `gkroam-toggle-prettify' for precise semanteme.

;; v2.3.6 - Implement a perfect linked references workflow.
;; When a link is the item of org plain list, the whole list structure will be shown.

;; v2.3.7 - Add headline id only when you insert a gkroam link.
;; Use `gkroam-rebuild-caches' command to rebuild headline and id caches.

;;; Code:

(require 'cl-lib)
(require 'org-id)
(require 'db)
(require 'company)

;;;; Declarations
(defvar org-link-frame-setup)

;;;; Variables
(defgroup gkroam nil
  "A roam replica on top of emacs org-mode."
  :tag "gkroam"
  :group 'org)

(defcustom gkroam-root-dir "~/gkroam/org/"
  "Gkroam's root directory, with org files in it."
  :type '(choice directory sexp)
  :group 'gkroam)

(defcustom gkroam-cache-dir (concat user-emacs-directory "gkroam/")
  "Gkroam's cache directory."
  :type 'directory
  :group 'gkroam)

(defcustom gkroam-index-title "GKROAM"
  "Title of index page."
  :type 'string
  :group 'gkroam)

(defcustom gkroam-window-margin 2
  "Gkroam window's left and right margin."
  :type 'integer
  :group 'gkroam)

(defcustom gkroam-use-default-filename nil
  "Non-nil means use default filename for gkroam page.
The default format is '%Y%m%d%H%M%S' time string."
  :type 'boolean
  :group 'gkroam)

(defcustom gkroam-show-brackets-p nil
  "Non-nil means to show brackets in page link."
  :type 'boolean
  :group 'gkroam)

(defcustom gkroam-title-height 300
  "Height of gkroam page title when prettifying."
  :type 'integer
  :group 'gkroam)

(defvar gkroam-headline-db
  (db-make
   `(db-hash
     :filename ,(concat gkroam-cache-dir "gkroam-headline-db")))
  "Database fot caching gkroam's headline and headline id.")

(defvar gkroam-page-db
  (db-make
   `(db-hash
     :filename ,(concat gkroam-cache-dir "gkroam-page-db")))
  "Database for caching gkroam page's filename.")

(defvar gkroam-reference-db
  (db-make
   `(db-hash
     :filename ,(concat gkroam-cache-dir "gkroam-reference-db")))
  "Database for caching gkroam page's references.")

(defvar gkroam-org-list-re
  "^ *\\([0-9]+[).]\\|[*+-]\\) \\(\\[[ X-]\\] \\)?"
  "Org list bullet and checkbox regexp.")

(defvar gkroam-pages nil
  "Page candidates for completion.")

(defvar gkroam-has-link-p nil
  "Judge if has link or hashtag in gkroam buffer.")

(defvar gkroam-link-regexp
  (rx (seq (group "{[")
           (group (+? not-newline))
           (group (?? (seq (group " » ")
                           (group (+? not-newline)))))
           (group "]")
           (group (?? (seq (group "[")
                           (group (+? not-newline))
                           (group "]"))))
           (group "}")))
  "Regular expression that matches a gkroam link.")

(defvar gkroam-hashtag-regexp
  (rx (seq (group "#")
           (group "{[")
           (group (+? not-newline))
           (group "]}")))
  "Regular expression that matches a gkroam hashtag.")

(defvar gkroam-link-with-headline-re "{\\[\\(.+?\\) » \\(.+?\\)\\].*}"
  "Gkroam headline link regexp.")

(defvar gkroam-reference-delimiter-re
  "^* [0-9]+ Linked References.*"
  "Delimiter string regexp to separate page contents from references region.")

(defvar gkroam-prettify-page-p nil
  "Non-nil means to prettify gkroam page.")

(defvar gkroam-return-wconf nil
  "Saved window configuration before goto gkroam capture.")

(defvar gkroam-capture-flag nil
  "Non-nil means it's in process of gkroam capture.")

(defvar gkroam-index-buf "*Gkroam Index*"
  "Gkroam index buffer name.")

(defvar gkroam-capture-buf "*Gkroam Capture*"
  "Gkroam capture buffer name.")

(defvar gkroam-capture-pages nil
  "Pages that have been capturing in gkroam capture buffer.
The value is a list of page's title.")

;;;; Functions
(defun gkroam-link-frame-setup (func)
  "Alter `org-link-frame-setup' for gkroam.
Use FUNC function to open file link."
  (setq org-link-frame-setup
        `((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . ,func)
          (wl . wl-other-frame))))

(defun gkroam-at-root-p ()
  "Check if current file exists in `gkroam-root-dir'.
If BUFFER is non-nil, check the buffer visited file."
  (when (buffer-file-name)
    (file-equal-p (file-name-directory (buffer-file-name))
                  (expand-file-name gkroam-root-dir))))

(defun gkroam-at-index-buf ()
  "Check if current buffer is `gkroam-index-buf'."
  (string= (buffer-name) gkroam-index-buf))

(defun gkroam-at-capture-buf ()
  "Check if current buffer is `gkroam-capture-buf'."
  (string= (buffer-name) gkroam-capture-buf))

(defun gkroam-work-p ()
  "Check if current file or buffer is where gkroam has to be in work."
  (or (gkroam-at-root-p)
      (gkroam-at-index-buf)
      (gkroam-at-capture-buf)))

(defun gkroam-retrive-page (title)
  "Retrive page's filename from database, 
the page has a title named TITLE"
  (cdar (db-get title gkroam-page-db)))

(defun gkroam-retrive-title (page)
  "Retrive page's title from database, 
the page has a filename named PAGE"
  (caar (db-query gkroam-page-db `(= "page" ,page))))

(defun gkroam-retrive-all-titles ()
  "Retrive all gkroam pages' titles from database."
  (mapcar #'car (db-query gkroam-page-db nil)))

(defun gkroam--get-file (page)
  "Get gkroam file accroding to PAGE."
  (expand-file-name page gkroam-root-dir))

(defun gkroam--gen-file ()
  "Generate new gkroam file path."
  (expand-file-name (gkroam--gen-page) gkroam-root-dir))

(defun gkroam--gen-page ()
  "Generate new gkroam page filename, without directory prefix."
  (let* (slug slug-format)
    (if gkroam-use-default-filename
        (setq slug-format (format-time-string "%Y%m%d%H%M%S"))
      (setq slug (completing-read "Input filename or press \"RET\" to use the default: "
                                  nil nil nil nil nil (format-time-string "%Y%m%d%H%M%S")))
      (setq slug-format (string-join (split-string slug) "-")))
    (format "%s.org" slug-format)))

(defun gkroam--narrow-to-content ()
  "Narrow region to gkroam page contents if there is a reference region."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward gkroam-reference-delimiter-re nil t)
      (narrow-to-region (point-min) (1- (line-beginning-position))))))

(defun gkroam--narrow-to-reference ()
  "Narrow region to page references if there is a reference region."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward gkroam-reference-delimiter-re nil t)
      (narrow-to-region (line-beginning-position) (point-max)))))

(defun gkroam-new (title)
  "Just create a new gkroam page titled with TITLE."
  (let ((file (gkroam--gen-file)))
    (with-current-buffer (find-file-noselect file t)
      (insert (format "#+TITLE: %s\n\n" title))
      (save-buffer))
    (push title gkroam-pages)
    (gkroam-cache-curr-page title)
    file))

;;; ----------------------------------------
;; linked references

(defvar gkroam-link-re-format "{\\[%s.*?\\]}"
  "Gkroam link regexp format used for searching link context.")

(defun gkroam-start-process (buf-name args)
  "Start a rg process with output buffer named BUF-NAME.
ARGS are the arguments of rg process."
  (let ((name (generate-new-buffer-name buf-name)))
    (eval `(start-process ,name ,name "rg" ,@args
                          ,(expand-file-name gkroam-root-dir)))))

(defun gkroam-search-process (process callback)
  "Call CALLBACK After the PROCESS finished."
  (unless (executable-find "rg")
    (user-error "Cannot find program rg"))
  (let (sentinel)
    (setq sentinel
          (lambda (process event)
            (if (string-match-p (rx (or "finished" "exited"))
                                event)
                (if-let ((buf (process-buffer process)))
                    (with-current-buffer buf
                      (save-excursion
                        (unless (string-empty-p (buffer-string))
                          (funcall callback (buffer-string)))))
                  (error "Gkroam’s rg process’ buffer is killed"))
              (error "Gkroam’s rg process failed with signal: %s"
                     event))))
    (set-process-sentinel process sentinel)))

(defun gkroam--process-backlink (string page line-number)
  "Convert gkroam link to backlink in STRING, 
the backlink refers to a link in Line-NUMBER line of PAGE."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward gkroam-link-regexp nil t)
      (let* ((title (match-string-no-properties 2))
             (alias (match-string-no-properties 9))
             (headline (match-string-no-properties 5)))
        (if alias
            (replace-match (gkroam--format-backlink page line-number alias))
          (if headline
              (replace-match (gkroam--format-backlink page line-number headline))
            (replace-match (gkroam--format-backlink page line-number title))))))
    (goto-char (point-min))
    (while (re-search-forward gkroam-hashtag-regexp nil t)
      (replace-match (gkroam--format-backlink
                      page line-number
                      (match-string-no-properties 2))))
    (buffer-string)))

(defun gkroam--format-reference-content ()
  "Format the content of linked reference."
  (save-excursion
    (goto-char (line-beginning-position))
    (let* ((blank-num (skip-chars-forward "[ ]"))
           (elem (org-element-at-point))
           (level-1-blank-num (cadr (car (org-element-property :structure elem))))
           (elem-type (org-element-type elem))
           (elem-start (org-element-property :begin elem))
           (elem-end (org-element-property :end elem))
           (elem-str (buffer-substring-no-properties elem-start elem-end))
           reference-str)
      (pcase elem-type
        ('item
         (let* ((relative-levels (/ (- blank-num level-1-blank-num) 2))
                parent parent-beg (parent-strs "") parent-str (relative-level 0))
           (catch 'break
             (while (point)
               (if (= blank-num level-1-blank-num)
                   (throw 'break nil)
                 (setq parent (org-element-property :parent (org-element-at-point)))
                 (setq parent-beg (org-element-property :begin parent))
                 (setq blank-num
                       (progn
                         (goto-char parent-beg)
                         (forward-line -1)
                         (skip-chars-forward "[ ]")))
                 (save-excursion
                   (goto-char (line-beginning-position))
                   (re-search-forward gkroam-org-list-re (line-end-position) t)
                   (incf relative-level)
                   (setq parent-str
                         (buffer-substring-no-properties (point)
                                                         (line-end-position))))
                 (when (= relative-level 1)
                   (setq parent-strs (concat parent-str "\n" parent-strs)))
                 (when (= relative-level relative-levels)
                   (setq parent-strs (concat parent-str " > " parent-strs))))))
           (setq reference-str (concat parent-strs elem-str))))
        ('plain-list
         (let* ((plain-lst (org-element-property :structure (org-element-at-point)))
                (item (car plain-lst)))
           (setq elem-start (car item))
           (setq elem-end (car (last item)))
           (setq reference-str (buffer-substring-no-properties elem-start elem-end))))
        (_ (setq reference-str elem-str)))
      reference-str)))

(defun gkroam--process-searched-string (string title)
  "Process searched STRING by 'rg', get the whole contents of TITLE page."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((gkroam-file-re (expand-file-name ".+\\.org" gkroam-root-dir))
          (beg (point-min)) (end (point)) (num 0) references)
      (while (not (= end (point-max)))
        (save-excursion
          (goto-char beg)
          (if (re-search-forward gkroam-file-re nil t 2)
              (setq end (line-beginning-position))
            (setq end (point-max))))
        (save-restriction
          (narrow-to-region beg end)
          (goto-char beg)
          (re-search-forward gkroam-file-re nil t)
          (let* ((path (match-string-no-properties 0))
                 (page (file-name-nondirectory path))
                 context (last-headline ""))
            (while (re-search-forward
                    (replace-regexp-in-string "%s" title gkroam-link-re-format)
                    nil t)
              (let* ((headline "")
                     (line-number (current-line))
                     (raw-content
                      (string-trim
                       (gkroam--format-reference-content) nil "[ \t\n\r]+"))
                     (content (gkroam--process-backlink raw-content page line-number)))
                (setq num (1+ num))
                (save-excursion
                  (when (re-search-backward "^*+ .+\n" nil t)
                    (setq headline
                          (string-trim (match-string-no-properties 0) "*+ +" nil))
                    (setq headline (concat "*** " headline))))
                (if (string= headline last-headline)
                    (setq context (concat context content "\n\n"))
                  (setq context (concat context headline "\n" content "\n\n")))
                (setq last-headline headline)))
            (setq references
                  (concat references
                          (format "** %s\n%s"
                                  (gkroam--format-backlink
                                   page nil (gkroam-retrive-title page))
                                  context)))
            (setq beg end))))
      (cons num (string-trim references)))))

(defun gkroam-search-page-link (page)
  "Return a rg process to search a specific PAGE's link.
Output matched files' path and context."
  (let ((title (gkroam-retrive-title page)))
    (gkroam-start-process " *gkroam-rg*"
                          `(,(format "\\{\\[%s.*?\\](\\[.+?\\])?\\}" title)
                            "--ignore-case" "--sortr" "path"
                            "-C" ,(number-to-string 9999)
                            "-N" "--heading"
                            "-g" "!index.org*"))))

(defun gkroam-update-reference (page)
  "Update gkroam PAGE's reference."
  (gkroam-search-process
   (gkroam-search-page-link page)
   (lambda (string)
     (let* ((title (gkroam-retrive-title page))
            (file (gkroam--get-file page))
            (file-buf (find-file-noselect file t))
            (processed-str (gkroam--process-searched-string string title))
            (num (car processed-str))
            (references (cdr processed-str))
            (cached-references (cdar (db-get title gkroam-reference-db)))
            reference-start)
       (with-current-buffer file-buf
         (save-excursion
           (goto-char (point-max))
           (re-search-backward gkroam-reference-delimiter-re nil t)
           (setq reference-start (point))
           (unless (string= references cached-references)
             (let ((inhibit-read-only t))
               (remove-text-properties reference-start (point-max) '(read-only nil)))
             (delete-region (point) (point-max))
             (insert (format "* %d Linked References to \"%s\"\n\n" num title))
             (insert references)
             ;; use overlay to hide part of reference. (filter)
             ;; (gkroam-overlay-region beg (point-max) 'invisible t)
             (indent-region reference-start (point-max))
             (save-buffer)
             (db-put title `(("reference" . ,references)) gkroam-reference-db)
             (message "%s reference updated" page))
           (gkroam-list-parent-item-overlay reference-start)
           (gkroam-reference-region-overlay reference-start)
           (when gkroam-prettify-page-p
             (gkroam-org-list-fontify reference-start (point-max)))
           (gkroam-backlink-fontify reference-start (point-max))
           (unless (get-text-property reference-start 'read-only)
             (put-text-property reference-start (point-max)
                                'read-only "Linked references region is uneditable."))))))))

;; ----------------------------------------
;; headline linked references

(defun gkroam--get-headlines (title)
  "Get page's headline list, the page is titled with TITLE."
  (with-temp-buffer
    (insert-file-contents
     ;; do not use `insert-file-contents-literally', it cannot show chinese normally.
     (gkroam--get-file (gkroam-retrive-page title)))
    (save-restriction
      (gkroam--narrow-to-content)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (headline)
          (org-element-property :raw-value headline))))))

(defun gkroam-goto-headline (id)
  "Goto headline with id ID."
  (org-id-goto id)
  (gkroam-update)
  (gkroam-fontify-link)
  (gkroam-prettify-page))

(defun gkroam-set-headline-id (title headline)
  "Cache the HEADLINE's id of page titled with TITLE in db."
  (let* ((file (gkroam--get-file (gkroam-retrive-page title)))
         (page-buf (find-file-noselect file t))
         headline-id)
    (with-current-buffer page-buf
      (save-excursion
        (goto-char (point-min))
        (re-search-forward (concat "^*+ " headline " *$") nil t)
        (let ((alist (db-get title gkroam-headline-db)))
          (if alist
              (let ((kv (assoc headline alist)))
                (setq headline-id (org-id-get-create))
                (if kv
                    (unless (string= headline-id (cdr kv))
                      (setq alist (assoc-delete-all headline alist))
                      (push (cons headline headline-id) alist))
                  (push (cons headline (org-id-get-create)) alist))
                (db-put title alist gkroam-headline-db))
            (db-put title `(,(cons headline (org-id-get-create))) gkroam-headline-db)))
        (save-buffer)))
    headline-id))

;; cache
(defun gkroam--get-page (title)
  "Get gkroam page from TITLE."
  (let ((pages (gkroam--all-pages))
        file)
    (catch 'break
      (dolist (page pages)
        (setq file (gkroam--get-file page))
        (with-temp-buffer
          (insert-file-contents file nil 0 2000 t)
          (goto-char (point-min))
          (when (re-search-forward (format "^ *#\\+TITLE: *%s *$" title) nil t)
            (throw 'break page)))))))

(defun gkroam--all-pages ()
  "Get all gkroam pages."
  (directory-files gkroam-root-dir nil "^[^.#].+\\.org$"))

(defun gkroam--get-title (page)
  "Get PAGE's title."
  (with-temp-buffer
    (insert-file-contents (gkroam--get-file page) nil 0 2000 t)
    (goto-char (point-min))
    (re-search-forward "^ *#\\+TITLE:" nil t)
    (string-trim (buffer-substring (match-end 0) (line-end-position)))))

(defun gkroam--all-titles ()
  "Get all gkroam titles."
  (let* ((pages (gkroam--all-pages)))
    (mapcar (lambda (page) (gkroam--get-title page)) pages)))

(defun gkroam-cache-curr-page (title)
  "Cache gkroam page's filename, which titled with TITLE."
  (let* ((page (db-get title gkroam-page-db))
         (filename (gkroam--get-page title)))
    (unless (equal page filename)
      (db-put title `(("page" . ,filename)) gkroam-page-db))))

;;;###autoload
(defun gkroam-cache-all-pages ()
  "Cache all gkroam pages' title and filename."
  (let* ((titles (gkroam--all-titles))
         (num (length titles)))
    (dolist (title titles)
      (gkroam-cache-curr-page title))
    num))

(defun gkroam-cache-curr-headline-links ()
  "Cache current page's gkroam headline links."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward gkroam-link-with-headline-re nil t)
      (gkroam-set-headline-id
       (match-string-no-properties 1)
       (match-string-no-properties 2)))))

(defun gkroam-search-all-headline-links ()
  "Return a rg process to search all gkroam headline links.
Output matched files' path."
  (gkroam-start-process " *gkroam-rg-headlines*"
                        '("\\{\\[.+? » .+?\\].*\\}" "-l")))

;;;###autoload
(defun gkroam-cache-all-headline-links ()
  "Cache all pages's gkroam headline links."
  (gkroam-search-process
   (gkroam-search-all-headline-links)
   (lambda (string)
     (dolist (file (split-string string))
       (with-current-buffer (find-file-noselect file t)
         (gkroam-cache-curr-headline-links))))))

(defun gkroam-cache-page-references (title)
  "Cache gkroam page's references, which titled with TITLE."
  (let ((page (gkroam-retrive-page title)))
    (gkroam-search-process
     (gkroam-search-page-link page)
     (lambda (string)
       (let ((references (cdr (gkroam--process-searched-string string title))))
         (db-put title `(("reference" . ,references)) gkroam-reference-db))))))

;;;###autoload
(defun gkroam-cache-all-page-references ()
  "Cache all gkroam page's references."
  (let ((titles (gkroam--all-titles)))
    (dolist (title titles)
      (gkroam-cache-page-references title))))

;;;###autoload
(defun gkroam-rebuild-caches ()
  "Clear gkroam headline-id cache."
  (interactive)
  (db-hash-clear gkroam-page-db)
  (db-hash-clear gkroam-headline-db)
  (db-hash-clear gkroam-reference-db)
  (gkroam-cache-all-pages)
  (gkroam-cache-all-headline-links)
  (gkroam-cache-all-page-references)
  (message "All caches have been built."))

;; ----------------------------------------

;;;; Commands
;;;###autoload
(defun gkroam-find (&optional title)
  "Create a new gkroam page or open an exist one in current window, titled with TITLE."
  (interactive)
  (let* ((title (or title (completing-read "New title or open an exist one: "
                                           (gkroam-retrive-all-titles) nil nil)))
         (page (gkroam-retrive-page title)))
    (if page
        (find-file (gkroam--get-file page))
      (find-file (gkroam-new title)))
    (gkroam-update)
    (gkroam-prettify-page)
    (gkroam-fontify-link)))

;;;###autoload
(defun gkroam-daily ()
  "Create or open gkroam daily notes."
  (interactive)
  (let* ((title (format-time-string "%b %d, %Y")))
    (gkroam-find title)))

;;;###autoload
(defun gkroam-insert (&optional title alias)
  "Insert a gkroam page link at point.
With optional arguments, use TITLE or HEADLINE or ALIAS to format link."
  (interactive)
  (if (gkroam-work-p)
      (let* ((title (or title
                        (completing-read
                         "Choose a page or create a new: "
                         (gkroam-retrive-all-titles) nil nil)))
             (title-exist-p (gkroam-retrive-page title))
             (headlines-exist-p (when title-exist-p
                                  (gkroam--get-headlines title)))
             (headline (when headlines-exist-p
                         (completing-read
                          "Choose a headline or press \"C-p RET\" (\"RET\") to skip: "
                          headlines-exist-p nil nil)))
             (alias (or alias
                        (completing-read
                         "Give an alias or press \"RET\" to skip: "
                         nil nil nil))))
        (when (string= headline "") (setq headline nil))
        (when (string= alias "") (setq alias nil))
        (when headline
          (gkroam-set-headline-id title headline))
        (insert (gkroam--format-link title headline alias))
        (unless (gkroam-at-capture-buf)
          (save-buffer)
          (when title-exist-p
            (gkroam-update-reference title-exist-p))))
    (message "Not in the gkroam directory!")))

;;;###autoload
(defun gkroam-new-at-point ()
  "Insert a file link and create a new file according to text at point."
  (interactive)
  (let* ((title (thing-at-point 'word t))
         (bounds (bounds-of-thing-at-point 'word))
         (beg (car bounds))
         (end (cdr bounds))
         (page-exist-p (gkroam-retrive-page title)))
    (if (gkroam-work-p)
        (if page-exist-p
            (progn
              (delete-region beg end)
              (gkroam-insert title ""))
          (delete-region beg end)
          (gkroam-insert title "")
          (unless (gkroam-at-capture-buf)
            (gkroam-find title)))
      (gkroam-find title))))

;;;###autoload
(defun gkroam-new-from-region ()
  "Insert a file link and create a new file according to a selected region."
  (interactive)
  (when (region-active-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (title (buffer-substring-no-properties beg end))
           (page-exist-p (gkroam-retrive-page title)))
      (if (gkroam-work-p)
          (if page-exist-p
              (progn
                (delete-region beg end)
                (gkroam-insert title ""))
            (delete-region beg end)
            (gkroam-insert title "")
            (unless (gkroam-at-capture-buf)
              (gkroam-find title)))
        (gkroam-find title)))))

;;;###autoload
(defun gkroam-smart-new ()
  "Smartly create a new file according to point or region."
  (interactive)
  (cond
   ((region-active-p) (gkroam-new-from-region))
   ((thing-at-point 'word) (gkroam-new-at-point))
   (t (call-interactively #'gkroam-find))))

;;;###autoload
(defun gkroam-index ()
  "Show gkroam index buffer."
  (interactive)
  (with-current-buffer (get-buffer-create gkroam-index-buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "#+TITLE: %s\n\n" gkroam-index-title))
      (dolist (page (gkroam--all-pages))
        (insert (format "+ [[file:%s][%s]]\n"
                        (gkroam--get-file page) (gkroam-retrive-title page)))))
    (switch-to-buffer gkroam-index-buf)
    (org-mode)
    (read-only-mode 1)))

;;;###autoload
(defun gkroam-update ()
  "Update current gkroam buffer's reference."
  (interactive)
  (if (gkroam-at-root-p)
      (gkroam-update-reference (file-name-nondirectory (buffer-file-name)))
    (message "Not in the gkroam directory!")))

(defun gkroam-refresh-page ()
  "Refresh current gkroam page, update reference,
 fontify link and prettify page"
  (when (gkroam-work-p)
    (gkroam-prettify-page)
    (gkroam-fontify-link)
    (gkroam-update)))

;;; ----------------------------------------
;; minor mode: gkroam-link-mode

(define-button-type 'gkroam-link
  'action #'gkroam-follow-link
  'face '(:underline nil)
  'title nil
  'headline nil
  'follow-link t
  'help-echo "Jump to page")

(defun gkroam--format-link (title &optional headline alias)
  "Format TITLE into a gkroam page link.
With optional argument HEADLINE, format also with headline.
With optional argument ALIAS, format also with alias."
  (if headline
      (if alias
          (format "{[%s » %s][%s]}" title headline alias)
        (format "{[%s » %s]}" title headline))
    (if alias
        (format "{[%s][%s]}" title alias)
      (format "{[%s]}" title))))

(defun gkroam-follow-link (button)
  "Jump to the page that BUTTON represents."
  (with-demoted-errors "Error when following the link: %s"
    (let* ((title (button-get button 'title))
           (headline (button-get button 'headline))
           headline-id-p headline-id)
      ;; When have cleared caches, have to wait for gkroam-headline-db byte compile. After that, gkroam-goto-headline will be OK.
      (when headline
        (setq headline-id-p (cdr (assoc headline (db-get title gkroam-headline-db))))
        (setq headline-id (or headline-id-p (gkroam-set-headline-id title headline))))
      (if (string= (buffer-name) gkroam-capture-buf)
          (progn
            (other-window 1)
            (if headline
                (gkroam-goto-headline headline-id)
              (gkroam-find title)))
        (if headline
            (gkroam-goto-headline headline-id)
          (gkroam-find title))))))

(defun gkroam--link-has-headline ()
  "Judge if a gkroam link has headline after `re-search-forward'."
  (match-string-no-properties 5))

(defun gkroam--link-has-alias ()
  "Judge if a gkroam link has alias after `re-search-forward'."
  (match-string-no-properties 9))

(defun gkroam--fontify-hashtag ()
  "Highlight gkroam hashtag using text properties."
  (with-silent-modifications
    (add-text-properties (match-beginning 0) (match-end 0) '(face shadow))
    (add-text-properties (match-beginning 2) (match-end 2) '(display ""))
    (add-text-properties (match-beginning 4) (match-end 4) '(display ""))))

(defun gkroam-hashtag-fontify (beg end)
  "Put gkroam link between BEG and END."
  (when (gkroam-work-p)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward gkroam-hashtag-regexp end t)
        (gkroam--fontify-hashtag)
        (with-silent-modifications
          (make-text-button (match-beginning 0)
                            (match-end 0)
                            :type 'gkroam-link
                            'title (match-string-no-properties 3)))))))

(defun gkroam--fontify-hide-brackets ()
  "Hide gkroam link brackets using text properties."
  (with-silent-modifications
    (if (gkroam--link-has-alias)
        (progn
          (add-text-properties (match-beginning 9) (match-beginning 10) '(face warning))
          (add-text-properties (match-beginning 0) (match-beginning 9) '(display ""))
          (add-text-properties (match-beginning 10) (match-end 0) '(display "")))
      (if (gkroam--link-has-headline)
          (progn
            (add-text-properties (match-beginning 2) (match-end 3) '(face warning))
            (add-text-properties (match-beginning 0) (match-beginning 5) '(display ""))
            (add-text-properties (match-end 3) (match-end 0) '(display "")))
        (add-text-properties (match-beginning 2) (match-end 3)  '(face warning))
        (add-text-properties (match-beginning 0) (match-beginning 2) '(display ""))
        (add-text-properties (match-end 3) (match-end 0) '(display ""))))))

(defun gkroam--fontify-show-brackets ()
  "Show gkroam link brackets using text properties."
  (with-silent-modifications
    (if (gkroam--link-has-alias)
        (progn
          (when (gkroam--link-has-headline)
            (add-text-properties (match-beginning 4) (match-end 4) '(face shadow)))
          (remove-text-properties (match-beginning 0) (match-beginning 9) '(display nil))
          (remove-text-properties (match-beginning 10) (match-end 0) '(display nil))
          (add-text-properties (match-beginning 0) (match-beginning 2) '(face shadow))
          (add-text-properties (match-beginning 6) (match-beginning 9) '(face shadow))
          (add-text-properties (match-beginning 10) (match-end 0) '(face shadow))
          (add-text-properties (match-beginning 2) (match-beginning 6) '(face warning))
          (add-text-properties (match-beginning 9) (match-beginning 10) '(face warning)))
      (if (gkroam--link-has-headline)
          (progn
            (remove-text-properties (match-beginning 0) (match-beginning 5) '(display nil))
            (remove-text-properties (match-end 3) (match-end 0)'(display nil))
            (add-text-properties (match-beginning 0) (match-beginning 2) '(face shadow))
            (add-text-properties (match-end 3) (match-end 0)'(face shadow))
            (add-text-properties (match-beginning 4) (match-end 4) '(face shadow))
            (add-text-properties (match-beginning 2) (match-end 3) '(face warning)))
        (remove-text-properties (match-beginning 0) (match-beginning 2) '(display nil))
        (remove-text-properties (match-end 3) (match-end 0) '(display nil))
        (add-text-properties (match-beginning 0) (match-beginning 2) '(face shadow))
        (add-text-properties (match-end 3) (match-end 0) '(face shadow))
        (add-text-properties (match-beginning 2) (match-end 3) '(face warning))))))

(defun gkroam-link-fontify (beg end)
  "Put gkroam link between BEG and END."
  (when (gkroam-work-p)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward gkroam-link-regexp end t)
        (let* ((title (match-string-no-properties 2))
               (headline (when (gkroam--link-has-headline)
                           (match-string-no-properties 5)))
               (alias (when (gkroam--link-has-alias)
                        (match-string-no-properties 9)))
               (echo (if headline
                         (concat title " » " headline)
                       title)))
          (unless (equal (char-to-string (char-before (match-beginning 0))) "#")
            (if gkroam-show-brackets-p
                (gkroam--fontify-show-brackets)
              (gkroam--fontify-hide-brackets)))
          (with-silent-modifications
            (make-text-button (match-beginning 0)
                              (match-end 0)
                              :type 'gkroam-link
                              'title title
                              'headline headline
                              'help-echo echo)))))))

;;;###autoload
(defun gkroam-link-edit ()
  "Edit gkroam link in minibuffer."
  (interactive)
  (if-let ((btn (button-at (point))))
      (let* ((btn-label (button-label btn))
             (btn-start (button-start btn))
             (btn-end (button-end btn))
             (new-link
              (if (gkroam-selectrum-mode-p)
                  (completing-read "Edit link: " nil nil nil btn-label nil btn-label)
                (completing-read "Edit link: " nil nil nil btn-label))))
        (delete-region btn-start btn-end)
        (insert new-link)
        (save-buffer))
    (message "no link at point")))

;; gkroam backlink

(defvar gkroam-backlink-regexp
  "{{\\(.+?\\)\\(::\\([0-9]+\\)\\)?}{\\(.+?\\)}}"
  "Regular expression that matches a gkroam backlink")

(defun gkroam--format-backlink (page line-number alias)
  "Format gkroam backlink for PAGE, refer to a link in LINE-NUMBER line,
 display a description ALIAS."
  (if line-number
      (format "{{%s::%d}{%s}}" page line-number alias)
    (format "{{%s}{%s}}" page alias)))

(define-button-type 'gkroam-backlink
  'action #'gkroam-follow-backlink
  'face '(:underline nil)
  'page nil
  'line-number nil
  'follow-link t
  'help-echo "Jump back to page")

(defun gkroam-follow-backlink (button)
  "Jump to the page that BUTTON represents."
  (with-demoted-errors "Error when following the link: %s"
    (let* ((page (button-get button 'page))
           (title (gkroam-retrive-title page))
           (line-number (button-get button 'line-number)))
      (gkroam-find title)
      (when line-number
        (setq line-number (string-to-number line-number))
        (forward-line (- line-number (current-line) 1)))
      (recenter-top-bottom)
      (gkroam-fontify-link))))

(defun gkroam-backlink-fontify (beg end)
  "Highlight gkroam backlink."
  (when (gkroam-work-p)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward gkroam-backlink-regexp end t)
        (let* ((page (match-string-no-properties 1))
               (line-number (match-string-no-properties 3)))
          (with-silent-modifications
            (add-text-properties (match-beginning 0) (match-beginning 4)
                                 '(display ""))
            (add-text-properties (match-end 4) (match-end 0)
                                 '(display ""))
            (add-text-properties (match-beginning 4) (match-end 4)
                                 '(face link))
            (make-text-button (match-beginning 4)
                              (match-end 4)
                              :type 'gkroam-backlink
                              'page page
                              'line-number line-number)))))))

(define-minor-mode gkroam-link-mode
  "Recognize gkroam link."
  t nil nil
  (when (gkroam-work-p)
    (if gkroam-link-mode
        (progn
          (jit-lock-register #'gkroam-hashtag-fontify)
          (jit-lock-register #'gkroam-link-fontify)
          (jit-lock-register #'gkroam-backlink-fontify))
      (jit-lock-unregister #'gkroam-hashtag-fontify)
      (jit-lock-unregister #'gkroam-link-fontify)
      (jit-lock-unregister #'gkroam-backlink-fontify)))
  (jit-lock-refontify))

;; page beautify

(defun gkroam--fontify-org-checkbox (notation)
  "Highlight org checkbox with NOTATION."
  (add-text-properties
   (match-beginning 2) (1- (match-end 2)) `(display ,notation)))

(defun gkroam--fontify-org-list ()
  "Highlight org list, including bullet and checkbox."
  (with-silent-modifications
    (add-text-properties
     (match-beginning 1) (match-end 1)
     '(display "•"))
    (when (match-beginning 2)
      (pcase (match-string-no-properties 2)
        ("[-] " (gkroam--fontify-org-checkbox "☐"))
        ("[ ] " (gkroam--fontify-org-checkbox "☐"))
        ("[X] " (gkroam--fontify-org-checkbox "☑"))))))

(defun gkroam-org-list-fontify (beg end)
  "Highlight org list bullet between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward gkroam-org-list-re end t)
      (if (string= (match-string-no-properties 1) "*")
          (unless (= (match-beginning 0) (match-beginning 1))
            (gkroam--fontify-org-list))
        (gkroam--fontify-org-list)))))

(defun gkroam-overlay-region (beg end prop value)
  "Put overlays in region started by BEG and ended with END.
The overlays has a PROP and VALUE."
  (overlay-put (make-overlay beg end) prop value))

(defvar gkroam-list-parent-item-re
  "^\\(\\*\\{3\\} .+\\)?\n +\\(.+\\( > .+\\)*\\)\n \\{4\\}\\([0-9]+[).]\\|[*+-]\\) \\(\\[[ X-]\\] \\)?"
  "Regular expression that matches org plain list parent items in references.")

(defun gkroam-list-parent-item-overlay (beg)
  "Shadow plain list's parent items in references."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward gkroam-list-parent-item-re nil t)
      (with-silent-modifications
        (gkroam-overlay-region (match-beginning 2) (match-end 2)
                               'face 'shadow)))))

(defun gkroam-reference-region-overlay (beg)
  "Highlight all reference regions."
  (let* ((page (file-name-nondirectory (buffer-file-name)))
         (title (gkroam-retrive-title page))
         (end beg))
    (while (not (= end (point-max)))
      (save-excursion
        (goto-char beg)
        (if (re-search-forward "^\\*\\* .+" nil t 2)
            (setq end (1- (line-beginning-position)))
          (setq end (point-max))))
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (re-search-forward "^\\*\\* .+" nil t)
        (while (re-search-forward gkroam-backlink-regexp nil t)
          (catch 'continue
            (if (overlays-at (1- (point)))
                (throw 'continue nil)
              (let (elem content-start content-end)
                (goto-char (line-beginning-position))
                (skip-chars-forward "[ ]")
                (setq elem (org-element-at-point))
                (setq content-start (org-element-property :begin elem))
                (setq content-end (org-element-property :end elem))
                (gkroam-overlay-region content-start content-end
                                       'face 'hl-line)
                (goto-char content-end))))))
      (setq beg end))))

(defun gkroam-org-title-overlay (beg &optional bound)
  "Overlay org title, search between BEG and BOUND."
  (save-excursion
    (goto-char beg)
    (when (re-search-forward "\\(^ *#\\+TITLE: *\\)\\(.+\\)$" bound t)
      (if (and gkroam-mode gkroam-prettify-page-p)
          (progn
            (gkroam-overlay-region (match-beginning 1) (match-end 1) 'display "")
            (gkroam-overlay-region (match-beginning 2) (match-end 2)
                                   'face `(:height ,gkroam-title-height)))
        (remove-overlays (line-beginning-position) (line-end-position))))))

;;;###autoload
(define-minor-mode gkroam-prettify-mode
  "Minor mode for prettifying page."
  :lighter ""
  :keymap nil
  :require 'gkroam
  (when (gkroam-work-p)
    (if gkroam-prettify-mode
        (progn
          (jit-lock-register #'gkroam-org-list-fontify)
          (gkroam-org-title-overlay (point-min))
          (gkroam-org-list-fontify (point-min) (point-max)))
      (jit-lock-unregister #'gkroam-org-list-fontify)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward gkroam-org-list-re nil t)
          (with-silent-modifications
            (remove-text-properties (match-beginning 0) (match-end 0)
                                    '(display nil)))))
      (gkroam-org-title-overlay (point-min)))
    (jit-lock-refontify)))

(defun gkroam-set-window-margin ()
  "Set gkroam pages' window margin."
  (if (and gkroam-mode gkroam-prettify-page-p)
      (set-window-margins (selected-window)
                          gkroam-window-margin
                          gkroam-window-margin)
    (set-window-margins (selected-window) 0 0)))

(defun gkroam-prettify-page ()
  (when (and gkroam-mode (gkroam-work-p))
    (if gkroam-prettify-page-p
        (gkroam-prettify-mode 1)
      (gkroam-prettify-mode -1))
    (gkroam-set-window-margin)))

(defun gkroam-fontify-link ()
  "Highlight links and org symbols in all gkroam live windows."
  (when gkroam-mode
    (save-excursion
      (save-restriction
        (gkroam--narrow-to-content)
        (gkroam-hashtag-fontify (point-min) (point-max))
        (gkroam-link-fontify (point-min) (point-max)))
      (save-restriction
        (when (gkroam--narrow-to-reference)
          (gkroam-backlink-fontify (point-min) (point-max)))))))

;;;; commands

;;;###autoload
(defun gkroam-toggle-brackets ()
  "Determine whether to show brackets in page link."
  (interactive)
  (if gkroam-show-brackets-p
      (progn
        (setq gkroam-show-brackets-p nil)
        (message "Hide gkroam link brackets"))
    (setq gkroam-show-brackets-p t)
    (message "Show gkroam link brackets"))
  (let ((windows (window-list)))
    (save-selected-window
      (dolist (window windows)
        (select-window window)
        (gkroam-fontify-link)))))

;;;###autoload
(defun gkroam-toggle-prettify ()
  "Toggle gkroam page prettification."
  (interactive)
  (if gkroam-prettify-page-p
      (progn
        (setq gkroam-prettify-page-p nil)
        (message "Page prettification is turned off"))
    (setq gkroam-prettify-page-p t)
    (message "Page prettification is turned on"))
  (let ((windows (window-list)))
    (save-selected-window
      (dolist (window windows)
        (select-window window)
        (gkroam-prettify-page)
        (gkroam-fontify-link)))))

;;; ----------------------------------------
;; minor mode: gkroam-capture-mode

(defun gkroam-dwim-page ()
  "Get page from gkroam link, org link, region or at point."
  (let (title page)
    (cond
     ((button-at (point))
      (setq title (string-trim (button-label (button-at (point))) "#?{\\[" "\\]}")))
     ((get-text-property (point) 'htmlize-link)
      (setq page (string-trim-left
                  (plist-get (get-text-property (point) 'htmlize-link) :uri) "file:")))
     ((region-active-p)
      (setq title (buffer-substring-no-properties (region-beginning) (region-end))))
     ((thing-at-point 'word t)
      (setq title (thing-at-point 'word t)))
     (t (setq title "")))
    (unless (string-empty-p title)
      (if (or page (gkroam-retrive-page title))
          (cons (or page (gkroam-retrive-page title)) 'page)
        (cons title 'title)))))

(defun gkroam--get-content-region ()
  "Get the region of real contents.
The region is a begin position and end position cons."
  (let (content-beg content-end)
    (goto-char (point-min))
    (while (re-search-forward "^ *#\\+.+?:.*" nil t))
    (setq content-beg (1+ (match-end 0)))
    (if (re-search-forward gkroam-reference-delimiter-re nil t)
        (setq content-end (1- (match-beginning 0)))
      (setq content-end (point-max)))
    (cons content-beg content-end)))

(defun gkroam--get-content (page)
  "Get the real contents in PAGE.
Except mata infomation and page references."
  (let ((file (gkroam--get-file page))
        region beg end)
    (with-current-buffer (find-file-noselect file t)
      (setq region (gkroam--get-content-region))
      (setq beg (car region))
      (setq end (cdr region))
      (string-trim (buffer-substring-no-properties beg end)))))

(defun gkroam-capture-append--cons ()
  "Get the title and content cons needed to be appended to side window."
  (let ((title-or-page (car (gkroam-dwim-page)))
        (type (cdr (gkroam-dwim-page)))
        title page content)
    (when title-or-page
      (pcase type
        ('page
         (setq page title-or-page)
         (setq title (gkroam-retrive-title page))
         (setq content (gkroam--get-content page)))
        (_
         (setq title title-or-page)
         (setq content "")))
      (cons title content))))

(defun gkroam-capture-append--process (content)
  "Process the CONTENT of appended page to make sure the headline level is greater than one."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (while (re-search-forward "^*+ " nil t)
      (backward-char 1)
      (insert "*"))
    (buffer-string)))

(defun gkroam-capture-append (title content)
  "Append TITLE and CONTENT in gkroam capture buffer."
  (goto-char (point-min))
  (re-search-forward "^*" nil t)
  (goto-char (line-beginning-position))
  (newline-and-indent 2)
  (goto-char (point-min))
  (insert (format "* %s\n%s" title content)))

(defun gkroam-capture-write--process (content)
  "Process the CONTENT, restore the headline level when write back to pages."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (while (re-search-forward "^*+ " nil t)
      (backward-char 1)
      (delete-char -1))
    (buffer-string)))

(defun gkroam-capture-write-pages ()
  "Write the gkroam capture buffer contents to pages separately."
  (interactive)
  (let (title content page file plist beg end)
    (goto-char (point-min))
    (while (re-search-forward "^* .+" nil t)
      (setq title (string-trim-left (match-string-no-properties 0) "* "))
      (setq page (gkroam-retrive-page title))
      (if page
          (setq file (gkroam--get-file page))
        (let ((gkroam-use-default-filename nil))
          (setq file (gkroam-new title))))
      (goto-char (line-beginning-position))
      (setq plist (cadr (org-element-headline-parser (point-max))))
      (setq beg (plist-get plist :contents-begin))
      (setq end (plist-get plist :contents-end))
      (when (and beg end)
        (setq content (string-trim (buffer-substring beg end))))
      (setq content (gkroam-capture-write--process content))
      (goto-char end)
      (save-excursion
        (with-current-buffer (find-file-noselect file t)
          (let (region beg2 end2)
            (setq region (gkroam--get-content-region))
            (setq beg2 (car region))
            (setq end2 (cdr region))
            (delete-region beg2 end2)
            (goto-char beg2)
            (insert (format "\n%s\n" content))
            (org-mode)
            (save-buffer)))))))

(defun gkroam-reset-variables ()
  "Reset all variables gkroam capture relays on."
  (setq gkroam-capture-flag nil)
  (setq gkroam-capture-pages nil)
  (setq gkroam-return-wconf nil))

(defun gkroam-capture-finalize ()
  "Finalize current gkroam capture process, write content to pages ordinally and restore window configuration."
  (interactive)
  (gkroam-capture-write-pages)
  (kill-current-buffer)
  (set-window-configuration gkroam-return-wconf)
  (gkroam-reset-variables))

(defun gkroam-capture-kill ()
  "Abort current gkroam capture process and restore window configuration."
  (interactive)
  (kill-current-buffer)
  (set-window-configuration gkroam-return-wconf)
  (gkroam-reset-variables))

(defvar gkroam-capture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'gkroam-capture-finalize)
    (define-key map "\C-c\C-k" #'gkroam-capture-kill)
    map)
  "Keymap for `gkroam-capture-mode', a minor mode.
Use this map to set additional keybindings for when Gkroam mode is used
for a side capture buffer.")

(defvar gkroam-capture-mode-hook nil
  "Hook for the `gkroam-capture-mode' minor mode.")

(define-minor-mode gkroam-capture-mode
  "Minor mode for special key bindings in a gkroam capture buffer.
Turning on this mode runs the normal hook `gkroam-capture-mode-hook'."
  nil " capture" gkroam-capture-mode-map
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<gkroam-capture-mode-map>Capture buffer, finish \
`\\[gkroam-capture-finalize]', abort `\\[gkroam-capture-kill]'.")))

;;;###autoload
(defun gkroam-capture ()
  "Temporary capture pages in side window."
  (interactive)
  (let* ((cons (gkroam-capture-append--cons))
         title page content)
    (if (null cons)
        (progn
          (setq title (completing-read "Choose a page to edit or capture a new one: "
                                       (gkroam-retrive-all-titles) nil nil))
          (setq page (gkroam-retrive-page title))
          (if page
              (setq content (gkroam-capture-append--process
                             (gkroam--get-content page)))
            (setq content "")))
      (setq title (car cons))
      (setq content (gkroam-capture-append--process (cdr cons))))
    (if (member title gkroam-capture-pages)
        (message "'%s' page is already in capture buffer!" title)
      (push title gkroam-capture-pages)
      (if (null gkroam-capture-flag)
          (progn
            (setq gkroam-return-wconf
                  (current-window-configuration))
            (delete-other-windows)
            (split-window-right)
            (other-window 1)
            (switch-to-buffer gkroam-capture-buf)
            (gkroam-capture-append title content)
            (org-mode)
            (gkroam-capture-mode)
            (setq gkroam-capture-flag t))
        (select-window (get-buffer-window gkroam-capture-buf))
        (gkroam-capture-append title content)
        (org-mode)
        (gkroam-capture-mode)))))

;; ----------------------------------------
;; gkroam mode

(defun gkroam-company-bracket-p ()
  "Judge if need to company bracket link."
  (save-excursion
    (let (word)
      (setq word (thing-at-point 'word t))
      (backward-word 1)
      (backward-char 2)
      (string= (thing-at-point 'sexp t)
               (format "{[%s]}" word)))))

(defun gkroam-company-hashtag-p ()
  "Judge if need to company hashtag link."
  (save-excursion
    (skip-chars-backward "^#" (line-beginning-position))
    (and (not (= (line-beginning-position) (point)))
         (thing-at-point 'word t))))

(defun gkroam--complete-hashtag (title)
  "Complete hashtag with brackets for TITLE."
  (let (len)
    (when (gkroam-company-hashtag-p)
      (save-excursion
        (setq len (abs (skip-chars-backward "^#")))
        (insert "{[")
        (forward-char len)
        (insert "]}")))
    title))

(defun gkroam-completion-finish (title)
  "Function binded to `company-completion-finished-hook' after finishing complete TITLE."
  (when (gkroam-company-hashtag-p)
    (gkroam--complete-hashtag title))
  (unless (string= (buffer-name) gkroam-capture-buf)
    (save-buffer)))

(defun gkroam-completion-at-point ()
  "Function binded to `completion-at-point-functions'."
  (interactive)
  (let (bds beg end)
    (cond
     ((gkroam-company-bracket-p)
      (setq bds (bounds-of-thing-at-point 'list))
      (setq beg (1+ (car bds)))
      (setq end (1- (cdr bds)))
      (list beg end gkroam-pages . nil))
     ((gkroam-company-hashtag-p)
      (setq bds (bounds-of-thing-at-point 'symbol))
      (setq beg (car bds))
      (setq end (cdr bds))
      (list beg end gkroam-pages . nil)))))

(defun gkroam-ivy-use-selectable-prompt (boolean)
  "Set `ivy-use-selectable-prompt' to BOOLEAN."
  (when (require 'ivy nil t)
    (when ivy-mode (setq ivy-use-selectable-prompt boolean))))

(defun gkroam-selectrum-mode-p ()
  "Judge if selectrum is installed and selectrum-mode is turned on."
  (when (require 'selectrum nil t)
    selectrum-mode))

;;;###autoload
(define-minor-mode gkroam-mode
  "Minor mode for gkroam."
  :lighter " Gkroam"
  :keymap (let ((map (make-sparse-keymap))) map)
  :require 'gkroam
  :global t
  (if gkroam-mode
      (progn
        ;; (gkroam-rebuild-caches)
        (gkroam-link-mode 1)
        (gkroam-refresh-page)
        (add-hook 'completion-at-point-functions #'gkroam-completion-at-point nil 'local)
        (add-hook 'company-completion-finished-hook #'gkroam-completion-finish nil 'local)
        (add-hook 'org-mode-hook #'gkroam-link-mode)
        (add-hook 'org-mode-hook (lambda ()
                                   (when (gkroam-work-p)
                                     (toggle-truncate-lines)
                                     (gkroam-link-frame-setup 'find-file)
                                     (gkroam-ivy-use-selectable-prompt t)
                                     (setq org-startup-folded nil)
                                     (setq org-return-follows-link t)
                                     (setq gkroam-pages (gkroam-retrive-all-titles))))))
    ;; how to preserve the original variable value?
    (remove-hook 'completion-at-point-functions #'gkroam-completion-at-point 'local)
    (remove-hook 'company-completion-finished-hook #'gkroam-completion-finish 'local)
    (remove-hook 'org-mode-hook #'gkroam-link-mode)
    (remove-hook 'org-mode-hook
                 (lambda ()
                   (when (gkroam-work-p)
                     (toggle-truncate-lines)
                     (gkroam-link-frame-setup 'find-file)
                     (gkroam-ivy-use-selectable-prompt t)
                     (setq org-startup-folded nil)
                     (setq org-return-follows-link t)
                     (setq gkroam-pages (gkroam-retrive-all-titles)))))
    (gkroam-prettify-mode -1)
    (gkroam-link-mode -1)
    (set-window-margins (selected-window) 0 0)
    (with-silent-modifications
      (set-text-properties (point-min) (point-max) nil))))

(provide 'gkroam)
;;; gkroam.el ends here
