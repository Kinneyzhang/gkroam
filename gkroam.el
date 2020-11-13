;;; gkroam.el --- A lightweight org-mode roam replica  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 2.4.1
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

;; v2.4.0

;; 1. Delete =gkroam-toggle-dynamic= command

;; 2. Set gkroam-mode as a minor mode, instead of a major mode derived from org-mode

;; 3. Use text properties to render gkroam links.

;; 4. More caches and a big improvement in performance.

;;    4.1 Cache gkroam pages and their filenames.
;;    4.2 Cache gkroam pages and their references.

;; 5. Prettify and enhance linked references
;;    5.1 Change backlink format to "{{page::line-number}{alias}}"
;;    5.2 Show list item's parent items above it and shadow them.
;;    5.3 Highlight each reference region.
;;    5.4 Jump back to the specific line when click backlink.

;; v2.4.1
;; 1. Implement a roam research like index buffer.
;; 2. Add new command `gkroam-delete'.
;; 3. Rename 'gkroam-smart-new' to `gkroam-dwim'.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'org-element)
(require 'db)
(require 'company)
(eval-when-compile
  (require 'subr-x)
  (require 'hl-line)
  (require 'crm))

;;;; Declarations
(defvar org-link-frame-setup)
(defvar ivy-use-selectable-prompt)

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

(defvar gkroam-crm-separator "[ \t]*|[ \t]*"
  "Default value of `crm-separator' in gkroam.")

(defvar gkroam-link-face 'font-lock-type-face
  "Face for gkroam link.")

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

(defvar gkroam-linked-reference-db
  (db-make
   `(db-hash
     :filename ,(concat gkroam-cache-dir "gkroam-linked-reference-db")))
  "Database for caching gkroam page's linked references.")

(defvar gkroam-unlinked-reference-db
  (db-make
   `(db-hash
     :filename ,(concat gkroam-cache-dir "gkroam-unlinked-reference-db")))
  "Database for caching gkroam page's unlinked references.")

(defvar gkroam-org-list-re
  "^ *\\([0-9]+[).]\\|[*+-]\\) \\(\\[[ X-]\\] \\)?"
  "Org list bullet and checkbox regexp.")

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

(defvar gkroam-linked-reference-delimiter-re
  "^* \\([0-9]+\\) Linked References.*"
  "Delimiter string regexp to separate page contents from references region.")

(defvar gkroam-unlinked-reference-delimiter-re
  "^* \\([0-9]+\\) Unlinked References to \"\\(.+?\\)\".*"
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

(defvar gkroam-mentions-buf "*Gkroam Mentions*"
  "Gkroam mentions buffer name.")

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
  "Check if current file exists in `gkroam-root-dir'."
  (when (buffer-file-name)
    (file-equal-p (file-name-directory (buffer-file-name))
                  (expand-file-name gkroam-root-dir))))

(defun gkroam-at-index-buf ()
  "Check if current buffer is `gkroam-index-buf'."
  (string= (buffer-name) gkroam-index-buf))

(defun gkroam-at-capture-buf ()
  "Check if current buffer is `gkroam-capture-buf'."
  (string= (buffer-name) gkroam-capture-buf))

(defun gkroam-at-mentions-buf ()
  "Check if current buffer is `gkroam-mentions-buf'."
  (string= (buffer-name) gkroam-mentions-buf))

(defun gkroam-work-p ()
  "Check if current file or buffer is where gkroam has to be in work."
  (or (gkroam-at-root-p)
      (gkroam-at-index-buf)
      (gkroam-at-capture-buf)
      (gkroam-at-mentions-buf)
      (gkroam-at-unlinked-buf)))

(defun gkroam-case-fold-string= (a b)
  "Ignore case sensitive when compare A and B using `string='."
  (eq t (compare-strings a nil nil b nil nil t)))

(defun gkroam-title-exist-p (title)
  "Check if the case fold TITLE page exists in `gkroam-root-dir'.
Return the original title if TITLE does't equal to the original title.
Return non-nil if TITLE equals to the original title.
Return nil if TITLE doesn't exist in gkroam pages."
  (let ((case-title (car (cl-member title (gkroam-retrive-all-titles)
                                    :test #'gkroam-case-fold-string=))))
    (when case-title
      (when (string= title case-title)
        (setq case-title t)))
    case-title))

(defun gkroam-retrive-page (title)
  "Retrive page's filename from database.
The page has a title named TITLE."
  (gkroam-db-get gkroam-page-db title "page"))

(defun gkroam-retrive-title (page)
  "Retrive page's title from database.
The page has a filename named PAGE."
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
    (let ((content-beg (point-min)))
      (goto-char (point-min))
      (when (re-search-forward "\\(^ *#+.+:.+\n+\\)+" nil t)
        (setq content-beg (point)))
      (when (re-search-forward gkroam-linked-reference-delimiter-re nil t)
        (unless (> (point-min) (1- (line-beginning-position)))
          (narrow-to-region content-beg (1- (line-beginning-position))))))))

(defun gkroam--narrow-to-reference ()
  "Narrow region to page references if there is a reference region."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward gkroam-linked-reference-delimiter-re nil t)
      (narrow-to-region (line-beginning-position) (point-max)))))

(defun gkroam-new (title)
  "Just create a new gkroam page titled with TITLE."
  (let ((file (gkroam--gen-file)))
    (with-current-buffer (find-file-noselect file t)
      (insert (format "#+TITLE: %s\n\n" title))
      (save-buffer))
    (gkroam-cache-curr-page title)
    file))

;; ----------------------------------------
;;;;; linked references

(defvar gkroam-link-re-format "{\\[%s.*?\\]}"
  "Gkroam link regexp format used for searching link context.")

(defun gkroam-start-process (buf-name args)
  "Start a rg process with output buffer named BUF-NAME.
ARGS are the arguments of rg process."
  (let ((name (generate-new-buffer-name buf-name)))
    (apply #'start-process `(,name ,name "rg" ,@args
                                   ,(expand-file-name gkroam-root-dir)))))

(defun gkroam-search-process (process callback)
  "Call CALLBACK After the PROCESS finished."
  (unless (executable-find "rg")
    (user-error "Cannot find program 'rg'."))
  (let ((sentinel
         (lambda (process event)
           (if (string-match-p (rx (or "finished" "exited"))
                               event)
               (if-let ((buf (process-buffer process)))
                   (with-current-buffer buf
                     (save-excursion
                       (funcall callback (buffer-string))))
                 (error "Gkroam’s rg process’ buffer is killed"))
             (error "Gkroam’s rg process failed with signal: %s"
                    event)))))
    (set-process-sentinel process sentinel)))

(defun gkroam--process-linked-backlink (string page line-number)
  "Convert gkroam link to backlink in STRING.
The backlink refers to a link in LINE-NUMBER line of PAGE."
  (with-temp-buffer
    (insert string)
    ;; (goto-char (point-min))
    ;; (while (re-search-forward gkroam-hashtag-regexp nil t)
    ;;   (replace-match (gkroam--format-backlink
    ;;                   page line-number
    ;;                   (concat "#" (match-string-no-properties 3)))))
    (goto-char (point-min))
    (while (re-search-forward gkroam-link-regexp nil t)
      (let* ((title (match-string-no-properties 2))
             (alias (match-string-no-properties 9))
             (headline (match-string-no-properties 5)))
        (if alias
            (replace-match (gkroam--format-backlink nil line-number alias))
          (if headline
              (replace-match (gkroam--format-backlink nil line-number headline))
            (replace-match (gkroam--format-backlink nil line-number title))))))
    (buffer-string)))

;; (define-button-type 'gkroam-unlinked-link
;;   'action #'gkroam-link-unlinked
;;   'face '(:bold t :underline t)
;;   'line-number nil
;;   'follow-link t
;;   'help-echo "Link it to reference")

(defvar gkroam-unlinked-buf "*Gkroam Unlinked*"
  "Gkroam unlinked references buffer name.")

(defun gkroam-at-unlinked-buf ()
  "Check if current buffer is `gkroam-unlinked-buf'"
  (string= (buffer-name) gkroam-unlinked-buf))

(defun gkroam-unlinked-title-fontify (beg end)
  "Highlight gkroam backlink between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "{{\\([0-9]+\\)}{\\(.+?\\)}}" end t)
      (let ((line-number (match-string-no-properties 1))
            (title (match-string-no-properties 2)))
        (with-silent-modifications
          (add-text-properties (match-beginning 0) (match-beginning 2)
                               '(display ""))
          (add-text-properties (match-end 2) (match-end 0)
                               '(display ""))
          (add-text-properties (match-beginning 2) (match-end 2)
                               '(face underline))
          (make-text-button (match-beginning 2)
                            (match-end 2)
                            :type 'gkroam-backlink
                            'title title
                            'line-number line-number
                            'help-echo "Link this to references."))))))

(defun gkroam--process-unlinked-title (title string line-number)
  "Highlight unlinked TITLE in STRING,
include the original line number in text property."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward (regexp-quote title) nil t)
      (when (gkroam--unlinked-title-valid-p)
        (replace-match (gkroam--format-backlink nil line-number title))))
    (buffer-string)))

(defun gkroam--format-linked-reference-content ()
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
         (let* ((parent-strs "")
                (relative-level 0)
                parent parent-beg parent-str)
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
                   (cl-incf relative-level)
                   (setq parent-str
                         (buffer-substring-no-properties (point)
                                                         (line-end-position))))
                 (when (= relative-level 1)
                   (setq parent-strs (concat parent-str "\n" parent-strs)))
                 (when (> relative-level 1)
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

(defun gkroam--format-unlinked-reference-content ()
  "Format the content of unlinked reference."
  (when (gkroam--unlinked-title-valid-p)
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun gkroam--unlinked-title-valid-p ()
  "Judge if current macth is a valid unlinked title after searching.
The following conditions should be excluded:
1. title is in a org headline.
2. title is a substring of words or phrase. (Chinese excluded)
3. title is in a gkroam link, hashtag or org link."
  (save-excursion
    (let* ((title (match-string-no-properties 0))
           (is-ascii (equal (find-charset-string title)
                            '(ascii)))
           valid-p)
      (pcase title
        ;; case 1
        ((guard (save-excursion
                  (goto-char (line-beginning-position))
                  (looking-at "^*+ .+")))
         (setq valid-p nil))
        ;; case 2
        ((and (guard is-ascii)
              (guard (not
                      (and
                       (string= " " (string (char-before
                                             (match-beginning 0))))
                       (string= " " (string (char-after
                                             (match-end 0))))))))
         (setq valid-p nil))
        ;; case 3
        ;; ((guard (or (button-at (point))
        ;;             (get-text-property (point) 'htmlize-link)))
        ;;  (setq valid-p nil))
        (_ (setq valid-p t)))
      valid-p)))

(defvar gkroam-file-path-re
  (concat "^" (expand-file-name gkroam-root-dir) ".+\\.org")
  "Regular expression to match gkroam file path.")

(defun gkroam--get-rg-output-region ()
  "Split rg output to different regions according to different pages.
Each region is a list consist of '(beg-postion end-position page-name)'.
Return a list of all region lists."
  (let ((beg (point-min))
        (end (point))
        page region-lst)
    (save-excursion
      (while (not (= end (point-max)))
        (goto-char beg)
        (save-excursion
          (when (re-search-forward gkroam-file-path-re nil t)
            (setq page (file-name-nondirectory
                        (match-string-no-properties 0)))))
        (if (re-search-forward gkroam-file-path-re nil t 2)
            (setq end (line-beginning-position))
          (setq end (point-max)))
        (push `(,beg ,end ,page) region-lst)
        (setq beg end)))
    (reverse region-lst)))

(defun gkroam--process-searched-string (type string title)
  "Process the TYPE type of searched STRING by 'rg', get the whole contents of TITLE page.
If type is 'linked', it means to process linked references.
If type is 'unlinked', it means to process unlinked references."
  (if (string-empty-p string)
      (cons 0 "")
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (let ((region-lst (gkroam--get-rg-output-region))
            (num 0) references)
        (dolist (region region-lst)
          (let ((beg (nth 0 region))
                (end (nth 1 region))
                (page (nth 2 region))
                (regexp-format
                 (pcase type
                   ('linked gkroam-link-re-format)
                   ('unlinked "%s")))
                (context nil))
            (goto-char beg)
            (save-restriction
              (let (bound-end
                    (last-headline "")
                    (last-content ""))
                (narrow-to-region beg end)
                ;; 'narrow in narrow' does not succeed.
                (goto-char (point-min))
                (re-search-forward "\\(^ *#+.+:.+\n+\\)+" nil t)
                (save-excursion
                  (if (re-search-forward gkroam-linked-reference-delimiter-re nil t)
                      (setq bound-end (match-beginning 0))
                    (setq bound-end (point-max))))
                (while (re-search-forward
                        (format regexp-format (regexp-quote title))
                        bound-end t)
                  (let* ((headline "")
                         (line-number (line-number-at-pos))
                         (raw-content
                          (pcase type
                            ('linked (string-trim
                                      (gkroam--format-linked-reference-content)
                                      nil "[ \t\n\r]+"))
                            ('unlinked
                             (when-let
                                 ((unlinked-content
                                   (gkroam--format-unlinked-reference-content)))
                               (string-trim unlinked-content nil "[ \t\n\r]+")))))
                         (content
                          (pcase type
                            ('linked (gkroam--process-linked-backlink
                                      raw-content page line-number))
                            ('unlinked
                             (when raw-content
                               (gkroam--process-unlinked-title
                                title raw-content line-number))))))
                    (when content
                      (save-excursion
                        (when (re-search-backward "^*+ .+\n" nil t)
                          (setq headline
                                (string-trim (match-string-no-properties 0) "*+ +" nil))
                          (setq headline (concat "*** " headline))))
                      (if (string= headline last-headline)
                          (pcase type
                            ('linked
                             (setq num (1+ num))
                             (setq context (concat context content "\n\n")))
                            ('unlinked
                             (unless (string= last-content content)
                               (setq num (1+ num))
                               (setq context (concat context content "\n\n")))))
                        (setq num (1+ num))
                        (setq context (concat context headline "\n" content "\n\n"))
                        (setq last-headline headline))
                      ;; always set current content to last-content
                      ;; regardless of headline equal or not.
                      (setq last-content content))))))
            (when context
              (setq references
                    (concat references
                            (format "** %s\n\n%s"
                                    (gkroam--format-backlink
                                     page 0 (gkroam-retrive-title page))
                                    context))))))
        (if references
            (setq references
                  (with-temp-buffer
                    (insert (string-trim references))
                    (org-mode)
                    (indent-region (point-min) (point-max))
                    (buffer-string)))
          (setq references ""))
        (cons num references)))))

(defun gkroam-search-page-link (title)
  "Return a rg process to search a specific TITLE page's link.
Output matched files' path and context."
  (gkroam-start-process " *gkroam-rg*"
                        `(,(format "\\{\\[%s( ».*)?\\](\\[.+?\\])?\\}"
                                   title)
                          ;; Write a funciton to generate rg regexp
                          ;; format for title!
                          "--ignore-case" "--sortr" "path"
                          "-C" ,(number-to-string 9999)
                          "-N" "--heading"
                          "-g" "!index.org*")))

(defun gkroam-search-page-title (title)
  "Return a rg process to search a specific PAGE's title.
Output the context including the TITLE."
  (gkroam-start-process " *gkroam-unlinked-references*"
                        `(,title
                          ;; Write a funciton to generate rg regexp
                          ;; format for title!
                          "--ignore-case" "--sortr" "path"
                          "-C" ,(number-to-string 9999)
                          "-N" "--heading"
                          "-g" ,(format
                                 "!%s"
                                 (gkroam-retrive-page title)))))

(defun gkroam-update-linked-references (title)
  "Update gkroam TITLE page's linked reference."
  (gkroam-search-process
   (gkroam-search-page-link title)
   (lambda (string)
     (let* ((page (gkroam-retrive-page title))
            (file (gkroam--get-file page))
            (file-buf (find-file-noselect file t))
            (processed-str (gkroam--process-searched-string
                            'linked string title))
            (num (number-to-string (car processed-str)))
            (references (cdr processed-str))
            (reference-db gkroam-linked-reference-db)
            (cached-references (cdar (db-get title reference-db)))
            (delimiter-re gkroam-linked-reference-delimiter-re)
            (delimiter-format "* %s Linked References to \"%s\"\n\n")
            reference-start curr-references reference-content-start)
       (unless (string= references cached-references)
         (db-put title `(("reference" . ,references)) reference-db)
         (setq cached-references (cdar (db-get title reference-db))))
       ;; compare searched references string with cached references string.
       ;; If not equal, cache searched one.
       (with-current-buffer file-buf
         (save-excursion
           (goto-char (point-max))
           (re-search-backward delimiter-re nil t)
           (setq reference-start (point))
           (forward-line 2)
           (setq reference-content-start (point))
           (setq curr-references
                 (string-trim (buffer-substring-no-properties
                               reference-content-start (point-max))))
           ;; Unindent current page's rerferences string and compare with
           ;; the cached ones. If they are different,
           ;; update current page's references.
           (unless (string= curr-references cached-references)
             (let ((inhibit-read-only t))
               (remove-text-properties reference-start (point-max) '(read-only nil)))
             (delete-region reference-start (point-max))
             (unless (string-empty-p string)
               (insert (format delimiter-format num title))
               (insert references)
               ;; use overlay to hide part of reference. (filter)
               ;; (gkroam-overlay-region beg (point-max) 'invisible t)
               (indent-region reference-content-start (point-max))
               (save-buffer)
               (gkroam-db-update gkroam-page-db title "mention" num)
               (message "%s reference updated" page)))
           (gkroam-list-parent-item-overlay reference-start)
           (gkroam-reference-region-overlay reference-start)
           (when gkroam-prettify-page-p
             (gkroam-org-list-fontify reference-start (point-max)))
           (gkroam-backlink-fontify reference-start (point-max))
           (unless (get-text-property reference-start 'read-only)
             (put-text-property reference-start (point-max)
                                'read-only "References region is uneditable."))))))))

(defun gkroam-set-unlinked-references (title)
  "Set unlinked references of TITLE gkroam page to `gkroam-unlinked-buf'."
  (gkroam-search-process
   (gkroam-search-page-title title)
   (lambda (string)
     (let* ((buf (get-buffer-create gkroam-unlinked-buf))
            (processed-str (gkroam--process-searched-string
                            'unlinked string title))
            (num (number-to-string (car processed-str)))
            (references (cdr processed-str))
            (reference-db gkroam-unlinked-reference-db)
            (cached-references (cdar (db-get title reference-db)))
            (reference-title "* %s unlinked references to \"%s\"\n\n"))
       (unless (string= references cached-references)
         (db-put title `(("reference" . ,references)) reference-db)
         (setq cached-references (cdar (db-get title reference-db))))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (remove-text-properties (point-min) (point-max) '(read-only nil)))
         (erase-buffer)
         (org-mode)
         (insert "#+TITLE: UNLINKED REFERENCES\n\n")
         (if (string-empty-p references)
             (progn
               (insert (format "* None unlinked references to \"%s\"\n" title))
               (gkroam-org-title-overlay (point-min)))
           (insert (format reference-title num title))
           (insert cached-references)
           (indent-region (point-min) (point-max))
           ;; use overlay to hide part of reference. (filter)
           ;; (gkroam-overlay-region beg (point-max) 'invisible t)
           (jit-lock-register #'gkroam-unlinked-title-fontify)
           (gkroam-unlinked-title-fontify (point-min) (point-max))
           (gkroam-backlink-fontify (point-min) (point-max))
           (gkroam-link-fontify (point-min) (point-max))
           (gkroam-hashtag-fontify (point-min) (point-max))
           (gkroam-prettify-page)
           ;; (gkroam-list-parent-item-overlay (point-min))
           ;; (gkroam-reference-region-overlay (point-min))
           ;; (when gkroam-prettify-page-p
           ;;   (gkroam-org-list-fontify (point-min) (point-max))))
           )
         (goto-char (point-min))
         (put-text-property (point-min) (point-max)
                            'read-only "References region is uneditable.")
         (gkroam-mentions-mode)))))
  (let ((buf gkroam-unlinked-buf))
    (when (null gkroam-mentions-flag)
      (setq gkroam-return-wconf (current-window-configuration)))
    (setq gkroam-mentions-flag t)
    (if (get-buffer-window buf)
        (progn
          (select-window (get-buffer-window buf))
          (let ((inhibit-read-only t))
            (remove-text-properties (point-min) (point-max) '(read-only nil))
            (erase-buffer)))
      (delete-other-windows)
      (split-window-right)
      (other-window 1)
      (switch-to-buffer buf))
    (insert (propertize "Calculating unlinked references..."
                        'face '(italic)))))

;;;###autoload
(defun gkroam-show-unlinked ()
  "Show unlinked references of current page in a side window."
  (interactive)
  (if (gkroam-at-root-p)
      (let ((title (gkroam-retrive-title
                    (file-name-nondirectory (buffer-file-name)))))
        (gkroam-set-unlinked-references title))
    (message "Not in the gkroam directory!")))

;;;###autoload
(defun gkroam-link-unlinked (title line-number)
  "Transform unlinked references to linked references for TITLE page."
  (interactive)
  (let* ((page
          (save-excursion
            (when (re-search-backward gkroam-backlink-full-regexp nil t)
              (match-string-no-properties 1))))
         (page-buf (find-file-noselect (gkroam--get-file page))))
    (save-excursion
      (if (get-buffer-window page-buf)
          (save-selected-window
            (select-window (get-buffer-window page-buf))
            (forward-line (- line-number (line-number-at-pos) 1))
            (catch 'break
              (while (re-search-forward title (line-end-position) t)
                (when (gkroam--unlinked-title-valid-p)
                  (gkroam-new-from-region (match-beginning 0) (match-end 0))
                  (gkroam-set-unlinked-references title)
                  (throw 'break
                         (message "Have linked this to references."))))))
        (with-current-buffer page-buf
          (forward-line (- line-number (line-number-at-pos) 1))
          (catch 'break
            (while (re-search-forward title (line-end-position) t)
              (when (gkroam--unlinked-title-valid-p)
                (gkroam-new-from-region (match-beginning 0) (match-end 0))
                (gkroam-set-unlinked-references title)
                (throw 'break
                       (message "Have linked this to references."))))))))))

;; ----------------------------------------
;;;;; headline linked references

(defun gkroam--get-headlines (title)
  "Get page's headline list, the page is titled with TITLE."
  (with-temp-buffer
    (insert-file-contents
     ;; do not use `insert-file-contents-literally',
     ;; it cannot show chinese normally.
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
  (when-let ((page (gkroam-retrive-page title)))
    (let* ((file (gkroam--get-file page))
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
      headline-id)))

;; ----------------------------------------
;;;;; gkroam cache

(defvar gkroam-wc-regexp-chinese-char-and-punc
  (rx (category chinese))
  "Regular expression to match Chinese characters 
and punctuations.")

(defvar gkroam-wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]"
  "Regular expression to match Chinese punctuations.")

(defvar gkroam-wc-regexp-english-word
  "[a-zA-Z0-9-]+"
  "Regular expression to match English words.")

(defun gkroam-word-count ()
  "Count gkroam page' words."
  (interactive)
  (let ((v-buffer-string
         (if (eq major-mode 'org-mode)
             (replace-regexp-in-string
              "^#\\+.+" ""
	      (buffer-substring-no-properties
               (point-min) (point-max)))
           (buffer-substring-no-properties
            (point-min) (point-max))))
        (chinese-char-and-punc 0)
        (chinese-punc 0)
        (english-word 0)
        (chinese-char 0))
    (with-temp-buffer
      (setq v-buffer-string
            (replace-regexp-in-string
             (format "^ *%s *.+" comment-start) "" v-buffer-string))
      (insert v-buffer-string)
      (goto-char (point-min))
      (while (re-search-forward gkroam-wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      (goto-char (point-min))
      (while (re-search-forward gkroam-wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      (goto-char (point-min))
      (while (re-search-forward gkroam-wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (+ chinese-char english-word)))

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
          (when (re-search-forward (format "^ *#\\+TITLE: *%s *$" (regexp-quote title)) nil t)
            (throw 'break page)))))))

(defun gkroam--all-pages ()
  "Get all gkroam pages."
  (directory-files gkroam-root-dir nil "^[^.#].+\\.org$"))

(defun gkroam--convert-date-num-to-string (date-num)
  "Convert a \"%Y%m%d%H%M%S\" format DATE-NUM
to a \"%Y-%m-%d %H-%M-%S\" time string."
  (let ((year (substring date-num 0 4))
        (month (substring date-num 4 6))
        (day (substring date-num 6 8))
        (hour (substring date-num 8 10))
        (minute (substring date-num 10 12))
        (second (substring date-num 12 14)))
    (format "%s-%s-%s %s:%s:%s"
            year month day hour minute second)))

(defun gkroam--format-date-string (string)
  "Format org date STRING to a \"%b %d, %Y\" time format."
  (cond
   ((string-match
     "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)?" string)
    (format-time-string "%b %d, %Y" (date-to-time string)))
   (t string)))

(defun gkroam--get-meta (meta &optional page)
  "Get PAGE's META value."
  (let* ((page (or page (file-name-nondirectory (buffer-file-name))))
         (date-num (string-trim-right page "\\.org"))
         title word-count mentions created-time updated-time org-date)
    (with-temp-buffer
      (insert-file-contents (gkroam--get-file page) nil)
      (setq-local major-mode 'org-mode)
      (setq word-count (gkroam-word-count))
      (goto-char (point-min))
      (if (re-search-forward "^ *#\\+TITLE:" nil t)
          (setq title (string-trim (buffer-substring (match-end 0) (line-end-position))))
        (error "%s doesn't have a title!" page))
      (if (re-search-forward gkroam-linked-reference-delimiter-re nil t)
          (setq mentions (match-string-no-properties 1))
        (setq mentions "0"))
      (setq created-time
            (if (string-match "[0-9]\\{14\\}" date-num)
                (format-time-string
                 "%b %d, %Y"
                 (date-to-time
                  (gkroam--convert-date-num-to-string date-num)))
              (goto-char (point-min))
              (if (re-search-forward "^ *#\\+DATE:" nil t)
                  (setq org-date (string-trim (buffer-substring (match-end 0) (line-end-position))))
                (setq org-date "unknow"))
              (gkroam--format-date-string org-date)))
      (setq updated-time (format-time-string
                          "%b %d, %Y"
                          (file-attribute-modification-time
                           (file-attributes (gkroam--get-file page)))))
      (pcase meta
        (:title title)
        (:count word-count)
        (:mention mentions)
        (:create created-time)
        (:update updated-time)))))

(defun gkroam--all-titles ()
  "Get all gkroam titles."
  (let* ((pages (gkroam--all-pages)))
    (mapcar (lambda (page) (gkroam--get-meta :title page)) pages)))

(defun gkroam-cache-curr-page (title)
  "Cache gkroam page's filename, which titled with TITLE."
  (let* ((db-page (gkroam-db-get gkroam-page-db title "page"))
         (page (gkroam--get-page title))
         (word-count (gkroam--get-meta :count page))
         (mentions (gkroam--get-meta :mention page))
         (created-time (gkroam--get-meta :create page))
         (updated-time (gkroam--get-meta :update page)))
    (unless (equal db-page page)
      (db-put title `(("page" . ,page)
                      ("count" . ,word-count)
                      ("mention" . ,mentions)
                      ("create" . ,created-time)
                      ("update" . ,updated-time))
              gkroam-page-db))))

(defun gkroam-db-get (db title key)
  "Get KEY attribute's value of TITLE page from DB database."
  (cdr (assoc key (db-get title db))))

(defun gkroam-db-update (db title key new-val)
  "For TITLE record in DB database, update KEY's value to NEW-VAL."
  (unless (equal new-val (gkroam-db-get db title key))
    (let* ((old-alist (db-get title db))
           (new-alist (mapcar
                       (lambda (cons)
                         (if (string= key (car cons))
                             (cons key new-val)
                           cons))
                       old-alist)))
      (db-put title new-alist db)
      (message "%s page cache updated" title))))

(defun gkroam-update-page-cache ()
  "Update current gkroam page's cache."
  (when (and gkroam-mode (gkroam-work-p))
    (let* ((page (file-name-nondirectory (buffer-file-name)))
           (title (gkroam-retrive-title page)))
      (unless (null title)
        (gkroam-db-update gkroam-page-db title "page" page)
        (gkroam-db-update gkroam-page-db title "count" (gkroam-word-count))
        (gkroam-db-update gkroam-page-db title "update"
                          (gkroam--get-meta :update page))
        (gkroam-db-update gkroam-page-db title "create"
                          (gkroam--get-meta :create page))))))

;;;###autoload
(defun gkroam-cache-all-pages ()
  "Cache all gkroam pages' title and filename."
  (let* ((titles (gkroam--all-titles)))
    (dolist (title titles)
      (gkroam-cache-curr-page title))))

(defun gkroam-cache-curr-headline-links ()
  "Cache current page's gkroam headline links."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward gkroam-link-regexp nil t)
      (when (gkroam--link-has-headline)
        (gkroam-set-headline-id
         (match-string-no-properties 2)
         (match-string-no-properties 5))))))

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

;;;###autoload
(defun gkroam-rebuild-caches ()
  "Clear gkroam headline-id cache."
  (interactive)
  (db-hash-clear gkroam-page-db)
  (db-hash-clear gkroam-headline-db)
  (gkroam-cache-all-pages)
  (gkroam-cache-all-headline-links)
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
            (gkroam-update-linked-references title))))
    (message "Not in the gkroam directory!")))

;;;###autoload
(defun gkroam-new-at-point ()
  "Insert a file link and create a new file according to text at point."
  (interactive)
  (let* ((title (thing-at-point 'word t))
         (bounds (bounds-of-thing-at-point 'word))
         (beg (car bounds))
         (end (cdr bounds))
         (case-title (gkroam-title-exist-p title)))
    (if (gkroam-work-p)
        (if case-title
            (progn
              (delete-region beg end)
              (if (stringp case-title)
                  (gkroam-insert case-title title)
                (gkroam-insert title "")))
          (delete-region beg end)
          (gkroam-insert title "")
          (unless (gkroam-at-capture-buf)
            (gkroam-find title)))
      (if case-title
          (gkroam-find case-title)
        (gkroam-find title)))))

;;;###autoload
(defun gkroam-new-from-region (&optional beg end)
  "Insert a file link and create a new file according to a selected region.
If BEG and END are non-nil, the region is between beg and end."
  (interactive)
  (let* ((beg (or beg (region-beginning)))
         (end (or end (region-end)))
         (title (buffer-substring-no-properties beg end))
         (case-title (gkroam-title-exist-p title)))
    (if (gkroam-work-p)
        (if case-title
            (progn
              (delete-region beg end)
              (if (stringp case-title)
                  (gkroam-insert case-title title)
                (gkroam-insert title "")))
          (delete-region beg end)
          (gkroam-insert title "")
          (unless (gkroam-at-capture-buf)
            (gkroam-find title)))
      (if case-title
          (gkroam-find case-title)
        (gkroam-find title)))))

;;;###autoload
(defun gkroam-dwim ()
  "Smartly create a new file or insert a link.
If in a region, read the text in region as file title.
If a word at point, read the text at point as file title.
Otherwise, use gkroam-find.  Finally, insert a file link
at point or in region."
  (interactive)
  (cond
   ((region-active-p) (gkroam-new-from-region))
   ((thing-at-point 'word) (gkroam-new-at-point))
   (t (call-interactively #'gkroam-find))))

;; ----------------------------------------
;;;;; gkroam index

(defvar gkroam-index-keys
  '("TITLE" "WORD COUNT" "MENTIONS" "UPDATED" "CREATED")
  "Column key list of gkroam index buffer.")

(defun gkroam--get-max-column-length (key)
  "Get KEY column's max length in gkroam index buffer."
  (let* ((key-len (length key))
         (titles-p (gkroam-retrive-all-titles))
         (title-max-len
          (when titles-p (apply #'max (mapcar #'length titles-p)))))
    (pcase key
      ("TITLE"
       (if title-max-len
           (max title-max-len key-len)
         key-len))
      ("WORD COUNT" 10)
      ("MENTIONS" 8)
      ("UPDATED" 12)
      ("CREATED" 12))))

(defsubst gkroam--valign-space (xpos)
  "Return a display property that aligns to XPOS."
  `(space :align-to (,xpos)))

(defun gkroam--pixel-width-from-to (from to &optional with-prefix)
  "Return the width of the glyphs from FROM (inclusive) to TO (exclusive).

The buffer has to be in a live window.  FROM has to be less than
TO and they should be on the same line.

If WITH-PREFIX is non-nil, don’t subtract the width of line
prefix."
  (let* ((window (get-buffer-window))
         (line-prefix
          (let ((pos to))
            (while (get-char-property pos 'display)
              (cl-decf pos))
            (car (window-text-pixel-size window pos pos)))))
    (- (car (window-text-pixel-size window from to))
       (if with-prefix 0 line-prefix)
       (if (bound-and-true-p display-line-numbers-mode)
           (line-number-display-width 'pixel)
         0))))

(defun gkroam--put-overlay (beg end &rest props)
  "Put overlay between BEG and END.
PROPS contains properties and values."
  (let ((ov (make-overlay beg end nil t nil)))
    (while props
      (overlay-put ov (pop props) (pop props)))))

(defvar gkroam-index-key-space-num 5
  "The number of spaces inserted between 
different keys of index buffer.")

(defun gkroam-index-max-columns ()
  "Get the max columns of index buffer string."
  (let ((columns 0)
        (max-len-lst
         (mapcar (lambda (key)
                   (gkroam--get-max-column-length key))
                 gkroam-index-keys)))
    (mapc (lambda (num)
            (setq columns (+ columns (+ num gkroam-index-key-space-num))))
          max-len-lst)
    (if gkroam-prettify-page-p
        (+ columns gkroam-window-margin)
      columns)))

;;;###autoload
(defun gkroam-index ()
  "Show gkroam index buffer."
  (interactive)
  (with-current-buffer (get-buffer-create gkroam-index-buf)
    (view-buffer gkroam-index-buf)
    (let ((inhibit-read-only t)
          max-column-len-lst
          right-pixel)
      (erase-buffer)
      (insert (format "#+TITLE: %s\n\n" gkroam-index-title))
      (dotimes (i (length gkroam-index-keys))
        (let* ((key (nth i gkroam-index-keys))
               (max-column-len (gkroam--get-max-column-length key))
               (key-len (length key)))
          (push max-column-len max-column-len-lst)
          (insert key)
          (gkroam-overlay-region (- (point) key-len) (point)
                                 'face '(bold italic))
          (unless (= i (1- (length gkroam-index-keys)))
            (insert (make-string (+ gkroam-index-key-space-num
                                    (- max-column-len key-len))
                                 ?\s))
            (when (= i 0)
              (setq right-pixel
                    (gkroam--pixel-width-from-to
                     (line-beginning-position) (point)))))))
      (setq max-column-len-lst (reverse max-column-len-lst))
      (newline)
      (dolist (page (reverse (gkroam--all-pages)))
        (let* ((db gkroam-page-db)
               (title (gkroam-retrive-title page))
               (count (number-to-string (gkroam-db-get db title "count")))
               (mention (gkroam-db-get db title "mention"))
               (create (gkroam-db-get db title "create"))
               (update (gkroam-db-get db title "update"))
               (value-lst (list title count mention create update))
               overlay-beg overlay-end)
          (dotimes (i (length gkroam-index-keys))
            (let* ((max-column-len (nth i max-column-len-lst))
                   (val (nth i value-lst))
                   (val-len (length val)))
              (pcase i
                (0 (insert (format "{[%s]}" val))
                   (setq overlay-beg (point)))
                (1 (insert (nth 1 value-lst))
                   (setq overlay-end (- (point) val-len)))
                (2 (if (string= "0" val)
                       (progn
                         (insert val)
                         (gkroam-overlay-region (- (point) val-len) (point)
                                                'face 'shadow))
                     (insert-button val
                                    'action 'gkroam-show-mentions
                                    'follow-link t
                                    'face 'font-lock-string-face
                                    'help-echo "Click to show all mentions.")))
                (_ (insert (nth i value-lst))
                   (gkroam-overlay-region (- (point) val-len) (point)
                                          'face 'shadow)))
              (unless (= i (1- (length gkroam-index-keys)))
                (insert (make-string (+ gkroam-index-key-space-num
                                        (- max-column-len val-len))
                                     ?\s)))))
          (gkroam--put-overlay
           overlay-beg overlay-end
           'display (gkroam--valign-space right-pixel)))
        (newline)))
    (let ((gkroam-show-brackets-p nil))
      (gkroam-fontify-link))
    (gkroam-prettify-page)
    (setq truncate-lines t)
    (goto-char (point-min))))

;;;###autoload
(define-minor-mode gkroam-mentions-mode
  "Minor mode for special key bindings in a gkroam mentions buffer.
Turning on this mode runs the normal hook `gkroam-mentions-mode-hook'."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") #'gkroam-mentions-finalize)
            map)
  :require 'gkroam
  (if gkroam-prettify-page-p
      (let ((spaces (make-string gkroam-window-margin ?\s)))
        (setq-local
         header-line-format
         (substitute-command-keys
          (concat "\\<gkroam-mentions-mode-map>" spaces "All references mentioned this page, press `\\[gkroam-mentions-finalize]' to quit window."))))
    (setq-local
     header-line-format
     (substitute-command-keys
      "\\<gkroam-mentions-mode-map>All references mentioned this page, press `\\[gkroam-mentions-finalize]' to quit window.")))
  (setq truncate-lines nil))

(defvar gkroam-mentions-flag nil
  "Non-nil means it's in process of gkroam mentions.")

(defun gkroam-mentions-finalize ()
  "Quit gkroam mentions window and restore window configuration."
  (interactive)
  (set-window-configuration gkroam-return-wconf)
  (kill-buffer (or (get-buffer gkroam-mentions-buf)
                   (get-buffer gkroam-unlinked-buf)))
  (setq gkroam-return-wconf nil)
  (setq gkroam-mentions-flag nil))

(defun gkroam-show-mentions (btn)
  "Show gkroam page mentions in a side window after push button BTN."
  (let ((buf (get-buffer-create gkroam-mentions-buf))
        (mentions-num (button-label btn))
        (inhibit-read-only t)
        title references)
    (when (null gkroam-mentions-flag)
      (setq gkroam-return-wconf (current-window-configuration)))
    (setq gkroam-mentions-flag t)
    (select-window (get-buffer-window gkroam-index-buf))
    (setq title (button-get (button-at (line-beginning-position)) 'title))
    (setq references
          (with-temp-buffer
            (insert-file-contents (gkroam--get-file (gkroam-retrive-page title)))
            (goto-char (point-max))
            (re-search-backward gkroam-linked-reference-delimiter-re nil t)
            (buffer-substring (point) (point-max))))
    (delete-other-windows)
    (with-current-buffer buf
      (erase-buffer)
      (insert "#+TITLE: MENTIONS OF PAGE\n\n")
      (insert references)
      (org-mode)
      (gkroam-backlink-fontify (point-min) (point-max))
      (gkroam-prettify-page)
      (gkroam-list-parent-item-overlay (point-min))
      (gkroam-reference-region-overlay (point-min))
      (when gkroam-prettify-page-p
        (gkroam-org-list-fontify (point-min) (point-max)))
      (goto-char (point-min)))
    (split-window-right (gkroam-index-max-columns))
    (other-window 1)
    (switch-to-buffer buf)
    (gkroam-mentions-mode)
    (read-only-mode 1)
    mentions-num))

;; ----------------------------------------

;;;###autoload
(defun gkroam-update ()
  "Update current gkroam buffer's reference."
  (interactive)
  (if (gkroam-at-root-p)
      (let ((title (gkroam-retrive-title
                    (file-name-nondirectory (buffer-file-name)))))
        (gkroam-update-linked-references title))
    (message "Not in the gkroam directory!")))

;;;###autoload
(defun gkroam-delete (&optional title-lst)
  "Delete gkroam pages.
If optional argument TITLE-LST is non-nil,
delete those pages with title in TITLE-LST."
  (interactive)
  (let* ((crm-separator gkroam-crm-separator)
         (titles (or title-lst
                     (completing-read-multiple
                      "Choose one or multiple pages to delete (use '|' to separate): "
                      (gkroam-retrive-all-titles) nil t)))
         page file)
    (dolist (title titles)
      (setq page (or (gkroam-retrive-page title)
                     (error "\"%s\" page is not exist!" title)))
      (setq file (gkroam--get-file page))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (delete-file file t)
      (gkroam-rebuild-caches))))

;; ----------------------------------------
;;;;; minor mode: gkroam-link-mode

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
      (if (gkroam-at-capture-buf)
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
  (save-excursion
    (goto-char beg)
    (while (re-search-forward gkroam-hashtag-regexp end t)
      (gkroam--fontify-hashtag)
      (with-silent-modifications
        (make-text-button (match-beginning 0)
                          (match-end 0)
                          :type 'gkroam-link
                          'title (match-string-no-properties 3))))))

(defun gkroam--fontify-hide-brackets ()
  "Hide gkroam link brackets using text properties."
  (with-silent-modifications
    (if (gkroam--link-has-alias)
        (progn
          (add-text-properties (match-beginning 9) (match-beginning 10) `(face ,gkroam-link-face))
          (add-text-properties (match-beginning 0) (match-beginning 9) '(display ""))
          (add-text-properties (match-beginning 10) (match-end 0) '(display "")))
      (if (gkroam--link-has-headline)
          (progn
            (add-text-properties (match-beginning 2) (match-end 3) `(face ,gkroam-link-face))
            (add-text-properties (match-beginning 0) (match-beginning 5) '(display ""))
            (add-text-properties (match-end 3) (match-end 0) '(display "")))
        (add-text-properties (match-beginning 2) (match-end 3)  `(face ,gkroam-link-face))
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
          (add-text-properties (match-beginning 2) (match-beginning 6) `(face ,gkroam-link-face))
          (add-text-properties (match-beginning 9) (match-beginning 10) `(face ,gkroam-link-face)))
      (if (gkroam--link-has-headline)
          (progn
            (remove-text-properties (match-beginning 0) (match-beginning 5) '(display nil))
            (remove-text-properties (match-end 3) (match-end 0)'(display nil))
            (add-text-properties (match-beginning 0) (match-beginning 2) '(face shadow))
            (add-text-properties (match-end 3) (match-end 0)'(face shadow))
            (add-text-properties (match-beginning 4) (match-end 4) '(face shadow))
            (add-text-properties (match-beginning 2) (match-end 3) `(face ,gkroam-link-face)))
        (remove-text-properties (match-beginning 0) (match-beginning 2) '(display nil))
        (remove-text-properties (match-end 3) (match-end 0) '(display nil))
        (add-text-properties (match-beginning 0) (match-beginning 2) '(face shadow))
        (add-text-properties (match-end 3) (match-end 0) '(face shadow))
        (add-text-properties (match-beginning 2) (match-end 3) `(face ,gkroam-link-face))))))

(defun gkroam-link-fontify (beg end)
  "Put gkroam link between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward gkroam-link-regexp end t)
      (let* ((title (match-string-no-properties 2))
             (headline (when (gkroam--link-has-headline)
                         (match-string-no-properties 5)))
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
                            'help-echo echo))))))

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

;;;;; gkroam backlink

;; (defvar gkroam-backlink-regexp
;;   "{{\\(.+?\\)\\(::\\([0-9]+\\)\\)?}{\\(.+?\\)}}"
;;   "Regular expression that matches a gkroam backlink.")

(defvar gkroam-backlink-regexp
  "{{\\(.+?::\\)?\\([0-9]+\\)}{\\(.+?\\)}}"
  "Regular expression that matches a gkroam backlink.")

(defvar gkroam-backlink-full-regexp
  "{{\\(.+?\\)::\\([0-9]+\\)}{\\(.+?\\)}}"
  "Regular expression that matches a gkroam backlink.")

(defun gkroam--format-backlink (page line-number alias)
  "Format gkroam backlink for PAGE, refer to a link
in LINE-NUMBER line, display a description ALIAS."
  (if page
      (format "{{%s::%d}{%s}}" page line-number alias)
    (format "{{%d}{%s}}" line-number alias)))

(define-button-type 'gkroam-backlink
  'action #'gkroam-follow-backlink
  'face '(:underline nil)
  'title nil
  'page nil
  'line-number nil
  'follow-link t
  'help-echo "Jump back to page")

(defun gkroam--goto-specific-line (title line-number)
  "Goto a specific LINE-NUMBER line in TITLE page."
  (gkroam-find title)
  (forward-line (- line-number (line-number-at-pos) 1))
  (recenter-top-bottom)
  (gkroam-fontify-link))

(defun gkroam-follow-backlink (button)
  "Jump to the page that BUTTON represents."
  (with-demoted-errors "Error when following the link: %s"
    (let* ((page (button-get button 'page))
           (title (gkroam-retrive-title page))
           (line-number (string-to-number (button-get button 'line-number))))
      (pcase nil
        ((guard (gkroam-at-mentions-buf))
         (if page
             (progn
               (other-window 1)
               (gkroam--goto-specific-line title line-number))
           (setq page
                 (save-excursion
                   (when (re-search-backward gkroam-backlink-full-regexp nil t)
                     (match-string-no-properties 1))))
           (setq title (gkroam-retrive-title page))
           (other-window 1)
           (gkroam--goto-specific-line title line-number)))
        ((guard (gkroam-at-unlinked-buf))
         (if page
             (progn
               (other-window 1)
               (gkroam--goto-specific-line title line-number))
           (setq title (button-get button 'title))
           (gkroam-link-unlinked title line-number)))
        ((guard (gkroam-at-root-p))
         (if page
             (gkroam--goto-specific-line title line-number)
           (setq page (save-excursion
                        (when (re-search-backward gkroam-backlink-full-regexp nil t)
                          (match-string-no-properties 1))))
           (setq title (gkroam-retrive-title page))
           (gkroam--goto-specific-line title line-number)))))))

(defun gkroam-backlink-fontify (beg end)
  "Highlight gkroam backlink between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward gkroam-backlink-regexp end t)
      (let ((page
             (when (match-string-no-properties 1)
               (substring (match-string-no-properties 1) 0 -2)))
            (line-number (match-string-no-properties 2)))
        (with-silent-modifications
          (add-text-properties (match-beginning 0) (match-beginning 3)
                               '(display ""))
          (add-text-properties (match-end 3) (match-end 0)
                               '(display ""))
          (add-text-properties (match-beginning 3) (match-end 3)
                               '(face link))
          (make-text-button (match-beginning 3)
                            (match-end 3)
                            :type 'gkroam-backlink
                            'page page
                            'line-number line-number))))))

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

;;;;; page beautify

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
  "^\\(\\*\\{2,3\\} .+\\)?\n +\\(.+\\( > .+\\)*\\)\n \\{3,4\\}\\([0-9]+[).]\\|[*+-]\\) \\(\\[[ X-]\\] \\)?"
  "Regular expression that matches org plain list parent items in references.")

(defun gkroam-list-parent-item-overlay (beg)
  "Shadow plain list's parent items in references between BEG and `point-max'."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward gkroam-list-parent-item-re nil t)
      (with-silent-modifications
        (gkroam-overlay-region (match-beginning 2) (match-end 2)
                               'face 'shadow)))))

(defun gkroam-reference-region-overlay (beg)
  "Highlight all reference regions between BEG and `point-max'."
  (let ((end beg))
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
                (setq content-end (org-element-property :contents-end elem))
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

(defun gkroam-preserve-window-margin ()
  "Preserve gkroam pages' window margin."
  (save-selected-window
    (dolist (win (window-list))
      (select-window win)
      (when (gkroam-work-p)
        (gkroam-set-window-margin)))))

(defun gkroam-prettify-page ()
  "Prettify gkroam page."
  (when (and gkroam-mode (gkroam-work-p))
    (if gkroam-prettify-page-p
        (gkroam-prettify-mode 1)
      (gkroam-prettify-mode -1))
    (gkroam-set-window-margin)))

(defun gkroam-fontify-link ()
  "Highlight gkroam links, hashtags and backlinks."
  (when (and gkroam-mode (gkroam-work-p))
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
        (unless (gkroam-at-index-buf)
          (gkroam-fontify-link))))))

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

;; ----------------------------------------
;;;;; minor mode: gkroam-capture-mode

(defun gkroam--get-page-dwim ()
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
      (let ((case-title (gkroam-title-exist-p title)))
        (if (or page case-title)
            (cons (or page (gkroam-retrive-page case-title)) 'page)
          (cons title 'title))))))

(defun gkroam--get-content-region ()
  "Get the region of real contents.
The region is a begin position and end position cons."
  (let (content-beg content-end)
    (goto-char (point-min))
    (while (re-search-forward "^ *#\\+.+?:.*" nil t))
    (setq content-beg (1+ (match-end 0)))
    (if (re-search-forward gkroam-linked-reference-delimiter-re nil t)
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
  (let ((title-or-page (car (gkroam--get-page-dwim)))
        (type (cdr (gkroam--get-page-dwim)))
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
  (insert (format "* %s\n%s" title content))
  (gkroam-prettify-page))

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
            (org-mode)
            (gkroam-capture-append title content)
            (gkroam-capture-mode)
            (setq gkroam-capture-flag t))
        (select-window (get-buffer-window gkroam-capture-buf))
        (gkroam-capture-append title content)
        (gkroam-capture-mode)))))

;; ----------------------------------------
;;;;; gkroam mode

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
      (list beg end (gkroam-retrive-all-titles) . nil))
     ((gkroam-company-hashtag-p)
      (setq bds (bounds-of-thing-at-point 'symbol))
      (setq beg (car bds))
      (setq end (cdr bds))
      (list beg end (gkroam-retrive-all-titles) . nil)))))

(defun gkroam-ivy-use-selectable-prompt (boolean)
  "Set `ivy-use-selectable-prompt' to BOOLEAN."
  (when (require 'ivy nil t)
    (when (and (bound-and-true-p ivy-mode)
               (boundp ivy-use-selectable-prompt))
      (setq ivy-use-selectable-prompt boolean))))

(defun gkroam-selectrum-mode-p ()
  "Judge if selectrum is installed and selectrum-mode is turned on."
  (when (require 'selectrum nil t)
    (bound-and-true-p selectrum-mode)))

;;;###autoload
(define-minor-mode gkroam-mode
  "Minor mode for gkroam."
  :lighter " Gkroam"
  :keymap (let ((map (make-sparse-keymap))) map)
  :require 'gkroam
  :global t
  (if gkroam-mode
      (progn
        (add-hook 'after-save-hook #'gkroam-update-page-cache)
        (add-hook 'completion-at-point-functions #'gkroam-completion-at-point nil 'local)
        (add-hook 'company-completion-finished-hook #'gkroam-completion-finish nil 'local)
        (add-hook 'window-configuration-change-hook #'gkroam-preserve-window-margin)
        (add-hook 'org-mode-hook #'gkroam-link-mode)
        (add-hook 'org-mode-hook (lambda ()
                                   (when (gkroam-work-p)
                                     (gkroam-link-frame-setup 'find-file)
                                     (gkroam-ivy-use-selectable-prompt t)
                                     (setq truncate-lines nil)
                                     (setq org-startup-folded nil)
                                     (setq org-return-follows-link t)))))
    ;; how to preserve the original variable value?
    (remove-hook 'after-save-hook #'gkroam-update-page-cache)
    (remove-hook 'completion-at-point-functions #'gkroam-completion-at-point 'local)
    (remove-hook 'company-completion-finished-hook #'gkroam-completion-finish 'local)
    (remove-hook 'window-configuration-change-hook #'gkroam-preserve-window-margin)
    (remove-hook 'org-mode-hook #'gkroam-link-mode)
    (remove-hook 'org-mode-hook
                 (lambda ()
                   (when (gkroam-work-p)
                     (gkroam-link-frame-setup 'find-file)
                     (gkroam-ivy-use-selectable-prompt t)
                     (setq truncate-lines nil)
                     (setq org-startup-folded nil)
                     (setq org-return-follows-link t))))
    (gkroam-prettify-mode -1)
    (gkroam-link-mode -1)
    (set-window-margins (selected-window) 0 0)
    (with-silent-modifications
      (set-text-properties (point-min) (point-max) nil))))

(provide 'gkroam)
;;; gkroam.el ends here
