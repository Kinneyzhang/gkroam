;;; gkroam.el --- A lightweight org-mode roam replica  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 2.3.6
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

;; v2.0.2 - Fix `gkroam-publish-current-file' and `gkroam-preview-current',
;; automatically convert gkroam link to org link and convert it back after
;; published (use 'undo', not reliable). But it has problem with publishing
;; the whole project.

;; v2.0.3 - Fix `gkroam-publish-site' and `gkroam-preview'.
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
  :type 'string
  :group 'gkroam)

(defcustom gkroam-index-title "GKROAM"
  "Title of index page."
  :type 'string
  :group 'gkroam)

(defvar gkroam-cache-dir (concat user-emacs-directory "gkroam/")
  "Gkroam's cache directory.")

(defvar gkroam-db
  (db-make
   `(db-hash
     :filename ,(concat gkroam-cache-dir "gkroam-db")))
  "Gkroam's cache database.")

(defvar gkroam-window-margin 2
  "Gkroam window's left and right margin.")

(defvar gkroam-use-default-filename nil
  "Non-nil means use default filename for gkroam page.
The default format is '%Y%m%d%H%M%S' time string.")

(defvar gkroam-dynamic-p nil
  "Non-nil means show gkroam brackets, bullets and title dynamically.")

(defvar gkroam-show-brackets-p nil
  "Non-nil means to show brackets in page link.")

(defvar gkroam-prettify-p nil
  "Non-nil means to prettify gkroam page.")

(defvar gkroam-title-height 300
  "Height of gkroam page title when prettifying.")

(defvar gkroam-org-list-re
  "^ *\\([0-9]+[).]\\|[*+-]\\) \\(\\[[ X-]\\] \\)?"
  "Org list bullet and checkbox regexp.")

(defvar gkroam-pages nil
  "Page candidates for completion.")

(defvar gkroam-mode-map (make-sparse-keymap)
  "Keymap for `gkroam-mode'.")

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
           (group (?? (seq (group " » ")
                           (group (+? not-newline)))))
           (group "]")
           (group (?? (seq (group "[")
                           (group (+? not-newline))
                           (group "]"))))
           (group "}")))
  "Regular expression that matches a gkroam hashtag.")

(defvar gkroam-return-wconf nil
  "Saved window configuration before goto gkroam capture.")

(defvar gkroam-capture-flag nil
  "Non-nil means it's in process of gkroam capture.")

(defvar gkroam-capture-buf "*gkroam-capture*"
  "Gkroam capture buffer name.")

(defvar gkroam-capture-pages nil
  "Pages that have been capturing in gkroam capture buffer.
The value is a list of page's title.")

;;;; Functions
(defun gkroam-link-frame-setup ()
  "Alter `org-link-frame-setup' for gkroam."
  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . gkroam--find-file)
          (wl . wl-other-frame))))

(defun gkroam--find-file (filename)
  "Find file function for `org-link-frame-setup'.
With argument FILENAME."
  (find-file filename)
  (gkroam-mode))

(defun gkroam-at-root-p ()
  "Check if current file exists in `gkroam-root-dir'.
If BUFFER is non-nil, check the buffer visited file."
  (when (buffer-file-name)
    (file-equal-p (file-name-directory (buffer-file-name))
                  (expand-file-name gkroam-root-dir))))

(defun gkroam--get-title (page)
  "Get PAGE's title."
  (with-temp-buffer
    (insert-file-contents (gkroam--get-file page) nil 0 2000 t)
    (goto-char (point-min))
    (re-search-forward "^ *#\\+TITLE:" nil t)
    (string-trim (buffer-substring (match-end 0) (line-end-position)))))

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

(defun gkroam--get-file (page)
  "Get gkroam file accroding to PAGE."
  (expand-file-name page gkroam-root-dir))

(defun gkroam--all-pages ()
  "Get all gkroam pages."
  (directory-files gkroam-root-dir nil "^[^.#].+\\.org$"))

(defun gkroam--all-titles ()
  "Get all gkroam titles."
  (let* ((pages (gkroam--all-pages)))
    (mapcar (lambda (page) (gkroam--get-title page)) pages)))

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

(defun gkroam--format-backlink (page)
  "Format gkroam backlink in PAGE."
  (let* ((title (gkroam--get-title page)))
    (format "[[file:%s][%s ➦]]" page title)))

;; ----------------------------------------

(defvar gkroam-link-re-format "{\\[%s.*?\\]}"
  "Gkroam link regexp format used for searching link context.")

(defun gkroam--search-process (page linum)
  "Return a rg process to search PAGE's link and output LINUM lines before and after matched string."
  (let ((title (gkroam--get-title page))
        (name (generate-new-buffer-name " *gkroam-rg*")))
    (start-process name name "rg" "--ignore-case" "--sortr" "path"
                   (format "\\{\\[%s.*?\\](\\[.+?\\])*\\}" title)
                   "-C" (number-to-string linum)
                   "-N" "--heading"
                   (expand-file-name gkroam-root-dir) ;; must be absolute path.
                   "-g" "!index.org*")))

(defun gkroam--process-link-in-references (string)
  "Remove links in reference's STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward gkroam-link-regexp nil t)
      (if (string= (ignore-errors (char-to-string (char-before (match-beginning 0)))) "#")
          (replace-match (concat "*" (match-string-no-properties 2) "*"))
        (if (gkroam--link-has-alias)
            (replace-match (concat "*" (match-string-no-properties 9) "*"))
          (if (gkroam--link-has-headline)
              (replace-match (concat "*" (match-string-no-properties 2)
                                     " » " (match-string-no-properties 5) "*"))
            (replace-match (concat "*" (match-string-no-properties 2) "*"))))))
    (buffer-string)))

(defun gkroam-get-reference-content ()
  "Get the content of linked reference."
  (save-excursion
    (goto-char (line-beginning-position))
    (skip-chars-forward "[ ]")
    (let* ((elem (org-element-at-point))
           (elem-type (org-element-type elem))
           elem-start elem-end)
      (if (string= elem-type "item")
          (let* ((level-1-blank-num
                  (cadr (car (org-element-property :structure elem))))
                 (structure
                  (cl-find-if
                   (lambda (lst)
                     (and (= (cadr lst) level-1-blank-num)
                          (< (point) (car (last lst)))))
                   (org-element-property :structure (org-element-at-point)))))
            (setq elem-start (car structure))
            (setq elem-end (car (last structure))))
        (setq elem-start (org-element-property :begin elem))
        (setq elem-end (org-element-property :end elem)))
      (buffer-substring-no-properties elem-start elem-end))))

(defun gkroam-process-searched-string (string title)
  "Process searched STRING by 'rg', get page LINUM*2+1 lines of TITLE and context."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((gkroam-file-re (expand-file-name ".+\\.org" gkroam-root-dir))
          (beg (point-min)) (end (point)) (num 0) references)
      (while (not (= end (point-max)))
        (save-excursion
          (goto-char beg)
          (if (re-search-forward gkroam-file-re nil t 2)
              (progn
                (setq end (line-beginning-position)))
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
              (let ((headline "")
                    (content (string-trim
                              (gkroam-get-reference-content) nil "[ \t\n\r]+")))
                (setq num (1+ num))
                (save-excursion
                  (when (re-search-backward "^*+ .+\n" nil t)
                    (setq headline (concat "**" (match-string-no-properties 0)))))
                (if (string= headline last-headline)
                    (setq context (concat context content "\n\n"))
                  (setq context (concat context headline content "\n\n")))
                (setq last-headline headline)))
            (setq context (gkroam--process-link-in-references context))
            (setq references
                  (concat references
                          (format "** %s\n%s" (gkroam--format-backlink page) context)))
            (setq beg end))))
      (cons num references))))

(defun gkroam--search-linked-pages (process callback)
  "Call CALLBACK After the PROCESS finished."
  (let (sentinel)
    (setq sentinel
          (lambda (process event)
            (if (string-match-p (rx (or "finished" "exited"))
                                event)
                (if-let ((buf (process-buffer process)))
                    (with-current-buffer buf
                      (funcall callback (buffer-string)))
                  (error "Gkroam’s rg process’ buffer is killed"))
              (error "Gkroam’s rg process failed with signal: %s"
                     event))))
    (set-process-sentinel process sentinel)))

(defun gkroam-update-reference (page)
  "Update gkroam PAGE's reference."
  (unless (executable-find "rg")
    (user-error "Cannot find program rg"))
  (let ((linum 9999))
    (gkroam--search-linked-pages
     (gkroam--search-process page linum)
     (lambda (string)
       (let* ((title (gkroam--get-title page))
              (file (gkroam--get-file page))
              (file-buf (find-file-noselect file t)))
         (with-current-buffer file-buf
           (save-excursion
             (goto-char (point-max))
             (re-search-backward "^* [0-9]+ Linked References\n" nil t)
             (delete-region (point) (point-max))
             (unless (string= string "")
               (let* ((processed-str (gkroam-process-searched-string string title))
                      (num (car processed-str))
                      (references (cdr processed-str)))
                 (insert (format "* %d Linked References\n" num))
                 (insert references)
                 ;; use overlay to hide part of reference. (filter)
                 ;; make ".. Linked References" uneditable
                 ;; put overlay onto each reference and click to jump page
                 ;; how to realize accurate jumping?
                 ;; change mouse style when on hover and display help-echo
                 ;; (gkroam-overlay-region beg (point-max) 'invisible t)
                 )
               (save-buffer))))))))
  (message "%s reference updated" page))

(defun gkroam-new (title)
  "Just create a new gkroam page titled with TITLE."
  (let* ((file (gkroam--gen-file)))
    (with-current-buffer (find-file-noselect file t)
      (insert (format "#+TITLE: %s\n\n" title))
      (save-buffer))
    (push title gkroam-pages)
    file))

;; ----------------------------------------
;; headline linked references

(defun gkroam--get-headlines (title)
  "Get page title with TITLE headlines."
  (mapcar #'car (db-get title gkroam-db)))

(defun gkroam-goto-headline (id)
  "Goto headline with id ID."
  (org-id-goto id)
  (gkroam-update)
  (gkroam-prettify-page)
  (gkroam-overlay-link (point-min)))

(defun gkroam-heading-id-pairs ()
  "Return all heading and id pairs of current page."
  (let (end)
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (if (re-search-forward "^* [0-9]+ Linked References$" nil t)
            (setq end (1- (line-beginning-position)))
          (setq end (point-max)))
        (narrow-to-region (point-min) end)
        (org-map-entries
         (lambda ()
           (cons (org-entry-get (point) "ITEM") (org-id-get-create))))))))

;;;###autoload
(defun gkroam-build-page-cache ()
  "Build current page's (heading . id) cache."
  (let (title)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^ *#\\+TITLE:" nil t)
      (setq title (string-trim (buffer-substring-no-properties
                                (match-end 0) (line-end-position)))))
    (db-put title (gkroam-heading-id-pairs) gkroam-db)))

;;;###autoload
(defun gkroam-build-caches ()
  "Build all pages' caches manually."
  (interactive)
  (let (file)
    (dolist (page (gkroam--all-pages))
      (setq file (gkroam--get-file page))
      (with-current-buffer (find-file-noselect file)
        (gkroam-build-page-cache)
        (save-buffer)))))

;; ----------------------------------------

;;;; Commands
;;;###autoload
(defun gkroam-find (&optional title)
  "Create a new gkroam page or open an exist one in current window, titled with TITLE."
  (interactive)
  (let* ((title (or title (completing-read "New title or open an exist one: "
                                           (gkroam--all-titles) nil nil)))
         (page (gkroam--get-page title)))
    (if page
        (find-file (gkroam--get-file page))
      (find-file (gkroam-new title)))
    (gkroam-mode)
    (gkroam-update)))

;;;###autoload
(defun gkroam-daily ()
  "Create or open gkroam daily notes."
  (interactive)
  (let* ((title (format-time-string "%b %d, %Y")))
    (gkroam-find title)))

;;;###autoload
(defun gkroam-insert (&optional title headline alias)
  "Insert a gkroam page link at point.
With optional arguments, use TITLE or HEADLINE or ALIAS to format link."
  (interactive)
  (if (gkroam-at-root-p)
      (let* ((title (or title (completing-read
                               "Choose a page or create a new: "
                               (gkroam--all-titles) nil nil)))
             (headlines (gkroam--get-headlines title))
             (headline
              (or headline
                  (when headlines (completing-read
                                   "Choose a headline, directly press \"C-p RET\" or \"RET\" to skip: "
                                   headlines nil nil))))
             (alias (or alias
                        (completing-read
                         "Give an alias, directly press \"RET\" to skip: "
                         nil nil nil))))
        (if (string= headline "")
            (setq headline nil))
        (if (string= alias "")
            (setq alias nil))
        (insert (gkroam--format-link title headline alias))
        (save-buffer))
    (message "Not in the gkroam directory!")))

;;;###autoload
(defun gkroam-new-at-point ()
  "Insert a file link and create a new file according to text at point."
  (interactive)
  (if (gkroam-at-root-p)
      (let* ((title (thing-at-point 'word t))
             (bounds (bounds-of-thing-at-point 'word))
             (page-exist-p (gkroam--get-page title)))
        (if page-exist-p
            (progn
              (delete-region (car bounds) (cdr bounds))
              (gkroam-insert title))
          (gkroam-new title)
          (delete-region (car bounds) (cdr bounds))
          (gkroam-insert title nil)
          (gkroam-find title)))
    (message "Not in the gkroam directory!")))

;;;###autoload
(defun gkroam-new-from-region ()
  "Insert a file link and create a new file according to a selected region."
  (interactive)
  (if (and (gkroam-at-root-p) (region-active-p))
      (let* ((beg (region-beginning))
             (end (region-end))
             (title (buffer-substring-no-properties beg end))
             (page-exist-p (gkroam--get-page title)))
        (if page-exist-p
            (progn
              (delete-region beg end)
              (gkroam-insert title))
          (gkroam-new title)
          (delete-region beg end)
          (gkroam-insert title)
          (gkroam-find title)))
    (message "Not in the gkroam directory!")))

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
  (let* ((index-buf "*Gkroam Index*"))
    (with-current-buffer (get-buffer-create index-buf)
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "#+TITLE: %s\n\n" gkroam-index-title))
      (dolist (page (gkroam--all-pages))
        (insert (format "+ [[file:%s][%s]]\n" (gkroam--get-file page) (gkroam--get-title page)))))
    (switch-to-buffer index-buf)
    (read-only-mode 1)
    (gkroam-mode)))

;;;###autoload
(defun gkroam-update ()
  "Update current gkroam buffer's reference."
  (interactive)
  (if (gkroam-at-root-p)
      (gkroam-update-reference (file-name-nondirectory (buffer-file-name)))
    (message "Not in the gkroam directory!")))

;;;###autoload
(defun gkroam-update-all ()
  "Update all gkroam files' reference."
  (interactive)
  (let ((pages (gkroam--all-pages)))
    (mapcar #'gkroam-update-reference pages)))

;;; ----------------------------------------
;; minor mode: gkroam-link-mode

(define-button-type 'gkroam-link
  'action #'gkroam-follow-link
  'face '(:underline nil)
  'title nil
  'headline nil
  'alias nil
  'follow-link t
  'help-echo "Jump to page")

(defun gkroam-follow-link (button)
  "Jump to the page that BUTTON represents."
  (with-demoted-errors "Error when following the link: %s"
    (let* ((title (button-get button 'title))
           (headline (button-get button 'headline))
           (headline-id (cdr (assoc headline (db-get title gkroam-db)))))
      (if (string= (buffer-name) gkroam-capture-buf)
          (progn
            (other-window 1)
            (if headline
                (gkroam-goto-headline headline-id)
              (gkroam-find title)))
        (if headline
            (gkroam-goto-headline headline-id)
          (gkroam-find title))))))

(defun gkroam-link-fontify (beg end)
  "Put gkroam link between BEG and END."
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
      (make-text-button (match-beginning 0)
                        (match-end 0)
                        :type 'gkroam-link
                        'title title
                        'headline headline
                        'alias alias
                        'help-echo echo))))

(defun gkroam-hashtag-fontify(beg end)
  "Put gkroam link between BEG and END."
  (goto-char beg)
  (while (re-search-forward gkroam-hashtag-regexp end t)
    (make-text-button (match-beginning 0)
                      (match-end 0)
                      :type 'gkroam-link
                      'title (match-string-no-properties 3))))

(define-minor-mode gkroam-link-mode
  "Recognize gkroam link."
  t "" nil
  (if gkroam-link-mode
      (progn
        (jit-lock-register #'gkroam-hashtag-fontify)
        (jit-lock-register #'gkroam-link-fontify))
    (jit-lock-unregister #'gkroam-hashtag-fontify)
    (jit-lock-unregister #'gkroam-link-fontify))
  (jit-lock-refontify))

;; gkroam overlays

(defun gkroam-overlay-region (beg end prop value)
  "Put overlays in region started by BEG and ended with END.
The overlays has a PROP and VALUE."
  (overlay-put (make-overlay beg end) prop value))

(defun gkroam-overlay-hashtag ()
  "Set overlays to gkroam hashtag."
  (with-silent-modifications
    (gkroam-overlay-region (1- (match-beginning 0)) (match-end 0) 'face 'shadow)
    (gkroam-overlay-region (match-beginning 0) (match-beginning 2) 'display "")
    (gkroam-overlay-region (match-end 3) (match-end 0) 'display "")))

(defun gkroam--link-has-headline ()
  "Judge if a gkroam link has headline after `re-search-forward'."
  (not (string-empty-p (match-string-no-properties 3))))

(defun gkroam--link-has-alias ()
  "Judge if a gkroam link has alias after `re-search-forward'."
  (not (string-empty-p (match-string-no-properties 7))))

(defun gkroam-overlay-brackets ()
  "Set overlays to gkroam brackets link."
  (with-silent-modifications
    (if (gkroam--link-has-alias)
        (progn
          (gkroam-overlay-region (match-beginning 9) (match-beginning 10) 'face 'warning)
          (gkroam-overlay-region (match-beginning 0) (match-beginning 9) 'display "")
          (gkroam-overlay-region (match-beginning 10) (match-end 0) 'display ""))
      (if (gkroam--link-has-headline)
          (progn
            (gkroam-overlay-region (match-beginning 2) (match-end 3) 'face 'warning)
            (gkroam-overlay-region (match-beginning 0) (match-beginning 5) 'display "")
            (gkroam-overlay-region (match-end 3) (match-end 0) 'display ""))
        (gkroam-overlay-region (match-beginning 2) (match-end 3) 'face 'warning)
        (gkroam-overlay-region (match-beginning 0) (match-beginning 2) 'display "")
        (gkroam-overlay-region (match-end 3) (match-end 0) 'display "")))))

(defun gkroam-show-entire-link ()
  "Show entire page link."
  (with-silent-modifications
    (if (gkroam--link-has-alias)
        (progn
          (gkroam-overlay-region (match-beginning 0) (match-beginning 2) 'face 'shadow)
          (gkroam-overlay-region (match-beginning 6) (match-beginning 9) 'face 'shadow)
          (gkroam-overlay-region (match-beginning 10) (match-end 0) 'face 'shadow)
          (gkroam-overlay-region (match-beginning 2) (match-beginning 6) 'face 'warning)
          (gkroam-overlay-region (match-beginning 9) (match-beginning 10) 'face 'warning)
          (remove-overlays (match-beginning 0) (match-beginning 9) 'display "")
          (remove-overlays (match-beginning 10) (match-end 0) 'display ""))
      (if (gkroam--link-has-headline)
          (progn
            (gkroam-overlay-region (match-beginning 0) (match-beginning 2) 'face 'shadow)
            (gkroam-overlay-region (match-end 3) (match-end 0) 'face 'shadow)
            (gkroam-overlay-region (match-beginning 2) (match-end 3) 'face 'warning)
            (remove-overlays (match-beginning 0) (match-beginning 5) 'display "")
            (remove-overlays (match-end 3) (match-end 0) 'display ""))
        (gkroam-overlay-region (match-beginning 0) (match-beginning 2) 'face 'shadow)
        (gkroam-overlay-region (match-end 3) (match-end 0) 'face 'shadow)
        (gkroam-overlay-region (match-beginning 2) (match-end 3) 'face 'warning)
        (remove-overlays (match-beginning 0) (match-beginning 2) 'display "")
        (remove-overlays (match-end 3) (match-end 0) 'display "")))))

(defun gkroam-overlay-link (beg &optional bound)
  "Put overlays to links between BEG and BOUND."
  (when (eq major-mode 'gkroam-mode)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward gkroam-link-regexp bound t)
        (if (string= (char-to-string (char-before (match-beginning 0))) "#")
            (gkroam-overlay-hashtag)
          (if gkroam-show-brackets-p
              (gkroam-show-entire-link)
            (gkroam-overlay-brackets)))))))

;;;###autoload
(defun gkroam-link-edit ()
  "Edit gkroam link alias when 'dynamic edit' is off."
  (interactive)
  (if-let ((btn (button-at (point))))
      (let* ((btn-label (button-label btn))
             (btn-start (button-start btn))
             (btn-end (button-end btn))
             (new-link (completing-read "Edit link: " nil nil nil btn-label nil btn-label)))
        (delete-region btn-start btn-end)
        (insert new-link))
    (message "no link at point")))

;; page beautify

(defun gkroam--fontify-org-checkbox (notation)
  "Fontify org checkbox with NOTATION."
  (add-text-properties
   (match-beginning 2) (1- (match-end 2)) `(display ,notation)))

(defun gkroam--fontify-org-list ()
  "Fontify org list, including bullet and checkbox."
  (with-silent-modifications
    (add-text-properties
     (match-beginning 1) (match-end 1)
     '(display "•"))
    (when (match-beginning 2)
      (pcase (match-string-no-properties 2)
        ("[-] " (gkroam--fontify-org-checkbox "☐"))
        ("[ ] " (gkroam--fontify-org-checkbox "☐"))
        ("[X] " (gkroam--fontify-org-checkbox "☑"))))))

(defun gkroam--org-list-fontify (beg end)
  "Fontify org list bullet between BEG and END."
  (goto-char beg)
  (while (re-search-forward gkroam-org-list-re end t)
    (if (string= (match-string-no-properties 1) "*")
        (unless (= (match-beginning 0) (match-beginning 1))
          (gkroam--fontify-org-list))
      (gkroam--fontify-org-list))))

(defun gkroam-prettify-org-symbols ()
  "Prettify org list."
  (if gkroam-prettify-p
      (progn
        (jit-lock-register #'gkroam--org-list-fontify))
    (jit-lock-unregister #'gkroam--org-list-fontify)
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max)
                              '(display nil))))
  (jit-lock-refontify))

(defun gkroam-org-title-overlay (beg &optional bound)
  "Overlay org title, search between BEG and BOUND."
  (save-excursion
    (goto-char beg)
    (when (re-search-forward "\\(^ *#\\+TITLE: *\\)\\(.+\\)$" bound t)
      (if gkroam-prettify-p
          (progn
            (gkroam-overlay-region (match-beginning 1) (match-end 1)
                                   'display "")
            (gkroam-overlay-region (match-beginning 2) (match-end 2)
                                   'face `(:height ,gkroam-title-height)))
        (remove-overlays (line-beginning-position) (line-end-position))))))

(defun gkroam-set-window-margin ()
  "Set gkroam window margin when `gkroam-prettify-mode' is on."
  (let ((windows (window-list)))
    (save-selected-window
      (dolist (window windows)
        (select-window window)
        (when (eq major-mode 'gkroam-mode)
          (if gkroam-prettify-p
              (set-window-margins window
                                  gkroam-window-margin
                                  gkroam-window-margin)
            (set-window-margins window 0 0)))))))

(defun gkroam-prettify-page ()
  "Prettify gkroam page."
  (gkroam-set-window-margin)
  (save-excursion
    (gkroam-org-title-overlay (point-min))
    (gkroam-prettify-org-symbols)))

;; show page dynamically

(defun gkroam-restore-line-overlays ()
  "Restore overlays of bullets when the cursor move out of a line."
  (gkroam-org-title-overlay (line-beginning-position) (line-end-position))
  (gkroam-overlay-link (line-beginning-position) (line-end-position)))

(defun gkroam-remove-line-overlays ()
  "Remove overlays of bullets when the cursor move onto a line."
  (remove-overlays (line-beginning-position) (line-end-position)))

(defun gkroam-dynamic-elements ()
  "Make elements in gkroam page hide and show dynamically."
  (if gkroam-dynamic-p
      (progn
        (add-hook 'pre-command-hook #'gkroam-restore-line-overlays nil t)
        (add-hook 'post-command-hook #'gkroam-remove-line-overlays nil t))
    (remove-hook 'pre-command-hook #'gkroam-restore-line-overlays t)
    (remove-hook 'post-command-hook #'gkroam-remove-line-overlays t)))

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
  (gkroam-overlay-link (point-min)))

;;;###autoload
(defun gkroam-toggle-prettify ()
  "Toggle gkroam page prettification."
  (interactive)
  (if gkroam-prettify-p
      (progn
        (setq gkroam-prettify-p nil)
        (message "Page prettification is turned off"))
    (setq gkroam-prettify-p t)
    (message "Page prettification is turned on"))
  (gkroam-prettify-page))

;;;###autoload
(defun gkroam-toggle-dynamic ()
  "Determine whether to show elements dynamically."
  (interactive)
  (if gkroam-dynamic-p
      (progn
        (setq gkroam-dynamic-p nil)
        (message "Gkroam dynamic mode is off"))
    (setq gkroam-dynamic-p t)
    (message "Gkroam dynamic is on"))
  (gkroam-dynamic-elements))

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
      (if (or page (gkroam--get-page title))
          (cons (or page (gkroam--get-page title)) 'page)
        (cons title 'title)))))

(defun gkroam--get-content-region ()
  "Get the region of real contents.
The region is a begin position and end position cons."
  (let (beg end)
    (goto-char (point-min))
    (while (re-search-forward "^ *#\\+.+?:.*" nil t))
    (setq beg (1+ (match-end 0)))
    (if (re-search-forward "^* [0-9]+ Linked References$" nil t)
        (setq end (1- (match-beginning 0)))
      (setq end (point-max)))
    (cons beg end)))

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
         (setq title (gkroam--get-title page))
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
  (let (title content page file plist region beg end)
    (goto-char (point-min))
    (while (re-search-forward "^* .+" nil t)
      (setq title (string-trim-left (match-string-no-properties 0) "* "))
      (setq page (gkroam--get-page title))
      (if page
          (setq file (gkroam--get-file page))
        (setq file (gkroam-new title)))
      (goto-char (line-beginning-position))
      (setq plist (cadr (org-element-headline-parser (point-max))))
      (setq beg (plist-get plist :contents-begin))
      (setq end (plist-get plist :contents-end))
      (setq content (string-trim (buffer-substring beg end)))
      (setq content (gkroam-capture-write--process content))
      (goto-char end)
      (save-excursion
        (with-current-buffer (find-file-noselect file t)
          (let (beg2 end2)
            (setq region (gkroam--get-content-region))
            (setq beg2 (car region))
            (setq end2 (cdr region))
            (delete-region beg2 end2)
            (goto-char beg2)
            (insert (format "\n%s\n" content))
            (save-buffer)
            (gkroam-mode)))))))

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
                                       (gkroam--all-titles) nil nil))
          (setq page (gkroam--get-page title))
          (if page
              (setq content (gkroam-capture-append--process
                             (gkroam--get-content (gkroam--get-page title))))
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
            (gkroam-mode)
            (gkroam-capture-mode)
            (setq gkroam-capture-flag t))
        (select-window (get-buffer-window gkroam-capture-buf))
        (gkroam-capture-append title content)
        (gkroam-mode)
        (gkroam-capture-mode)))))

;; ----------------------------------------
;; major mode

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

(defun gkroam-set-window-fringe ()
  "Set gkroam window fringe to zero."
  (fringe-mode 0))

(defun gkroam-set-major-mode ()
  "Set major mode to `gkroam-mode' after find file in `gkroam-root-dir'."
  (when (file-equal-p
         (file-name-directory (buffer-file-name))
         (expand-file-name gkroam-root-dir))
    (gkroam-mode)))

(add-hook 'find-file-hook #'gkroam-set-major-mode)

(define-derived-mode gkroam-mode org-mode "gkroam"
  "Major mode for gkroam."
  
  (add-hook 'completion-at-point-functions #'gkroam-completion-at-point nil 'local)
  (add-hook 'company-completion-finished-hook #'gkroam-completion-finish nil 'local)
  (add-hook 'window-state-change-hook #'gkroam-set-window-margin)

  (add-hook 'gkroam-mode-hook #'gkroam-link-mode)
  (add-hook 'gkroam-mode-hook #'gkroam-link-frame-setup)
  (add-hook 'gkroam-mode-hook #'toggle-truncate-lines)
  (add-hook 'gkroam-mode-hook #'gkroam-prettify-page)
  (add-hook 'gkroam-mode-hook (lambda ()
                                (gkroam-overlay-link (point-min))))
  (add-hook 'gkroam-mode-hook #'gkroam-set-window-fringe)
  
  (add-hook 'gkroam-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        (lambda ()
                          (when (and (eq major-mode 'gkroam-mode)
                                     (eq buffer-read-only nil))
                            (gkroam-prettify-page)
                            (gkroam-overlay-link (point-min))
                            (indent-region (point-min) (point-max))
                            (gkroam-build-page-cache))))))

  (when (require 'ivy nil t)
    (unless (null ivy-mode)
      (setq-local ivy-use-selectable-prompt t)))
  
  (setq gkroam-pages (gkroam--all-titles))
  (setq-local org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq-local gkroam-has-link-p nil)
  (setq-local org-startup-folded nil)
  (setq-local org-return-follows-link t)
  (use-local-map gkroam-mode-map))

;; ---------------------------------
(provide 'gkroam)
;;; gkroam.el ends here
