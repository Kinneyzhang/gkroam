;;; gkroam.el --- A lightweight org-mode roam replica  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 2.3.1
;; Keywords: org, convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/gkroam.el
;; Package-Requires: ((emacs "26.3") (db "0.0.6") (company "0.9.10") (simple-httpd "1.5.1") (undo-tree "0.7.5"))

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

;; Gkroam is a light-weight roam repica, built on top of Org-mode.

;;; ChangeLog:

;; v1.0 - Auto update link references at the buttom of page buffer.

;; v2.0 - Use overlay to hide and show gkroam brackets accordingly and fix some bugs.

;; v2.0.1 - Fix 'hide and show brackets' problems in some main occasion.
;; Such as newline, etc.

;; v2.0.2 - Fix `gkroam-publish-current-file' and `gkroam-preview-current',
;; automatically convert gkroam link to org link and convert it back
;; after published (use 'undo', not reliable). But it has problem with publishing
;; the whole project.

;; v2.0.3 - Fix `gkroam-publish-site' and `gkroam-preview'. Now you can publish and
;; preview the whole roam site.

;; v2.0.4 - Many bugs fixed and code improvement.

;; v2.1.0 - A more powerful linked references system.

;; v2.1.1 - Change package name to 'gkroam' from 'gk-roam'.

;; v2.2.0 - Edit many pages in one side window and save changes separately.

;; v2.2.1 - Many bug fixed and misc code optimization.

;; v2.3.0 - Implement headline references, add a new minor mode `gkroam-dynamic-brackets-mode' and rename 'gkroam-edit' to `gkroam-capture'.

;; 2.3.1 - A more resonable way to insert link. Press "C-p RET" or "C-M-j" to skip headline completion for ivy user or just press "RET" for vanilla user.

;;; Code:

(require 'cl-lib)
(require 'org-id)
(require 'ox-publish)
(require 'db)
(require 'simple-httpd)
(require 'company)
(require 'undo-tree)

;;;; Declarations
(declare-function org-publish-project "ox-publish")
(defvar org-link-frame-setup)
(defvar org-publish-project-alist)

;;;; Variables
(defgroup gkroam nil
  "A roam replica on top of emacs org-mode."
  :tag "gkroam"
  :group 'org)

(defcustom gkroam-root-dir "~/gkroam/org/"
  "Gkroam's root directory, with org files in it."
  :type 'string
  :group 'gkroam)

(defcustom gkroam-pub-dir "~/gkroam/site/"
  "Gkroam's publish directory, with html files in it."
  :type 'string
  :group 'gkroam)

(defcustom gkroam-static-dir "~/gkroam/static/"
  "Gkroam's static files directory."
  :type 'string
  :group 'gkroam)

(defcustom gkroam-static-pub-dir "~/gkroam/site/static/"
  "Gkroam's static files publish directory."
  :type 'string
  :group 'gkroam)

(defcustom gkroam-pub-css "<link rel=\"stylesheet\" href=\"https://gongzhitaao.org/orgcss/org.css\">"
  "Gkroam publish css style."
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

(defvar gkroam-publish-project-alist
  `(("gkroam-page"
     :base-extension "org"
     :recursive nil
     :base-directory ,(expand-file-name gkroam-root-dir)
     :publishing-directory ,(expand-file-name gkroam-pub-dir)
     :publishing-function org-html-publish-to-html
     :html-head ,gkroam-pub-css
     :html-home/up-format
     "<div id=\"org-div-header\"><a href=\"/index.html\">INDEX</a></div?"
     :html-link-home "/"
     :html-link-up "/"
     :with-toc nil
     :section-numbers 0
     :body-only nil)
    ("gkroam-static"
     :base-directory ,(expand-file-name gkroam-static-dir)
     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
     :publishing-directory ,(expand-file-name gkroam-static-pub-dir)
     :recursive t
     :publishing-function org-publish-attachment)
    ("gkroam" :components ("gkroam-page" "gkroam-static")))
  "Gkroam project alist for `org-publish-project-alist'.")

(defvar gkroam-window-margin 2
  "Gkroam window's left and right margin.")

(defvar gkroam-dynamic-brackets nil
  "Non-nil means to show brackets dynamically.")

(defvar gkroam-show-brackets-p nil
  "Non-nil means to show brackets in page link.")

(defvar gkroam-pages nil
  "Page candidates for completion.")

(defvar gkroam-mode-map (make-sparse-keymap)
  "Keymap for `gkroam-mode'.")

(defvar gkroam-has-link-p nil
  "Judge if has link or hashtag in gkroam buffer.")

(defvar gkroam-update-index-timer nil
  "Gkroam indexing timer.")

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

(defvar gkroam-slash-magics nil
  "Gkroam slash commands.")

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
  (directory-files gkroam-root-dir nil (rx bol (+ (in num)) ".org" eol)))

(defun gkroam--all-titles ()
  "Get all gkroam titles."
  (let* ((pages (gkroam--all-pages)))
    (mapcar (lambda (page) (gkroam--get-title page)) pages)))

(defun gkroam--gen-file ()
  "Generate new gkroam file path."
  (expand-file-name (gkroam--gen-page) gkroam-root-dir))

(defun gkroam--gen-page ()
  "Generate new gkroam page filename, without directory prefix."
  (format "%s.org" (format-time-string "%Y%m%d%H%M%S")))

(defun gkroam--format-link (title &optional headline aliase)
  "Format TITLE into a gkroam page link.
With optional argument HEADLINE, format also with headline.
With optional argument ALIASE, format also with aliase."
  (if headline
      (if aliase
          (format "{[%s » %s][%s]}" title headline aliase)
        (format "{[%s » %s]}" title headline))
    (if aliase
        (format "{[%s][%s]}" title aliase)
      (format "{[%s]}" title))))

(defun gkroam--format-backlink (page)
  "Format gkroam backlink in PAGE."
  (let* ((title (gkroam--get-title page)))
    (format "[[file:%s][%s]]" page title)))

;; ----------------------------------------
(defvar gkroam-link-re-format
  "\\(\\(-\\|+\\|*\\|[0-9]+\\.\\|[0-9]+)\\) .*?{\\[%s.*?\\]}.*\\(\n+ +.*\\)*
\\|\\(.*{\\[%s.*?\\]}.*\\\\\n\\(.+\\\\\n\\)*.+\\|\\(.+\\\\\n\\)+.*{\\[%s.*?\\]}.*\\\\\n\\(.+\\\\\n\\)*.+\\|\\(.+\\\\\n\\)+.*{\\[%s.*?\\]}.*\\)
\\|.*#\\+begin_verse.*\n+\\(.+\n+\\|.*{\\[%s.*?\\]}.*\n+\\)*.*{\\[%s.*?\\]}.*\n+\\(\\)+\\(.+\n+\\|.*{\\[%s.*?\\]}.*\n+\\)*.*#\\+end_verse.*
\\|.*{\\[%s.*?\\]}.*\n\\)"
  "Gkroam link regexp format used for searching link context.")

(defun gkroam--search-process (page linum)
  "Return a rg process to search PAGE's link and output LINUM lines before and after matched string."
  (let ((title (gkroam--get-title page))
        (name (generate-new-buffer-name " *gkroam-rg*")))
    (start-process name name "rg" "--ignore-case"
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
          (replace-match (match-string-no-properties 2))
        (if (gkroam--link-has-aliase)
            (replace-match (concat "*" (match-string-no-properties 9) "*"))
          (if (gkroam--link-has-headline)
              (replace-match (concat "*" (match-string-no-properties 2)
                                     " » " (match-string-no-properties 5)  "*"))
            (replace-match (concat "*" (match-string-no-properties 2) "*"))))))
    (buffer-string)))

(defun gkroam-process-searched-string (string title)
  "Process searched STRING by 'rg', get page LINUM*2+1 lines of TITLE and context."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((gkroam-file-re (expand-file-name "[0-9]\\{14\\}\\.org" gkroam-root-dir))
          (num 0) references)
      (while (re-search-forward gkroam-file-re nil t)
        (let* ((path (match-string-no-properties 0))
               (page (file-name-nondirectory path))
               content context)
          (forward-line)
          (catch 'break
            (while (re-search-forward
                    (replace-regexp-in-string "%s" title gkroam-link-re-format)
                    nil t)
              (setq num (1+ num))
              (setq content (concat " " (match-string-no-properties 0) "\n"))
              ;; (setq content (gkroam-process-references-style content))
              (setq context (concat context content))
              (save-excursion
                (when (re-search-forward
                       (replace-regexp-in-string "%s" title gkroam-link-re-format)
                       nil t)
                  (re-search-backward gkroam-file-re nil t)
                  (unless (string= path (match-string-no-properties 0))
                    (throw 'break nil))))))
          (setq context (gkroam--process-link-in-references context))
          (setq references
                (concat references
                        (format "** %s\n%s" (gkroam--format-backlink page) context)))))
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
  (let ((linum 10))
    (gkroam--search-linked-pages
     (gkroam--search-process page linum)
     (lambda (string)
       (let* ((title (gkroam--get-title page))
              (file (gkroam--get-file page))
              (file-buf (find-file-noselect file t)))
         (with-current-buffer file-buf
           (save-excursion
             (goto-char (point-max))
             (re-search-backward "\n-----\n" nil t)
             (delete-region (point) (point-max))
             (unless (string= string "")
               (let* ((processed-str (gkroam-process-searched-string string title))
                      (num (car processed-str))
                      (references (cdr processed-str)))
                 (insert "\n-----\n")
                 (goto-char (point-min))
                 (re-search-forward "-----\n" nil t)
                 (insert (format "* %d Linked References\n" num))
                 (insert references))
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

(defun gkroam-update-index ()
  "Update gkroam index page."
  (let* ((index-org (expand-file-name "index.org" gkroam-root-dir))
         (index-buf (find-file-noselect index-org t)))
    (with-current-buffer index-buf
      (erase-buffer)
      (insert (format "#+TITLE: %s\n\n" gkroam-index-title))
      (dolist (page (gkroam--all-pages))
        (insert (format "+ [[file:%s][%s]]\n" page (gkroam--get-title page))))
      (save-buffer))
    index-buf))

;; ----------------------------------------
;; headline linked references

(defun gkroam--get-headlines (title)
  "Get page title with TITLE headlines."
  (mapcar #'car (db-get title gkroam-db)))

(defun gkroam-goto-headline (id)
  "Goto headline with id ID."
  (org-id-goto id)
  (gkroam-update)
  (gkroam-overlay-buffer))

(defun gkroam-heading-id-pairs ()
  "Return all heading and id pairs of current page."
  (let (end)
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (if (re-search-forward "^-----" nil t)
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
(defun gkroam-insert (&optional title headline aliase)
  "Insert a gkroam page link at point.
With optional arguments, use TITLE or HEADLINE or ALIASE to format link."
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
             (aliase (or aliase
                         (completing-read
                          "Give an aliase, directly press \"RET\" to skip: "
                          nil nil nil))))
        (if (string= headline "")
            (setq headline nil))
        (if (string= aliase "")
            (setq aliase nil))
        (insert (gkroam--format-link title headline aliase))
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
  "Show gkroam index page."
  (interactive)
  (switch-to-buffer (gkroam-update-index))
  (gkroam-mode))

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
  (gkroam-update-index)
  (let ((pages (gkroam--all-pages)))
    (mapcar #'gkroam-update-reference pages)))

(defun gkroam-resolve-link (orig-fun file &rest args)
  "Convert gkroam link to org link.
This is an advice for ORIG-FUN with argument FILE and other ARGS."
  (with-current-buffer (find-file-noselect file t)
    (goto-char (point-min))
    (setq gkroam-has-link-p nil)
    (while (re-search-forward gkroam-link-regexp nil t)
      (setq gkroam-has-link-p t)
      (let (beg end title hashtag-p)
        (setq beg (match-beginning 0))
        (setq end (match-end 0))
        (setq title (match-string-no-properties 2))
        (save-excursion
          (goto-char (1- beg))
          (when (string= (thing-at-point 'char t) "#")
            (setq hashtag-p t)))
        (delete-region beg end)
        (when hashtag-p (delete-region (1- beg) beg))
        (insert (format (if hashtag-p
                            "[[file:%s][#%s]]"
                          "[[file:%s][%s]]")
                        (gkroam--get-page title) title))))
    (save-buffer)
    (apply orig-fun file args)
    (when gkroam-has-link-p
      ;; if possible, use original undo function.
      (undo-tree-undo))))

(defun gkroam-set-project-alist ()
  "Add gkroam project to `org-publish-project-alist'."
  (setq org-publish-project-alist
        (cl-remove-if (lambda (lst)
                        (string-match "gkroam.*" (car lst)))
                      org-publish-project-alist))
  (dolist (lst gkroam-publish-project-alist)
    (add-to-list 'org-publish-project-alist lst)))

;;;###autoload
(defun gkroam-publish-current-file ()
  "Publish current file."
  (interactive)
  (if (gkroam-at-root-p)
      (progn
        (gkroam-update-index)
        (gkroam-set-project-alist)
        (if undo-tree-mode
            (org-publish-file (buffer-file-name))
          (message "please enable 'undo-tree-mode' in this buffer!")))
    (message "Not in the gkroam directory!")))

;;;###autoload
(defun gkroam-preview-current ()
  "Preview current file."
  (interactive)
  (if (gkroam-at-root-p)
      (let ((current-file (concat (file-name-base (buffer-file-name)) ".html")))
        (httpd-serve-directory gkroam-pub-dir)
        (unless (httpd-running-p) (httpd-start))
        (gkroam-publish-current-file)
        (if undo-tree-mode
            (browse-url (format "http://%s:%d/%s" "127.0.0.1" 8080 current-file))
          (message "please enable 'undo-tree-mode' in this buffer!")))
    (message "Not in the gkroam directory!")))

;;;###autoload
(defun gkroam-publish-site (&optional force async)
  "Publish gkroam project to html site.
If FORCE is non-nil, force to publish all pages.
If ASYNC is non-nil, publish pages in an async process."
  (interactive)
  (gkroam-update-index)
  (gkroam-set-project-alist)
  (if global-undo-tree-mode
      (org-publish-project "gkroam" force async)
    (message "please enable 'global-undo-tree-mode'!")))

;;;###autoload
(defun gkroam-preview ()
  "Preview gkroam site."
  (interactive)
  (httpd-serve-directory gkroam-pub-dir)
  (unless (httpd-running-p) (httpd-start))
  (gkroam-publish-site nil nil)
  (if global-undo-tree-mode
      (browse-url (format "http://%s:%d" "127.0.0.1" 8080))
    (message "please enable 'global-undo-tree-mode'!")))

;;; ----------------------------------------
;; minor mode: gkroam-link-mode

(define-button-type 'gkroam-link
  'action #'gkroam-follow-link
  'face '(:underline nil)
  'title nil
  'headline nil
  'aliase nil
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
           (aliase (when (gkroam--link-has-aliase)
                     (match-string-no-properties 9)))
           (echo (if headline
                     (concat title " » " headline)
                   title)))
      (make-text-button (match-beginning 0)
                        (match-end 0)
                        :type 'gkroam-link
                        'title title
                        'headline headline
                        'aliase aliase
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

(defun gkroam--link-has-aliase ()
  "Judge if a gkroam link has aliase after `re-search-forward'."
  (not (string-empty-p (match-string-no-properties 7))))

(defun gkroam-overlay-link ()
  "Set overlays to gkroam page link."
  (with-silent-modifications
    (if (gkroam--link-has-aliase)
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
    (if (gkroam--link-has-aliase)
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

(defun gkroam-put-overlays (beg &optional bound)
  "Put overlays between BEG and BOUND."
  (when (eq major-mode 'gkroam-mode)
    (let ((bound (or bound (point-max))))
      (save-excursion
        (goto-char beg)
        (when (re-search-forward "\\(^ *#\\+TITLE: \\)\\(.*\\)" bound t)
          (overlay-put (make-overlay (match-beginning 1) (match-beginning 2))
                       'display "")
          (overlay-put (make-overlay (match-beginning 2) (match-end 0))
                       'face '(:height 300)))
        (while (re-search-forward gkroam-link-regexp bound t)
          (if (string= (char-to-string (char-before (match-beginning 0))) "#")
              (gkroam-overlay-hashtag)
            (if gkroam-show-brackets-p
                (gkroam-show-entire-link)
              (gkroam-overlay-link))))))))

;;;###autoload
(defun gkroam-link-edit ()
  "Edit gkroam link aliase when `gkroam-dynamic-brackets-mode' is disabled."
  (interactive)
  (if-let ((btn (button-at (point))))
      (let* ((btn-label (button-label btn))
             (btn-start (button-start btn))
             (btn-end (button-end btn))
             (new-link (completing-read "Edit link: " nil nil nil btn-label)))
        (delete-region btn-start btn-end)
        (insert new-link))
    (message "no link at point")))

;; gkroam-dynamic-brackets-mode

(defun gkroam-restore-line-overlays ()
  "Restore overlays in last line."
  (gkroam-put-overlays (line-beginning-position) (line-end-position)))

(defun gkroam-remove-line-overlays ()
  "Remove overlays in current line."
  (when (eq major-mode 'gkroam-mode)
    (save-excursion
      (goto-char (line-beginning-position))
      (when (re-search-forward "\\(^ *#\\+TITLE: \\)\\(.*\\)" nil t)
        (with-silent-modifications
          (remove-overlays (line-beginning-position) (line-end-position))))
      (when (re-search-forward gkroam-link-regexp (line-end-position) t)
        (with-silent-modifications
          (remove-overlays (line-beginning-position) (line-end-position)))))))

(defun gkroam-overlay-buffer ()
  "Put overlay in currnt gkroam buffer."
  (gkroam-put-overlays (point-min) (point-max)))

(define-minor-mode gkroam-dynamic-brackets-mode
  "Minor for showing brackets dynamically.
When the cursor moves on a line with links, show brackets.
Hide brackets when the cursor moves out of the line."
  nil " dynamic" nil
  (if gkroam-dynamic-brackets-mode
      (progn
        (add-hook 'pre-command-hook #'gkroam-restore-line-overlays)
        (add-hook 'post-command-hook #'gkroam-remove-line-overlays))
    (remove-hook 'pre-command-hook #'gkroam-restore-line-overlays)
    (remove-hook 'post-command-hook #'gkroam-remove-line-overlays)))

;;;###autoload
(defun gkroam-toggle-brackets ()
  "Determine whether to show brackets in page link."
  (interactive)
  (if gkroam-show-brackets-p
      (setq gkroam-show-brackets-p nil)
    (setq gkroam-show-brackets-p t))
  (gkroam-overlay-buffer))

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
    (if (re-search-forward "^-----+" nil t)
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
            (gkroam-overlay-buffer)))))))

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

(defun gkroam-company-slash-p ()
  "Judge if need to company slash."
  (save-excursion
    (skip-chars-backward "^/" (line-beginning-position))
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
  "Function binded to `company-completion-finish-hook' after finishing complete TITLE."
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
      (list beg end gkroam-pages . nil))
     ((gkroam-company-slash-p)
      (setq bds (bounds-of-thing-at-point 'symbol))
      (setq beg (car bds))
      (setq end (cdr bds))
      (list beg end gkroam-slash-magics . nil)))))

;;;###autoload
(defun gkroam-set-major-mode ()
  "Set major mode to `gkroam-mode' after find file in `gkroam-root-dir'."
  (interactive)
  (when (file-equal-p
         (file-name-directory (buffer-file-name))
         (expand-file-name gkroam-root-dir))
    (gkroam-mode)))

(add-hook 'find-file-hook #'gkroam-set-major-mode)

(define-derived-mode gkroam-mode org-mode "gkroam"
  "Major mode for gkroam."
  (gkroam-link-mode)
  
  (add-hook 'completion-at-point-functions #'gkroam-completion-at-point nil 'local)
  (add-hook 'company-completion-finished-hook #'gkroam-completion-finish nil 'local)
  
  (add-hook 'gkroam-mode-hook #'gkroam-link-frame-setup)
  (add-hook 'gkroam-mode-hook #'gkroam-set-project-alist)
  (add-hook 'gkroam-mode-hook #'toggle-truncate-lines)
  (add-hook 'gkroam-mode-hook #'gkroam-overlay-buffer)
  (add-hook 'gkroam-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        (lambda ()
                          (when (and (eq major-mode 'gkroam-mode)
                                     (eq buffer-read-only nil))
                            (gkroam-overlay-buffer)
                            (gkroam-build-page-cache))))))
  
  (advice-add 'org-publish-file :around #'gkroam-resolve-link)

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
