;;; gkroam.el --- A light-weight org-mode roam replica  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 2.1.1
;; Keywords: org, convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: http://github.com/Kinneyzhang/gkroam.el
;; Package-Requires: ((emacs "27.1") (simple-httpd "1.5.1") (undo-tree "0.7.5"))

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

;;; Code:

(require 'ox-publish)
(require 'simple-httpd)
(require 'undo-tree)

;;;; Declarations
;; (declare-function org-get-heading "org")
(declare-function org-publish-project "ox-publish")
(defvar org-link-frame-setup)

;;;; Variables
(defgroup gkroam nil
  "A roam replica on top of emacs org-mode."
  :tag "gkroam")

(defcustom gkroam-root-dir ""
  "Gkroam's root directory, with org files in it."
  :type 'string
  :group 'gkroam)

(defcustom gkroam-pub-dir ""
  "Gkroam's publish directory, with html files in it."
  :type 'string
  :group 'gkroam)

(defcustom gkroam-pub-css "<link rel=\"stylesheet\" href=\"https://gongzhitaao.org/orgcss/org.css\">"
  "Gkroam publish css style."
  :type 'string
  :group 'gkroam)

(defvar gkroam-toggle-brackets-p t
  "Determine whether to show brackets in page link.")

(defvar gkroam-pages nil
  "Page candidates for completion.")

(defvar gkroam-mode-map nil
  "Keymap for `gkroam-mode'.")

(defvar gkroam-has-link-p nil
  "Judge if has link or hashtag in gkroam buffer.")

(defvar gkroam-link-regexp
  (rx (seq (group "{[")
           (group (+? (not (any "/\n"))))
           (group "]}")))
  "Regular expression that matches a gkroam link.")

(defvar gkroam-hashtag-regexp
  (rx (seq (group "#{[")
           (group (+? (not (any "/\n"))))
           (group "]}")))
  "Regular expression that matches a gkroam hashtag.")

;;;; Functions
(defun gkroam-link-frame-setup ()
  "Alter `org-link-frame-setup' for gkroam."
  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame))))

;; -------------------------------
(defun gkroam--get-title (page)
  "Get PAGE's title."
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (gkroam--get-file page) nil 0 2000 t)
    (goto-char (point-min))
    (re-search-forward (concat "^ *#\\+TITLE:") nil t)
    (plist-get (cadr (org-element-at-point)) :value)))

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
  (expand-file-name (concat gkroam-root-dir page)))

(defun gkroam--all-pages ()
  "Get all gkroam pages."
  (directory-files gkroam-root-dir nil (rx bol (+ (in num)) ".org" eol)))

(defun gkroam--all-titles ()
  "Get all gkroam titles."
  (let* ((pages (gkroam--all-pages))
	 (titles (mapcar (lambda (page) (gkroam--get-title page)) pages)))
    titles))

(defun gkroam--gen-file ()
  "Generate new gkroam file."
  (concat gkroam-root-dir (format "%s.org" (format-time-string "%Y%m%d%H%M%S"))))

(defun gkroam--gen-page ()
  "Generate new gkroam page."
  (format "%s.org" (format-time-string "%Y%m%d%H%M%S")))

(defsubst gkroam--format-link (title)
  "Format TITLE into a gkroam page link."
  (format "{[%s]}" title))

(defun gkroam--format-backlink (page)
  "Format gkroam backlink in PAGE."
  (let* ((title (gkroam--get-title page)))
    (format "[[file:%s][%s]]" page title)))

;; ----------------------------------------
(setq gkroam-link-re-format
      "\\(\\(-\\|+\\|*\\|[0-9]+\\.\\|[0-9]+)\\) .*?{\\[%s\\]}.*\\(\n+ +.*\\)*
\\|\\(.*{\\[%s\\]}.*\\\\\n\\(.+\\\\\n\\)*.+\\|\\(.+\\\\\n\\)+.*{\\[%s\\]}.*\\\\\n\\(.+\\\\\n\\)*.+\\|\\(.+\\\\\n\\)+.*{\\[%s\\]}.*\\)
\\|.*#\\+begin_verse.*\n+\\(.+\n+\\|.*{\\[%s\\]}.*\n+\\)*.*{\\[%s\\]}.*\n+\\(\\)+\\(.+\n+\\|.*{\\[%s\\]}.*\n+\\)*.*#\\+end_verse.*
\\|.*{\\[%s\\]}.*\n\\)")

(defun gkroam--search-process (page linum)
  "Return a rg process to search PAGE's link and output LINUM lines before and after matched string."
  (let ((title (gkroam--get-title page))
	(name (generate-new-buffer-name " *gkroam-rg*")))
    (start-process
     name name "rg" "-C" (number-to-string linum)
     "-FN" "--heading"
     (format "{[%s]}" title)
     (expand-file-name gkroam-root-dir) ;; must be absolute path.
     "-g" "!index.org*")))

(defun gkroam--process-link-in-references (string)
  "Remove links in reference's STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "#{\\[" nil t)
      (replace-match "*#"))
    (goto-char (point-min))
    (while (re-search-forward "\\({\\[\\|\\]}\\)" nil t)
      (replace-match "*"))
    (buffer-string)))

(defun gkroam-process-searched-string (string title linum)
  "Process searched STRING by 'rg', get page LINUM*2+1 lines of TITLE and context."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((gkroam-file-re (format "%s[0-9]\\{14\\}\\.org"
				  (expand-file-name gkroam-root-dir)))
	  (num 0) references)
      (while (re-search-forward gkroam-file-re nil t)
	(let* ((path (match-string-no-properties 0))
	       (page (file-name-nondirectory path))
	       beg end content context)
	  (forward-line)
	  (catch 'break
	    (while (re-search-forward
		    (replace-regexp-in-string "%s" title gkroam-link-re-format)
		    nil t)
	      (setq num (1+ num))
	      (setq content (concat (match-string-no-properties 0) "\n"))
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
	      (file-buf (or (get-file-buffer file)
			    (find-file-noselect file nil nil))))
	 (with-current-buffer file-buf
	   (save-excursion
	     (goto-char (point-max))
	     (re-search-backward "\n-----\n" nil t)
	     (delete-region (point) (point-max))
	     (unless (string= string "")
	       (let* ((processed-str (gkroam-process-searched-string string title linum))
		      (num (car processed-str))
		      (references (cdr processed-str)))
		 (insert "\n-----\n")
		 (goto-char (point-min))
		 (re-search-forward "-----\n" nil t)
		 (insert (format "* %d Linked References\n" num))
		 (insert references))
	       (save-buffer))))))))
  (message "%s reference updated" page))

;; -----------------------------------

(defun gkroam-new (title)
  "Just create a new gkroam page titled with TITLE."
  (let* ((file (gkroam--gen-file))
	 (file-buf (find-file-noselect file))
	 beg)
    (with-current-buffer file-buf
      (insert
       (format "#+TITLE: %s\n#+DATE: %s\n#+OPTIONS: toc:nil H:2 num:0\n» [[file:index.org][ /Gkroam/ ]]\n\n" title (format-time-string "%Y-%m-%d")))
      (save-buffer))
    (setq gkroam-pages (gkroam--all-titles))
    file))

(defun gkroam-update-index ()
  "Update gkroam index page."
  (let* ((index-org (concat gkroam-root-dir "index.org"))
         (index-buf (or (get-file-buffer index-org)
                        (find-file-noselect index-org))))
    (with-current-buffer index-buf
      (erase-buffer)
      (insert "#+TITLE: INDEX\n#+OPTIONS: toc:nil H:2 num:0\n\n* Site Map\n\n")
      (dolist (page (gkroam--all-pages))
	(insert (format " - [[file:%s][%s]]\n" page (gkroam--get-title page))))
      (save-buffer))
    index-buf))

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
    (gkroam-update)))

;;;###autoload
(defun gkroam-daily ()
  "Create or open gkroam daily notes."
  (interactive)
  (let* ((title (format-time-string "%b %d, %Y")))
    (gkroam-find title)))

;;;###autoload
(defun gkroam-insert (&optional title)
  "Insert a gkroam page titled with TITLE."
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gkroam-root-dir))
      (let* ((title (or title (completing-read
			       "Choose a page or create a new: "
			       (gkroam--all-titles) nil nil
			       (thing-at-point 'word t))))
	     (page (gkroam--get-page title)))
	(insert (gkroam--format-link title))
	(save-buffer)
	(gkroam-update-reference page))
    (message "Not in the gkroam directory!")))

;;;###autoload
(defun gkroam-new-at-point ()
  "Insert a file link and create a new file according to text at point."
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gkroam-root-dir))
      (let* ((title (thing-at-point 'word t))
	     (page-exist-p (gkroam--get-page title)))
	(if page-exist-p
	    (progn
	      (backward-word)
	      (kill-word 1)
	      (gkroam-insert title)
	      (save-buffer))
	  (gkroam-new title)
	  (backward-word)
	  (kill-word 1)
	  (gkroam-insert title)
	  (gkroam-find title)))
    (message "Not in the gkroam directory!")))

;;;###autoload
(defun gkroam-new-from-region ()
  "Insert a file link and create a new file according to a selected region."
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gkroam-root-dir))
      (let* ((beg (region-beginning))
	     (end (region-end))
	     (title (when (region-active-p)
		      (buffer-substring-no-properties beg end)))
	     (page-exist-p (gkroam--get-page title)))
	(if page-exist-p
	    (progn
	      (delete-region beg end)
	      (gkroam-insert title)
	      (save-buffer))
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
   (t (funcall-interactively 'gkroam-find))))

;;;###autoload
(defun gkroam-index ()
  "Show gkroam index page."
  (interactive)
  (switch-to-buffer (gkroam-update-index)))

;;;###autoload
(defun gkroam-update ()
  "Update current gkroam buffer's reference."
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gkroam-root-dir))
      (gkroam-update-reference (file-name-nondirectory (buffer-file-name)))
    (message "Not in the gkroam directory!")))

;;;###autoload
(defun gkroam-update-all () ;; have problem!
  "Update all gkroam files' reference."
  (interactive)
  (gkroam-update-index)
  (let ((pages (gkroam--all-pages)))
    (mapcar #'gkroam-update-reference pages)))

(defun gkroam-resolve-link (orig-fun file &rest args)
  "Convert gkroam link to org link.
This is an advice for ORIG-FUN with argument FILE and other ARGS."
  (let ((file-buf (or (get-file-buffer file)
		      (find-file-noselect file))))
    (with-current-buffer file-buf
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
	  (if hashtag-p
	      (progn
		(delete-region (1- beg) end)
		(insert (format "[[file:%s][#%s]]" (gkroam--get-page title) title)))
	    (delete-region beg end)
	    (insert (format "[[file:%s][%s]]" (gkroam--get-page title) title)))))
      (save-buffer)
      (apply orig-func file args)
      (when gkroam-has-link-p
	;; if possible, use original undo function.
	(undo-tree-undo)))))

(defun gkroam-set-project-alist ()
  "Add gkroam project to `org-publish-project-alist'."
  (add-to-list
   'org-publish-project-alist
   `("gkroam"
     :base-extension "org"
     :recursive nil
     :base-directory ,(expand-file-name gkroam-root-dir)
     :publishing-directory ,(expand-file-name gkroam-pub-dir)
     :publishing-function org-html-publish-to-html
     :html-head ,gkroam-pub-css)))

;;;###autoload
(defun gkroam-publish-current-file ()
  "Publish current file."
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gkroam-root-dir))
      (progn
	(gkroam-update)
	(if undo-tree-mode
	    (org-publish-file (buffer-file-name))
	  (message "please enable 'undo-tree-mode' in this buffer!")))
    (message "Not in the gkroam directory!")))

;;;###autoload
(defun gkroam-preview-current ()
  "Preview current file."
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gkroam-root-dir))
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
  ;; (gkroam-update-all)
  (if global-undo-tree-mode
      (org-publish-project "gkroam" force async)
    (message "please enable 'global-undo-tree-mode'!")))

;;;###autoload
(defun gkroam-preview ()
  "Preview gkroam site."
  (interactive)
  (progn
    (httpd-serve-directory gkroam-pub-dir)
    (unless (httpd-running-p) (httpd-start))
    (gkroam-publish-site t nil)
    (if global-undo-tree-mode
	(browse-url (format "http://%s:%d" "127.0.0.1" 8080))
      (message "please enable 'global-undo-tree-mode'!"))))

;; --------------------------------------------
;; Edit pages in side buffer, each page is under a headline.

;; (defun gkroam-side-edit (title)
;;   "Edit a page titled with TITLE in a side window."
;;   (interactive))

;; --------------------------------------------
;; slash magic

;; -------------------------------------------
;; minor mode

(define-button-type 'gkroam-link
  'action #'gkroam-follow-link
  'title nil
  'follow-link t
  'help-echo "Jump to page")

(defun gkroam-follow-link (button)
  "Jump to the page that BUTTON represents."
  (with-demoted-errors "Error when following the link: %s"
    (gkroam-find (button-get button 'title))))

(defun gkroam-link-fontify (beg end)
  "Put gkroam link between BEG and END."
  (goto-char beg)
  (while (re-search-forward gkroam-link-regexp end t)
    (make-text-button (match-beginning 0)
		      (match-end 0)
		      :type 'gkroam-link
		      'face '(:underline nil)
		      ;; 'mouse-face '(:underline nil)
		      'title (match-string-no-properties 2))))

(defun gkroam-hashtag-fontify(beg end)
  "Put gkroam link between BEG and END."
  (goto-char beg)
  (while (re-search-forward gkroam-hashtag-regexp end t)
    (make-text-button (match-beginning 0)
		      (match-end 0)
		      :type 'gkroam-link
		      'face '(:underline nil)
		      'title (match-string-no-properties 2))))

(define-minor-mode gkroam-link-minor-mode
  "Recognize gkroam link."
  :lighter ""
  :keymap (make-sparse-keymap)
  (if gkroam-link-minor-mode
      (progn
	(jit-lock-register #'gkroam-hashtag-fontify)
	(jit-lock-register #'gkroam-link-fontify))
    (jit-lock-unregister #'gkroam-hashtag-fontify)
    (jit-lock-unregister #'gkroam-link-fontify))
  (jit-lock-refontify))

;; -------------------------------------------------
;; gkroam overlays

(defun gkroam-overlay-region (beg end prop value)
  "Put overlays in region started by BEG and ended with END.
The overlays has a PROP and VALUE."
  (overlay-put (make-overlay beg end) prop value))

(defun gkroam-overlay-hashtag ()
  "Overlay gkroam hashtag."
  (with-silent-modifications
    (gkroam-overlay-region (match-beginning 1) (match-beginning 2) 'display "")
    (gkroam-overlay-region (match-beginning 3) (match-end 0) 'display "")
    (gkroam-overlay-region (1- (match-beginning 0)) (match-end 0) 'face '(shadow (:underline nil)))))

(defun gkroam-overlay-shadow-brackets ()
  "Set overlays to shadow brackets."
  (with-silent-modifications
    (remove-overlays (match-beginning 1) (match-beginning 2) 'display "")
    (remove-overlays (match-beginning 3) (match-end 0) 'display "")
    (gkroam-overlay-region (match-beginning 1) (match-beginning 2) 'face 'shadow)
    (gkroam-overlay-region (match-beginning 3) (match-end 0) 'face 'shadow)
    (gkroam-overlay-region (match-beginning 0) (match-end 0) 'face '(warning (:underline nil)))))

(defun gkroam-overlay-hide-brackets ()
  "Set overlays to hide gkroam brackets."
  (with-silent-modifications
    (gkroam-overlay-region (match-beginning 1) (match-beginning 2) 'display "")
    (gkroam-overlay-region (match-beginning 3) (match-end 0) 'display "")
    (gkroam-overlay-region (match-beginning 0) (match-end 0) 'face '(warning (:underline nil)))))

(defun gkroam-put-overlays (beg &optional bound)
  "Put overlays between BEG and BOUND."
  (when (string= major-mode "gkroam-mode")
    (let ((bound (or bound (point-max)))
	  pos)
      (save-excursion
	(goto-char beg)
	(while (re-search-forward gkroam-link-regexp bound t)
	  (setq pos (point))
	  (goto-char (1- (match-beginning 0)))
	  (if (string= (thing-at-point 'char t) "#")
	      (gkroam-overlay-hashtag)
	    (if gkroam-toggle-brackets-p
		(gkroam-overlay-shadow-brackets)
	      (gkroam-overlay-hide-brackets)))
	  (goto-char pos))))))

(defun gkroam-remove-overlays ()
  "Remove overlays in current line."
  (when (string= major-mode "gkroam-mode")
    (save-excursion
      (goto-char (line-beginning-position))
      (when (re-search-forward gkroam-link-regexp (line-end-position) t)
	(with-silent-modifications
	  (remove-overlays (line-beginning-position) (line-end-position)))))))

(defun gkroam-overlay-buffer ()
  "Put overlay in currnt gkroam buffer."
  (gkroam-put-overlays (line-end-position) (point-max))
  (gkroam-put-overlays (point-min) (line-beginning-position)))

(defun gkroam-overlay1 (orig-fun &rest args)
  "Advice for ORIG-FUN with ARGS to automatically hide and show brackets when cursor move."
  (gkroam-put-overlays (line-beginning-position) (line-end-position))
  (apply orig-fun args)
  (gkroam-remove-overlays))

(defun gkroam-overlay2 (orig-fun &rest args)
  "Advice for ORIG-FUN with ARGS to automatically hide and show brackets when cursor move."
  (gkroam-put-overlays (line-beginning-position) (line-end-position))
  (apply orig-fun args)
  (unless (ignore-errors (mouse-on-link-p (point)))
    (gkroam-remove-overlays)))

;;;###autoload
(defun gkroam-toggle-brackets ()
  "Determine whether to show brackets in page link."
  (interactive)
  (if gkroam-toggle-brackets-p
      (setq gkroam-toggle-brackets-p nil)
    (setq gkroam-toggle-brackets-p t))
  (gkroam-overlay-buffer))

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

(defun gkroam--complete-hashtag ()
  "Complete hashtag with brackets."
  (when (gkroam-company-hashtag-p)
    (save-excursion
      (let (end len)
	(setq end (point))
	(setq len (abs (skip-chars-backward "^#")))
	(insert "{[")
	(forward-char len)
	(insert "]}")))))

(defun gkroam-completion-finish (title)
  "Function binded to `company-completion-finish-hook' after finishing complete TITLE."
  (when (gkroam-company-hashtag-p)
    (gkroam--complete-hashtag)
    (save-buffer)))

;; (defvar gkroam-slash-magics nil)

;; (setq gkroam-slash-magics '("TODO" "Page Reference" "Hashtag" "Current Time"))

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

(progn
  (setq gkroam-mode-map (make-sparse-keymap)))

(defun gkroam-set-major-mode ()
  "Set major mode to `gkroam-mode' after find file in `gkroam-root-dir'."
  (when (string=
	 (file-name-directory (buffer-file-name))
	 (expand-file-name gkroam-root-dir))
    (gkroam-mode)))

(add-hook 'find-file-hook #'gkroam-set-major-mode)

(define-derived-mode gkroam-mode org-mode "gkroam"
  "Major mode for gkroam."
  (add-hook 'completion-at-point-functions 'gkroam-completion-at-point nil 'local)
  (add-hook 'company-completion-finished-hook 'gkroam-completion-finish nil 'local)
  (add-hook 'gkroam-mode-hook 'gkroam-link-frame-setup)
  (add-hook 'gkroam-mode-hook 'gkroam-set-project-alist)
  (add-hook 'gkroam-mode-hook 'toggle-truncate-lines)

  (advice-add 'org-publish-file :around #'gkroam-resolve-link)
  
  ;; It's ugly to use 'advice-add', though things seem to go well.
  ;; But I haven't found a better way to auto hide and show brackets.
  (advice-add 'next-line :around #'gkroam-overlay1)
  (advice-add 'previous-line :around #'gkroam-overlay1)
  (advice-add 'newline :around #'gkroam-overlay1)
  (advice-add 'org-delete-backward-char :around #'gkroam-overlay1)
  (advice-add 'org-meta-return :around #'gkroam-overlay1)
  (advice-add 'mouse-drag-region :around #'gkroam-overlay2)
  (if (require 'hungry-delete nil t)
      (advice-add 'hungry-delete-backward :around #'gkroam-overlay1))
  
  (gkroam-link-minor-mode)
  (add-hook 'gkroam-mode-hook 'gkroam-overlay-buffer)
  
  (setq gkroam-pages (gkroam--all-titles))
  (setq-local gkroam-has-link-p nil)
  (use-local-map gkroam-mode-map))

;; ---------------------------------
(provide 'gkroam)
;;; gkroam.el ends here
