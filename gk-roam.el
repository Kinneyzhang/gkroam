;;; gk-roam.el --- A light-weight org-mode roam replica  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 2.1.0
;; Keywords: org, convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: http://github.com/Kinneyzhang/gk-roam
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

;; Gk-roam is a light-weight roam repica, built on top of Org-mode.

;;; ChangeLog:

;; v1.0 - Auto update link references at the buttom of page buffer.

;; v2.0 - Use overlay to hide and show gk-roam brackets accordingly and fix some bugs.

;; v2.0.1 - Fix 'hide and show brackets' problems in some main occasion.
;; Such as newline, etc.

;; v2.0.2 - Fix `gk-roam-publish-current-file' and `gk-roam-preview-current',
;; automatically convert gk-roam link to org link and convert it back
;; after published (use 'undo', not reliable). But it has problem with publishing
;; the whole project.

;; v2.0.3 - Fix `gk-roam-publish-site' and `gk-roam-preview'. Now you can publish and
;; preview the whole roam site.

;; v2.0.4 - Many bugs fixed and code improvement.

;; v2.1.0 - A more powerful linked references system.

;;; Code:

(require 'ox-publish)
(require 'simple-httpd)
(require 'undo-tree)

;;;; Declarations
(declare-function org-get-heading "org")
(declare-function org-publish-project "ox-publish")
(defvar org-link-frame-setup)

;;;; Variables
(defvar gk-roam-root-dir ""
  "Gk-roam's root directory, with org files in it.")

(defvar gk-roam-pub-dir ""
  "Gk-roam's publish directory, with html files in it.")

(defvar gk-roam-pub-css "<link rel=\"stylesheet\" href=\"https://gongzhitaao.org/orgcss/org.css\">"
  "Gk-roam publish css link.")

(defvar gk-roam-temp-file (concat user-emacs-directory "gk-roam/temp")
  "Gk-roam temporary file.")

(defvar gk-roam-toggle-brackets-p t
  "Determine whether to show brackets in page link.")

(defvar gk-roam-pages nil
  "Page candidates for completion.")

(defvar gk-roam-mode-map nil
  "Keymap for `gk-roam-mode'")

(defvar gk-roam-has-link-p nil
  "Judge if has link or hashtag in gk-roam buffer.")

(defvar gk-roam-link-regexp
  (rx (seq (group "{[")
           (group (+? (not (any "/\n"))))
           (group "]}")))
  "Regular expression that matches a gk-roam link.")

(defvar gk-roam-hashtag-regexp
  (rx (seq (group "#{[")
           (group (+? (not (any "/\n"))))
           (group "]}")))
  "Regular expression that matches a gk-roam hashtag.")

;;;; Functions
(defun gk-roam-link-frame-setup ()
  "Alter `org-link-frame-setup' for gk-roam."
  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame))))

;; -------------------------------
(defun gk-roam--get-title (page)
  "Get page title."
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (gk-roam--get-file page) nil 0 2000 t)
    (goto-char (point-min))
    (re-search-forward (concat "^ *#\\+TITLE:") nil t)
    (plist-get (cadr (org-element-at-point)) :value)))

(defun gk-roam--get-page (title)
  (let ((pages (gk-roam--all-pages))
	file)
    (catch 'break
      (dolist (page pages)
	(setq file (gk-roam--get-file page))
	(with-temp-buffer
	  (insert-file-contents file nil 0 2000 t)
	  (goto-char (point-min))
	  (when (re-search-forward (format "^ *#\\+TITLE: *%s *$" title) nil t)
	    (throw 'break page)))))))

(defun gk-roam--get-file (page)
  "Get gk-roam file accroding to PAGE."
  (expand-file-name (concat gk-roam-root-dir page)))

(defun gk-roam--all-pages ()
  "Get all gk-roam pages."
  (directory-files gk-roam-root-dir nil (rx bol (+ (in num)) ".org" eol)))

(defun gk-roam--all-titles ()
  "Get all gk-roam titles"
  (let* ((pages (gk-roam--all-pages))
	 (titles (mapcar (lambda (page) (gk-roam--get-title page)) pages)))
    titles))

(defun gk-roam--gen-file ()
  "Generate new gk-roam file."
  (concat gk-roam-root-dir (format "%s.org" (format-time-string "%Y%m%d%H%M%S"))))

(defun gk-roam--gen-page ()
  "Generate new gk-roam page."
  (format "%s.org" (format-time-string "%Y%m%d%H%M%S")))

(defsubst gk-roam--format-link (title)
  "Format TITLE into a gk-roam page link."
  (format "{[%s]}" title))

(defun gk-roam-heading-of-line (line page)
  "Get the org heading of specific LINE in FILE."
  (let ((line (if (stringp line) (string-to-number line) line))
	heading)
    (with-temp-buffer
      (insert-file-contents (gk-roam--get-file page))
      (goto-char (point-min))
      (forward-line line)
      (org-mode)
      (setq heading (org-get-heading t t t t)))
    heading))

(defun gk-roam--format-backlink (page)
  "Format gk-roam backlink in PAGE."
  (let* ((title (gk-roam--get-title page)))
    (format "[[file:%s][%s]]" page title)))

;; ----------------------------------------
(setq gk-roam-link-re-format
      "\\(\\(-\\|+\\|*\\|[0-9]+\\.\\|[0-9]+)\\) .*?{\\[%s\\]}.*\\(\n+ +.*\\)*
\\|\\(.*{\\[%s\\]}.*\\\\\n\\(.+\\\\\n\\)*.+\\|\\(.+\\\\\n\\)+.*{\\[%s\\]}.*\\\\\n\\(.+\\\\\n\\)*.+\\|\\(.+\\\\\n\\)+.*{\\[%s\\]}.*\\)
\\|.*#\\+begin_verse.*\n+\\(.+\n+\\|.*{\\[%s\\]}.*\n+\\)*.*{\\[%s\\]}.*\n+\\(\\)+\\(.+\n+\\|.*{\\[%s\\]}.*\n+\\)*.*#\\+end_verse.*
\\|.*{\\[%s\\]}.*\n\\)")

(defun gk-roam--search-process (page linum)
  "Search gk-roam links or hashtags in org-mode in list,
and output NUM*2 lines before and after the link line."
  (let ((title (gk-roam--get-title page))
	(name (generate-new-buffer-name " *gk-roam-rg*")))
    (start-process
     name name "rg" "-C" (number-to-string linum)
     "-FN" "--heading"
     (format "{[%s]}" title)
     (expand-file-name gk-roam-root-dir) ;; must be absolute path.
     "-g" "!index.org*")))

(defun gk-roam--process-link-in-references (str)
  "Remove links in reference's text."
  (with-temp-buffer
    (string-trim str "\n+" "\n+")
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "#{\\[" nil t)
      (replace-match "*#"))
    (goto-char (point-min))
    (while (re-search-forward "\\({\\[\\|\\]}\\)" nil t)
      (replace-match "*"))
    (buffer-string)))

(defun gk-roam-process-searched-string (string title linum)
  "Process searched STRING by 'rg', get page LINUM*2+1 lines of TITLE and context."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((gk-roam-file-re (format "%s[0-9]\\{14\\}\\.org"
				   (expand-file-name gk-roam-root-dir)))
	  (num 0) references)
      (while (re-search-forward gk-roam-file-re nil t)
	(let* ((path (match-string-no-properties 0))
	       (page (file-name-nondirectory path))
	       beg end content context)
	  (forward-line)
	  (catch 'break
	    (while (re-search-forward
		    (replace-regexp-in-string "%s" title gk-roam-link-re-format)
		    nil t)
	      (setq num (1+ num))
	      (setq content (concat (match-string-no-properties 0) "\n"))
	      ;; (setq content (gk-roam-process-references-style content))
	      (setq context (concat context content))
	      (save-excursion
		(when (re-search-forward
		       (replace-regexp-in-string "%s" title gk-roam-link-re-format)
		       nil t)
		  (re-search-backward gk-roam-file-re nil t)
		  (unless (string= path (match-string-no-properties 0))
		    (throw 'break nil))))))
	  (setq context (gk-roam--process-link-in-references context))
	  (setq references
		(concat references
			(format "** %s\n%s" (gk-roam--format-backlink page) context)))))
      (cons num references))))

(defun gk-roam--search-linked-pages (process callback)
  "Call CALLBACK with the matched string that has a link to PAGE,
 using the rg PROCESS."
  (let (sentinel)
    (setq sentinel
	  (lambda (process event)
            (if (string-match-p (rx (or "finished" "exited"))
				event)
                (if-let ((buf (process-buffer process)))
                    (with-current-buffer buf
		      (funcall callback (buffer-string)))
                  (error "Gk-roam’s rg process’ buffer is killed"))
	      (error "Gk-roam’s rg process failed with signal: %s"
                     event))))
    (set-process-sentinel process sentinel)))

(defun gk-roam-update-reference (page)
  "Update gk-roam file reference."
  (unless (executable-find "rg")
    (user-error "Cannot find program rg"))
  (let ((linum 10))
    (gk-roam--search-linked-pages
     (gk-roam--search-process page linum)
     (lambda (string)
       (let* ((title (gk-roam--get-title page))
	      (file (gk-roam--get-file page))
	      (file-buf (or (get-file-buffer file)
			    (find-file-noselect file nil nil))))
	 (with-current-buffer file-buf
	   (save-excursion
	     (goto-char (point-max))
	     (re-search-backward "\n-----\n" nil t)
	     (delete-region (point) (point-max))
	     (unless (string= string "")
	       (let* ((processed-str (gk-roam-process-searched-string string title linum))
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

(defun gk-roam-new (title)
  "Just create a new gk-roam file."
  (let* ((file (gk-roam--gen-file))
	 (file-buf (find-file-noselect file))
	 beg)
    (with-current-buffer file-buf
      (insert
       (format "#+TITLE: %s\n#+DATE: %s\n#+OPTIONS: toc:nil H:2 num:0\n» [[file:index.org][ /Gk-Roam/ ]]\n\n" title (format-time-string "%Y-%m-%d")))
      (save-buffer))
    (setq gk-roam-pages (gk-roam--all-titles))
    file))

(defun gk-roam-update-index ()
  "Update gk-roam index page."
  (let* ((index-org (concat gk-roam-root-dir "index.org"))
         (index-buf (or (get-file-buffer index-org)
                        (find-file-noselect index-org))))
    (with-current-buffer index-buf
      (erase-buffer)
      (insert "#+TITLE: gk-roam\n#+OPTIONS: toc:nil H:2 num:0\n\n* Site Map\n\n")
      (dolist (page (gk-roam--all-pages))
	(insert (format " - [[file:%s][%s]]\n" page (gk-roam--get-title page))))
      (save-buffer))
    index-buf))

;;;; Commands
;;;###autoload
(defun gk-roam-find (&optional title)
  "Create a new gk-roam file or open an exist one in current window."
  (interactive)
  (let* ((title (or title (completing-read "New title or open an exist one: "
					   (gk-roam--all-titles) nil nil)))
	 (page (gk-roam--get-page title)))
    (if page
	(find-file (gk-roam--get-file page))
      (find-file (gk-roam-new title)))
    (gk-roam-update)))

;;;###autoload
(defun gk-roam-daily ()
  "Create or open gk-roam daily notes."
  (interactive)
  (let* ((title (format-time-string "%b %d, %Y")))
    (gk-roam-find title)))

;;;###autoload
(defun gk-roam-insert (&optional title)
  "Insert a gk-roam page"
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gk-roam-root-dir))
      (let* ((title (or title (completing-read
			       "Choose a page or create a new: "
			       (gk-roam--all-titles) nil nil
			       (thing-at-point 'word t))))
	     (page (gk-roam--get-page title)))
	(insert (gk-roam--format-link title))
	(save-buffer)
	(gk-roam-update-reference page))
    (message "Not in the gk-roam directory!")))

;;;###autoload
(defun gk-roam-new-at-point ()
  "Insert a file link and create a new file according to text at point."
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gk-roam-root-dir))
      (let* ((title (thing-at-point 'word t))
	     (page-exist-p (gk-roam--get-page title)))
	(if page-exist-p
	    (progn
	      (backward-word)
	      (kill-word 1)
	      (gk-roam-insert title)
	      (save-buffer))
	  (gk-roam-new title)
	  (backward-word)
	  (kill-word 1)
	  (gk-roam-insert title)
	  (gk-roam-find title)))
    (message "Not in the gk-roam directory!")))

;;;###autoload
(defun gk-roam-new-from-region ()
  "Insert a file link and create a new file according to a selected region."
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gk-roam-root-dir))
      (let* ((beg (region-beginning))
	     (end (region-end))
	     (title (when (region-active-p)
		      (buffer-substring-no-properties beg end)))
	     (page-exist-p (gk-roam--get-page title)))
	(if page-exist-p
	    (progn
	      (delete-region beg end)
	      (gk-roam-insert title)
	      (save-buffer))
	  (gk-roam-new title)
	  (delete-region beg end)
	  (gk-roam-insert title)
	  (gk-roam-find title)))
    (message "Not in the gk-roam directory!")))

;;;###autoload
(defun gk-roam-smart-new ()
  "Smartly create a new file according to point or region."
  (interactive)
  (cond
   ((region-active-p) (gk-roam-new-from-region))
   ((thing-at-point 'word) (gk-roam-new-at-point))
   (t (funcall-interactively 'gk-roam-find))))

;;;###autoload
(defun gk-roam-index ()
  "Show gk-roam index page."
  (interactive)
  (switch-to-buffer (gk-roam-update-index)))

;;;###autoload
(defun gk-roam-update ()
  "Update current gk-roam buffer's reference."
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gk-roam-root-dir))
      (gk-roam-update-reference (file-name-nondirectory (buffer-file-name)))
    (message "Not in the gk-roam directory!")))

;;;###autoload
(defun gk-roam-update-all () ;; have problem!
  "Update all gk-roam files' reference."
  (interactive)
  (gk-roam-update-index)
  (let ((pages (gk-roam--all-pages)))
    (mapcar #'gk-roam-update-reference pages)))

(defun gk-roam-resolve-link (orig-func file &rest args)
  "Convert gk-roam link to org link."
  (let ((file-buf (or (get-file-buffer file)
		      (find-file-noselect file))))
    (with-current-buffer file-buf
      (goto-char (point-min))
      (setq gk-roam-has-link-p nil)
      (while (re-search-forward gk-roam-link-regexp nil t)
	(setq gk-roam-has-link-p t)
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
		(insert (format "[[file:%s][#%s]]" (gk-roam--get-page title) title)))
	    (delete-region beg end)
	    (insert (format "[[file:%s][%s]]" (gk-roam--get-page title) title)))))
      (save-buffer)
      (apply orig-func file args)
      (when gk-roam-has-link-p
	;; if possible, use original undo function.
	(undo-tree-undo)))))

(defun gk-roam-set-project-alist ()
  "Add gk-roam project to 'org-publish-project-alist'"
  (add-to-list
   'org-publish-project-alist
   `("gk-roam"
     :base-extension "org"
     :recursive nil
     :base-directory ,(expand-file-name gk-roam-root-dir)
     :publishing-directory ,(expand-file-name gk-roam-pub-dir)
     :publishing-function org-html-publish-to-html
     :html-head ,gk-roam-pub-css)))

;;;###autoload
(defun gk-roam-publish-current-file ()
  "Publish current file."
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gk-roam-root-dir))
      (progn
	(gk-roam-update)
	(if undo-tree-mode
	    (org-publish-file (buffer-file-name))
	  (message "please enable 'undo-tree-mode' in this buffer!")))
    (message "Not in the gk-roam directory!")))

;;;###autoload
(defun gk-roam-preview-current ()
  "Preview current file."
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gk-roam-root-dir))
      (let ((current-file (concat (file-name-base (buffer-file-name)) ".html")))
        (httpd-serve-directory gk-roam-pub-dir)
        (unless (httpd-running-p) (httpd-start))
        (gk-roam-publish-current-file)
	(if undo-tree-mode
	    (browse-url (format "http://%s:%d/%s" "127.0.0.1" 8080 current-file))
	  (message "please enable 'undo-tree-mode' in this buffer!")))
    (message "Not in the gk-roam directory!")))

;;;###autoload
(defun gk-roam-publish-site (&optional FORCE ASYNC)
  "Publish gk-roam project to html page."
  (interactive)
  (gk-roam-update-index)
  ;; (gk-roam-update-all)
  (if global-undo-tree-mode
      (org-publish-project "gk-roam" FORCE ASYNC)
    (message "please enable 'global-undo-tree-mode'!")))

;;;###autoload
(defun gk-roam-preview ()
  "Preview gk-roam site."
  (interactive)
  (progn
    (httpd-serve-directory gk-roam-pub-dir)
    (unless (httpd-running-p) (httpd-start))
    (gk-roam-publish-site t nil)
    (if global-undo-tree-mode
	(browse-url (format "http://%s:%d" "127.0.0.1" 8080))
      (message "please enable 'global-undo-tree-mode'!"))))

(define-button-type 'gk-roam-link
  'action #'gk-roam-follow-link
  'title nil
  'follow-link t
  'help-echo "Jump to page")

(defun gk-roam-follow-link (button)
  "Jump to the page that BUTTON represents."
  (with-demoted-errors "Error when following the link: %s"
    (gk-roam-find (button-get button 'title))))

(defun gk-roam-link-fontify (beg end)
  "Put gk-roam link between BEG and END."
  (goto-char beg)
  (while (re-search-forward gk-roam-link-regexp end t)
    (make-text-button (match-beginning 0)
		      (match-end 0)
		      :type 'gk-roam-link
		      'face '(:underline nil)
		      ;; 'mouse-face '(:underline nil)
		      'title (match-string-no-properties 2))))

(defun gk-roam-hashtag-fontify(beg end)
  "Put gk-roam link between BEG and END."
  (goto-char beg)
  (while (re-search-forward gk-roam-hashtag-regexp end t)
    (make-text-button (match-beginning 0)
		      (match-end 0)
		      :type 'gk-roam-link
		      'face '(:underline nil)
		      'title (match-string-no-properties 2))))

(define-minor-mode gk-roam-link-minor-mode
  "Recognize gk-roam link."
  :lighter ""
  :keymap (make-sparse-keymap)
  (if gk-roam-link-minor-mode
      (progn
	(jit-lock-register #'gk-roam-hashtag-fontify)
	(jit-lock-register #'gk-roam-link-fontify))
    (jit-lock-unregister #'gk-roam-hashtag-fontify)
    (jit-lock-unregister #'gk-roam-link-fontify))
  (jit-lock-refontify))

;; -------------------------------------------------
;; gk-roam overlays

(defun gk-roam-overlay-region (beg end prop value)
  "Use overlays in region with property."
  (overlay-put (make-overlay beg end) prop value))

(defun gk-roam-overlay-hashtag ()
  "Overlay gk-roam hashtag."
  (with-silent-modifications
    (gk-roam-overlay-region (match-beginning 1) (match-beginning 2) 'display "")
    (gk-roam-overlay-region (match-beginning 3) (match-end 0) 'display "")
    (gk-roam-overlay-region (1- (match-beginning 0)) (match-end 0) 'face '(shadow (:underline nil)))))

(defun gk-roam-overlay-shadow-brackets ()
  "Set overlays to shadow brackets."
  (with-silent-modifications
    (remove-overlays (match-beginning 1) (match-beginning 2) 'display "")
    (remove-overlays (match-beginning 3) (match-end 0) 'display "")
    (gk-roam-overlay-region (match-beginning 1) (match-beginning 2) 'face 'shadow)
    (gk-roam-overlay-region (match-beginning 3) (match-end 0) 'face 'shadow)
    (gk-roam-overlay-region (match-beginning 0) (match-end 0) 'face '(warning (:underline nil)))))

(defun gk-roam-overlay-hide-brackets ()
  "Set overlays to hide gk-roam brackets."
  (with-silent-modifications
    (gk-roam-overlay-region (match-beginning 1) (match-beginning 2) 'display "")
    (gk-roam-overlay-region (match-beginning 3) (match-end 0) 'display "")
    (gk-roam-overlay-region (match-beginning 0) (match-end 0) 'face '(warning (:underline nil)))))

(defun gk-roam-put-overlays (beg &optional bound)
  "Put overlays between BEG and BOUND."
  (when (string= major-mode "gk-roam-mode")
    (let ((bound (or bound (point-max)))
	  pos)
      (save-excursion
	(goto-char beg)
	(while (re-search-forward gk-roam-link-regexp bound t)
	  (setq pos (point))
	  (goto-char (1- (match-beginning 0)))
	  (if (string= (thing-at-point 'char t) "#")
	      (gk-roam-overlay-hashtag)
	    (if gk-roam-toggle-brackets-p
		(gk-roam-overlay-shadow-brackets)
	      (gk-roam-overlay-hide-brackets)))
	  (goto-char pos))))))

(defun gk-roam-remove-overlays ()
  "Remove overlays in current line."
  (when (string= major-mode "gk-roam-mode")
    (save-excursion
      (goto-char (line-beginning-position))
      (when (re-search-forward gk-roam-link-regexp (line-end-position) t)
	(with-silent-modifications
	  (remove-overlays (line-beginning-position) (line-end-position)))))))

(defun gk-roam-overlay-buffer ()
  "Put overlay in currnt gk-roam buffer."
  (gk-roam-put-overlays (line-end-position) (point-max))
  (gk-roam-put-overlays (point-min) (line-beginning-position)))

(defun gk-roam-overlay1 (orig-fun &rest args)
  "Advice function for automatically hide and show brackets when cursor moves."
  (gk-roam-put-overlays (line-beginning-position) (line-end-position))
  (apply orig-fun args)
  (gk-roam-remove-overlays))

(defun gk-roam-overlay2 (orig-fun &rest args)
  "Advice function for automatically hide and show brackets when cursor moves."
  (gk-roam-put-overlays (line-beginning-position) (line-end-position))
  (apply orig-fun args)
  (unless (ignore-errors (mouse-on-link-p (point)))
    (gk-roam-remove-overlays)))

;;;###autoload
(defun gk-roam-toggle-brackets ()
  "Determine whether to show brackets in page link."
  (interactive)
  (if gk-roam-toggle-brackets-p
      (setq gk-roam-toggle-brackets-p nil)
    (setq gk-roam-toggle-brackets-p t))
  (gk-roam-overlay-buffer))

;; ----------------------------------------

;; major mode

(defun gk-roam-company-bracket-p ()
  "Judge if need to company bracket link."
  (save-excursion
    (let (word)
      (setq word (thing-at-point 'word t))
      (backward-word 1)
      (backward-char 2)
      (string= (thing-at-point 'sexp t)
	       (format "{[%s]}" word)))))

(defun gk-roam-company-hashtag-p ()
  "Judge if need to company hashtag link."
  (save-excursion
    (skip-chars-backward "^#" (line-beginning-position))
    (and (not (= (line-beginning-position) (point)))
	 (thing-at-point 'word t))))

(defun gk-roam--complete-hashtag ()
  "Complete hashtag with brackets."
  (when (gk-roam-company-hashtag-p)
    (save-excursion
      (let (end len)
	(setq end (point))
	(setq len (abs (skip-chars-backward "^#")))
	(insert "{[")
	(forward-char len)
	(insert "]}")))))

(defun gk-roam-completion-finish (title)
  "Function binded to `company-completion-finish-hook'."
  (when (gk-roam-company-hashtag-p)
    (gk-roam--complete-hashtag)
    (save-buffer)))

(defun gk-roam-completion-at-point ()
  "Function binded to `completion-at-point-functions'."
  (interactive)
  (when (or (gk-roam-company-bracket-p)
	    (gk-roam-company-hashtag-p))
    (let (bds start end)
      (if (gk-roam-company-bracket-p)
	  (progn
	    (setq bds (bounds-of-thing-at-point 'list))
            (setq start (1+ (car bds)))
	    (setq end (1- (cdr bds))))
	(setq bds (bounds-of-thing-at-point 'symbol))
	(setq start (car bds))
	(setq end (cdr bds)))
      (list start end gk-roam-pages . nil))))

(progn
  (setq gk-roam-mode-map (make-sparse-keymap)))

(defun gk-roam-set-major-mode ()
  (when (string=
	 (file-name-directory (buffer-file-name))
	 (expand-file-name gk-roam-root-dir))
    (gk-roam-mode)))

(add-hook 'find-file-hook #'gk-roam-set-major-mode)

(define-derived-mode gk-roam-mode org-mode "gk-roam"
  "Major mode for gk-roam."
  (add-hook 'completion-at-point-functions 'gk-roam-completion-at-point nil 'local)
  (add-hook 'company-completion-finished-hook 'gk-roam-completion-finish nil 'local)
  (add-hook 'gk-roam-mode-hook 'gk-roam-link-frame-setup)
  (add-hook 'gk-roam-mode-hook 'gk-roam-set-project-alist)
  (add-hook 'gk-roam-mode-hook 'toggle-truncate-lines)

  (advice-add 'org-publish-file :around #'gk-roam-resolve-link)
  
  ;; It's ugly to use 'advice-add', though things seem to go well.
  ;; But I haven't found a better way to auto hide and show brackets.
  (advice-add 'next-line :around #'gk-roam-overlay1)
  (advice-add 'previous-line :around #'gk-roam-overlay1)
  (advice-add 'newline :around #'gk-roam-overlay1)
  (advice-add 'org-delete-backward-char :around #'gk-roam-overlay1)
  (advice-add 'org-meta-return :around #'gk-roam-overlay1)
  (advice-add 'mouse-drag-region :around #'gk-roam-overlay2)
  (if (require 'hungry-delete nil t)
      (advice-add 'hungry-delete-backward :around #'gk-roam-overlay1))
  
  (gk-roam-link-minor-mode)
  (add-hook 'gk-roam-mode-hook 'gk-roam-overlay-buffer)
  
  (setq gk-roam-pages (gk-roam--all-titles))
  (setq-local gk-roam-has-link-p nil)
  
  (use-local-map gk-roam-mode-map))

;; ---------------------------------
(provide 'gk-roam)
;;; gk-roam.el ends here
