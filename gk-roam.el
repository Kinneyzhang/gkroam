;;; gk-roam.el --- A light-weight roam replica. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 1.0
;; Keywords: roam org
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: http://github.com/Kinneyzhang/gk-roam
;; Package-Requires: ((emacs "27.1"))

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

;; Gk-roam is a light-weight roam repica, built on top of emacs OrgMode.

;;; Code:

(require 'simple-httpd)

(defvar gk-roam-root-dir ""
  "Gk-roam's root directory, with org files in it.")

(defvar gk-roam-pub-dir ""
  "Gk-roam's publish directory, with html files in it.")

(defvar gk-roam-pub-css "<link rel=\"stylesheet\" href=\"https://gongzhitaao.org/orgcss/org.css\">"
  "Gk-roam publish css link.")

(defvar gk-roam-temp-file (concat user-emacs-directory "gk-roam/temp")
  "Gk-roam temporary file.")

(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
	(vm-imap . vm-visit-imap-folder-other-frame)
	(gnus . org-gnus-no-new-news)
	(file . find-file)
	(wl . wl-other-frame)))

(setq org-hide-emphasis-markers t)

(defun gk-roam--slugify-title (title)
  "Slugify gk-roam file title."
  (string-join (split-string title "[ ]+") "-"))

(defun gk-roam--slugify-title-reversed (slug)
  "Slugify gk-roam file title in reverse."
  (string-join (split-string slug "-") " "))

;; -------------------------------
(defun gk-roam--get-title (page)
  "Get page title."
  (with-temp-buffer
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
;; --------------

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

(defsubst gk-roam--format-link (page)
  "Format PAGE into a gk-roam page link.."
  (format "[[file:%s][%s]]" page (gk-roam--get-title page)))

(defun gk-roam--search-linked-pages (page callback)
  "Call CALLBACK with a list of files’ name that has a link to PAGE."
  (let* ((name (generate-new-buffer-name " *gk-roam-rg*"))
         (process (start-process
                   name name "rg" "-Fn" "--heading"
		   (gk-roam--format-link page)
		   (expand-file-name gk-roam-root-dir) ;; must be absolute path.
		   "-g" "!index.org*" "-j" "10"))
         ;; When the rg process finishes, we parse the result files
         ;; and call CALLBACK with them.
         (sentinal
          (lambda (process event)
            (if (string-match-p (rx (or "finished" "exited"))
                                event)
                (if-let ((buf (process-buffer process)))
                    (with-current-buffer buf
                      (let ((results (split-string (buffer-string) "\n\n")))
                        (funcall callback (remove "" results))))
                  (error "Gk-roam’s rg process’ buffer is killed"))
              (error "Gk-roam’s rg process failed with signal: %s"
                     event)))))
    (set-process-sentinel process sentinal)))

(defun gk-roam-heading-of-line (line page)
  "Get the org heading of specific LINE in FILE."
  (let ((line (if (stringp line) (string-to-number line) line))
	heading)
    (with-temp-buffer
      (goto-char (point-min))
      (insert-file-contents (gk-roam--get-file page))
      (goto-line line)
      (org-mode)
      (setq heading (org-get-heading t t t t)))
    heading))

(defun gk-roam--format-backlink (line page)
  "Format gk-roam backlink of specific LINE in PAGE."
  (let* ((heading (gk-roam-heading-of-line line page))
	 (title (gk-roam--get-title page)))
    (if (null heading)
	(format "[[file:%s][%s/top]]" page title)
      (format "[[file:%s::*%s][%s/%s]]" page heading title heading))))

(defun gk-roam--process-reference (text)
  "Remove links in reference's text."
  (with-temp-buffer
    (insert (string-trim text "\\** +" nil))
    (goto-char (point-min))
    (while (re-search-forward "\\(\\[\\[file:.+?\\]\\[\\|\\]\\]\\)+?" nil t)
      (replace-match "_"))
    (concat " * " (buffer-string))))

(defun gk-roam-update-index ()
  "Update gk-roam index page."
  (let* ((index-org (concat gk-roam-root-dir "index.org"))
	 (index-buf (or (get-file-buffer index-org)
			(find-file-noselect index-org))))
    (with-current-buffer index-buf
      (erase-buffer)
      (gk-roam-mode)
      (insert "#+TITLE: gk-roam\n#+OPTIONS: toc:nil H:2 num:0\n\n* Site Map\n\n")
      (dolist (page (gk-roam--all-pages))
	(insert (format " - [[file:%s][%s]]\n" page (gk-roam--get-title page))))
      (save-buffer))
    index-buf))

(defun gk-roam-update-reference (page)
  "Update gk-roam file reference."
  (unless (executable-find "rg")
    (user-error "Cannot find program rg"))
  (gk-roam--search-linked-pages
   page
   (lambda (results)
     (let* ((file (gk-roam--get-file page))
	    (file-buf (or (get-file-buffer file)
			  (find-file-noselect file nil nil))))
       (with-current-buffer file-buf
	 (save-excursion
	   (goto-char (point-max))
	   (re-search-backward "\n-----\n" nil t)
	   (delete-region (point) (point-max))
	   (let ((num 0))
	     (when results
	       (insert "\n-----\n")
	       (dolist (res results)
		 (let* ((res-list (split-string res "\n" t "[ \n]+"))
			(res-file (car res-list))
			line text)
		   (pop res-list) ;; return first elem!
		   (setq num (+ num (length res-list)))
		   (dolist (item res-list)
		     (setq line (when (string-match "[0-9]+" item)
				  (match-string 0 item)))
		     (setq text
			   (gk-roam--process-reference
			    (string-trim item
					 (when (string-match "[0-9]+: *" item)
					   (match-string 0 item))
					 nil)))
		     (insert
		      (concat "\n"
			      (gk-roam--format-backlink line
							(file-name-nondirectory res-file))
			      "\n" text))
		     (insert "\n"))))
	       (goto-char (point-min))
	       (re-search-forward "-----\n" nil t)
	       (insert (format "~%d Linked References to \"%s\"~\n" num (gk-roam--get-title page)))
	       (save-buffer))))))))
  (message "%s reference updated" page))

;; -----------------------------------------------------
;;;###autoload
(defun gk-roam-new (title)
  "Just create a new gk-roam file."
  (let* ((file (gk-roam--gen-file))
	 (file-buf (find-file-noselect file))
	 beg)
    (with-current-buffer file-buf
      (gk-roam-mode)
      (insert
       (format "#+TITLE: %s\n#+DATE: %s\n#+OPTIONS: toc:nil H:2 num:0\n» [[file:index.org][ /Gk-Roam/ ]]\n\n" title (format-time-string "%Y-%m-%d")))
      (save-buffer))
    (setq gk-roam-pages (gk-roam--all-titles))
    file))

;;;###autoload
(defun gk-roam-find (&optional title)
  "Create a new gk-roam file or open an exist one."
  (interactive)
  (let* ((title (if title title
		  (completing-read "New title or open an exist one: "
				   (gk-roam--all-titles) nil nil)))
	 (page-exist-p (gk-roam--get-page title)))
    (if page-exist-p
	(progn
	  (find-file (gk-roam--get-file page-exist-p))
	  (gk-roam-mode))
      (find-file (gk-roam-new title))
      (gk-roam-mode))))

;;;###autoload
(defun gk-roam-daily ()
  "Create or open gk-roam daily notes."
  (interactive)
  (let* ((title (format-time-string "%b %d, %Y")))
    (gk-roam-find title)))

;;;###autoload
(defun gk-roam-new-at-point ()
  "Insert a file link and create a new file according to text at point."
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gk-roam-root-dir))
      (let* ((title (thing-at-point 'word))
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
	  (save-buffer)
	  (gk-roam-find title))
	(gk-roam-update-reference page-exist-p))
    (message "Not in the gk-roam directory!")))

;;;###autoload
(defun gk-roam-new-from-region ()
  "Insert a file link and create a new file according to a selected region"
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
	  (save-buffer)
	  (gk-roam-find title))
	(gk-roam-update-reference page-exist-p))
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
(defun gk-roam-insert (&optional title)
  "Insert a gk-roam file at point"
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gk-roam-root-dir))
      (let* ((title (if title title (completing-read "Choose a file: " (gk-roam--all-titles) nil t)))
	     (page (gk-roam--get-page title)))
	(insert (gk-roam--format-link page))
	(save-buffer)
	(gk-roam-update-reference page))
    (message "Not in the gk-roam directory!")))

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

;;;###autoload
(defun gk-roam-publish-current-file ()
  "Publish current file."
  (interactive)
  (if (string= (file-name-directory (buffer-file-name))
	       (expand-file-name gk-roam-root-dir))
      (progn
	(gk-roam-update)
	(org-publish-file (buffer-file-name)))
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
	(browse-url (format "http://%s:%d/%s" "127.0.0.1" 8080 current-file)))
    (message "Not in the gk-roam directory!")))

;;;###autoload
(defun gk-roam-publish-site ()
  "Publish gk-roam project to html page."
  (interactive)
  (defvar org-publish-project-alist nil)
  (add-to-list
   'org-publish-project-alist
   `("gk-roam"
     :base-extension "org"
     :recursive nil
     :base-directory ,gk-roam-root-dir
     :publishing-directory ,gk-roam-pub-dir
     :publishing-function org-html-publish-to-html
     :html-head ,gk-roam-pub-css))
  ;; (gk-roam-update-all)
  (org-publish-project "gk-roam"))

;;;###autoload
(defun gk-roam-preview ()
  "Preview gk-roam site."
  (interactive)
  (httpd-serve-directory gk-roam-pub-dir)
  (unless (httpd-running-p) (httpd-start))
  (gk-roam-publish-site)
  (browse-url (format "http://%s:%d" "127.0.0.1" 8080)))

;; ----------------------------------------
(defvar gk-roam-pages (gk-roam--all-titles)
  "Page candidates for completion.")

(defvar gk-roam-mode-map nil
  "Keymap for `gk-roam-mode'")

(progn
  (setq gk-roam-mode-map (make-sparse-keymap)))

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

(defun gk-roam--complete-hashtag (arg)
  "Complete hashtag with brackets."
  (when (gk-roam-company-hashtag-p)
    (save-excursion
      (let (end len)
	(setq end (point))
	(setq len (abs (skip-chars-backward "^#")))
	(insert "{[")
	(forward-char len)
	(insert "]}")))))

(defun gk-roam-completion-at-point ()
  "This is the function to be used for the hook `completion-at-point-functions'."
  (interactive)
  (let* ((bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (when (or (gk-roam-company-bracket-p)
	      (gk-roam-company-hashtag-p))
      (list start end gk-roam-pages . nil))))

(define-derived-mode gk-roam-mode org-mode "gk-roam"
  "Major mode for gk-roam."
  (add-hook 'completion-at-point-functions 'gk-roam-completion-at-point nil 'local)
  (add-hook 'company-after-completion-hook 'gk-roam--complete-hashtag)
  (use-local-map gk-roam-mode-map))

(provide 'gk-roam)
;;; gk-roam.el ends here
