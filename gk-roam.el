;;; gk-roam.el --- Geekinney's RoamResearch in orgmode. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: roam org
;; Author: Kinney Zhang <kinneyzhang666 AT gmail DOT com>
;; URL: http://github.com/Kinneyzhang/gk-roam
;; Package-Requires: ((emacs "26.3"))

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

;;; Commentary: gk-roam is a light-weight RoamResearch.

;; 

;;; Code:

(require 'simple-httpd)

(defvar gk-roam-root-dir "/Users/kinney/gk-roam/org/"
  "Gk-roam's root directory, with org files in it.")

(defvar gk-roam-pub-dir "/Users/kinney/gk-roam/html/"
  "Gk-roam's publish directory, with html files in it.")

(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
	(vm-imap . vm-visit-imap-folder-other-frame)
	(gnus . org-gnus-no-new-news)
	(file . find-file)
	(wl . wl-other-frame)))

(defun gk-roam--get-date (file)
  "Get gk-roam date number in filename."
  (substring (file-name-base file) 0 14))

(defun gk-roam--slugify-title (title)
  "Slugify gk-roam file title."
  (string-join (split-string title "[ ]+") "-"))

(defun gk-roam--slugify-title-reversed (slug)
  "Slugify gk-roam file title in reverse."
  (string-join (split-string slug "-") " "))

(defun gk-roam--get-title (filename)
  "Get the title of file named FILENAME."
  (gk-roam--slugify-title-reversed (string-trim filename "^[0-9]+-" "\\.org")))

(defun gk-roam--all-titles ()
  "Get all gk-roam titles"
  (let* ((filenames (gk-roam--all-files nil))
	 (titles (mapcar (lambda (x) (gk-roam--get-title x)) filenames)))
    titles))

(defun gk-roam--gen-file (title)
  "Generate new gk-roam file string from TITLE."
  (concat gk-roam-root-dir
	  (format "%s-%s.org" (format-time-string "%Y%m%d%H%M%S")
		  (gk-roam--slugify-title title))))

(defun gk-roam--get-file (title)
  "Get an exisit gk-roam file string from TITLE, none return nil."
  (let ((pair (assoc title (gk-roam--all-link-pairs))))
    (when pair
      (concat gk-roam-root-dir (cdr pair)))))

(defun gk-roam--all-files (full)
  "Get all gk-roam files path."
  (directory-files gk-roam-root-dir full (rx bol (+ (in num)) "-" (+ anychar) ".org" eol)))

(defun gk-roam--all-link-pairs ()
  "Get all gk-roam link description pairs, sorted by predicate."
  (let* ((filenames (gk-roam--all-files nil)) ;; 排除index.org
	 (titles (gk-roam--all-titles))
	 pair pairs)
    (dotimes (i (length filenames))
      (setq pair (cons (nth i titles) (nth i filenames)))
      (setq pairs (push pair pairs)))
    pairs))

(defsubst gk-roam--format-link (file)
  "Format FILE into a gk-roam file link.."
  (let ((file (file-name-nondirectory file)))
    (format "[[file:%s][%s]]" file
	    (gk-roam--slugify-title-reversed (gk-roam--get-title file)))))

(defun gk-roam--search-linked-files (file process callback)
  "Call CALLBACK with a list of files’ name that has a link to FILE."
  ;; (gk-roam-delete-reference file)
  (let* ((name (generate-new-buffer-name (format " *%s*" process)))
         (process (start-process
                   name name "rg" "-F" ;; 排除临时文件和index.org等
		   (gk-roam--format-link file)
		   (file-name-directory file)
		   "-g" "!index.org*"))
         ;; When the grep process finishes, we parse the result files
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

(defun gk-roam-heading-of-line (line file)
  "Get the org heading of specific LINE in FILE."
  (let ((line (if (stringp line) (string-to-number line) line))
	headline)
    (with-temp-buffer
      (goto-char (point-min))
      (insert-file-contents file)
      (goto-line line)
      (org-mode)
      (setq headline (org-get-heading t t t t)))
    headline))

(defun gk-roam--format-backlink (line file)
  "Format gk-roam backlink of specific LINE in FILE."
  (let* ((filename (file-name-nondirectory file))
	 (heading (gk-roam-heading-of-line line file))
	 (text (gk-roam--get-title filename)))
    (if (null heading)
	(format "[[file:%s][%s/top]]" filename text)
      (format "[[file:%s::*%s][%s/%s]]" filename heading text heading))))

(defun gk-roam--process-reference (text file)
  "Remove the link of current file in reference's text."
  (with-temp-buffer
    (insert (string-trim text "\\** +" nil))
    (goto-char (point-min))
    (while (re-search-forward "\\(\\[\\[file:.+?\\]\\[\\|\\]\\]\\)+?" nil t)
      (replace-match "*"))
    (buffer-string)))

(defun gk-roam-update-reference (file)
  "Update gk-roam file reference."
  (interactive)
  (unless (executable-find "rg")
    (user-error "Displaying reference needs rg but we cannot find it"))
  (gk-roam--search-linked-files
   file "gk-roam-rg"
   (lambda (results)
     (let* ((file-buf (or (get-file-buffer file)
			  (find-file-noselect file nil nil))))
       (with-current-buffer file-buf
	 (save-excursion
	   (goto-char (point-min))
	   (unless (re-search-forward "-----\n" nil t)
	     (goto-char (point-max))
	     (insert "\n-----\n"))
	   (delete-region (point) (point-max))
	   (let ((num 0))
	     (if (null results)
		 (progn
		   (insert "/*No Linked Reference*/")
		   (save-buffer))
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
					 nil)
			    file))
		     (insert
		      (concat "\n-- "
			      (gk-roam--format-backlink line res-file)
			      "\\\\\n" text))
		     (insert "\n"))))
	       (goto-char (point-min))
	       (re-search-forward "-----\n" nil t)
	       (insert (format "/*%d Linked Reference:*/\n" num))
	       (save-buffer))))))))
  (message "%s reference updated" (file-name-nondirectory file)))
;; -----------------------------------------------------

;;;###autoload
(defun gk-roam-find (&optional date title tags)
  "Create a new gk-roam file or open an exist one."
  (interactive)
  (let ((title (if title title
		 (completing-read "New title or open an exisit one: "
				  (gk-roam--all-titles) nil nil)))
	date tags beg)
    (if (member title (gk-roam--all-titles))
	(progn
	  (find-file (gk-roam--get-file title))
	  (save-buffer))
      (setq date (if date date (format-time-string "%Y%m%d%H%M%S")))
      (setq tags (if tags tags
		  (completing-read "New tags: " nil nil nil)))
      (find-file (concat gk-roam-root-dir (format "%s-%s.org" date (gk-roam--slugify-title title))))
      (insert
       (format "#+TITLE: %s\n#+DATE: %s\n#+OPTIONS: toc:nil H:2 num:0\n#+TAGS: %s\n» [[file:index.org][ /Gk-Roam/ ]]\n\n" title (format-time-string "%Y-%m-%d") tags))
      (setq beg (point))
      (insert "\n\n-----\n/*No Linked Reference*/")
      (goto-char beg)
      (save-buffer))))

;;;###autoload
(defun gk-roam-new-at-point ()
  "Create a new file according to text at point."
  (interactive)
  (let* ((title (thing-at-point 'word))
	 (file (or (gk-roam--get-file title)
		   (gk-roam--gen-file title)))
	 date)
    (backward-word)
    (kill-word 1)
    (insert (gk-roam--format-link file))
    (save-buffer)
    (if (member title (gk-roam--all-titles))
	(gk-roam-find nil title)
      (setq date (gk-roam--get-date file))
      (gk-roam-find date title))
    (gk-roam-update)))

;;;###autoload
(defun gk-roam-new-from-region ()
  "Create a new file according to a selected region"
  (interactive)
  (let* ((beg (region-beginning))
	 (end (region-end))
	 (title (when (region-active-p)
		  (buffer-substring-no-properties beg end)))
	 (file (or (gk-roam--get-file title)
		   (gk-roam--gen-file title)))
	 date)
    (delete-region beg end)
    (insert (gk-roam--format-link file))
    (save-buffer)
    (if (member title (gk-roam--all-titles))
	(gk-roam-find nil title)
      (setq date (gk-roam--get-date file))
      (gk-roam-find date title))
    (gk-roam-update)))

;;;###autoload
(defun gk-roam-smart-new ()
  "Smartly create a new file according to point or region."
  (interactive)
  (cond
   ((region-active-p) (gk-roam-new-from-region))
   ((thing-at-point 'word) (gk-roam-new-at-point))
   (t (gk-roam-find))))

;;;###autoload
(defun gk-roam-index ()
  "Show gk-roam index page."
  (interactive)
  (let* ((index-org (concat gk-roam-root-dir "index.org"))
	 (index-buf (or (get-file-buffer index-org)
			(find-file-noselect index-org))))
    (with-current-buffer index-buf
      (erase-buffer)
      (insert "#+TITLE: gk-roam\n#+OPTIONS: toc:nil H:2 num:0\n\n* Site Map\n\n")
      (dolist (pair (gk-roam--all-link-pairs))
	(insert (format " - [[file:%s][%s]]\n" (cdr pair) (car pair))))
      (save-buffer))
    (switch-to-buffer index-buf)))

;;;###autoload
(defun gk-roam-insert ()
  "Insert a gk-roam file at point"
  (interactive)
  (let* ((filenames (gk-roam--all-files nil))
	 (title (completing-read "Choose a file: " (mapcar #'gk-roam--get-title filenames) nil t))
	 (file (gk-roam--get-file title))
	 (file-buf (or (get-file-buffer file)
		       (find-file-noselect file))))
    (insert (gk-roam--format-link file))
    (save-buffer)
    (with-current-buffer file-buf
      (gk-roam-update))))

;;;###autoload
(defun gk-roam-update ()
  "Update current gk-roam buffer reference."
  (interactive)
  (gk-roam-update-reference (buffer-file-name)))

;;;###autoload
(defun gk-roam-update-all ()
  "Update all gk-roam files' reference."
  (interactive)
  (let ((files (gk-roam--all-files t)))
    (dolist (file files)
      (gk-roam-update-reference file))))

;;;###autoload
(defun gk-roam-publish-current-file ()
  "Publish current file."
  (interactive)
  (org-publish-file (buffer-file-name)))

;;;###autoload
(defun gk-roam-preview-current ()
  "Preview current file."
  (interactive)
  (let ((current-file (concat (file-name-base (buffer-file-name)) ".html")))
    (httpd-serve-directory gk-roam-pub-dir)
    (unless (httpd-running-p) (httpd-start))
    (gk-roam-publish-current-file)
    (browse-url (format "http://%s:%d/%s" "127.0.0.1" 8080 current-file))))

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
     :html-head "<link rel=\"stylesheet\" href=\"https://gongzhitaao.org/orgcss/org.css\">"))
  (org-publish-project "gk-roam"))

;;;###autoload
(defun gk-roam-preview ()
  "Preview gk-roam site."
  (interactive)
  (httpd-serve-directory gk-roam-pub-dir)
  (unless (httpd-running-p) (httpd-start))
  (gk-roam-publish-site)
  (browse-url (format "http://%s:%d" "127.0.0.1" 8080)))

(provide 'gk-roam)
;;; gk-roam.el ends here
