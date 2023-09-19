(defvar paper-download-path
  "~/Downloads/"
  "Path to downloads folder for browser")

(defvar paper-storage-path
  "./papers/"
  "Path to paper storage")

(defvar paper-warning-period-minutes
  5
  "Minutes until pull-paper starts complaining that the file is too old")

(defvar paper-org-headline
  "Paper Index"
  "Index of heading under which to insert new entries")


(defun resolve-file-timeline-p (file-path file-mod-time)
  (let ((minutes-since-download (/ (time-convert (time-since file-mod-time) 'integer) 60)))
    (if (> minutes-since-download paper-warning-period-minutes)
        (yes-or-no-p (format "Download \"%s\" is %d minutes old, continue? " file-path minutes-since-download))
      't)))


(defun mod-time-lesser-p (a b)
  (let ((a-time (file-attribute-modification-time (cdr a)))
        (b-time (file-attribute-modification-time (cdr b))))
    (time-less-p b-time a-time)))

(defun pull-paper ()
  (interactive)
   (let* ((last-downloaded-file-and-attr
           (car (sort (directory-files-and-attributes paper-download-path nil ".*\.pdf" 't) 'mod-time-lesser-p)))
         (file-name (car last-downloaded-file-and-attr))
         (file-mod-time (file-attribute-modification-time (cdr last-downloaded-file-and-attr)))
         (paper-index-marker (org-find-exact-headline-in-buffer paper-org-headline (current-buffer))))
     (when (resolve-file-timeline-p file-name file-mod-time)
       (rename-file (concat paper-download-path file-name) (concat paper-storage-path file-name))
       (save-excursion
         (org-goto-marker-or-bmk paper-index-marker)
         (org-narrow-to-subtree)
         (org-fold-hide-subtree)
         (org-insert-heading-after-current)
         (org-demote-subtree)
         (insert (format "TODO %s"
                         (replace-regexp-in-string "_+" " "
                          (string-trim-right file-name "\.pdf"))))
         (newline)
         (insert "- ")
         (org-insert-link nil (concat "file:" paper-storage-path file-name) "Link")
         (org-insert-subheading nil)
         (insert "Citation")
         (newline)
         (insert "#+BEGIN_SRC latex")
         (newline)
         (insert "#+END_SRC")
         (org-insert-heading)
         (insert "Notes")
         (widen)))))
