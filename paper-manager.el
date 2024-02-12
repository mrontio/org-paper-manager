(require 'org)

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
        (y-or-n-p (format "Download \"%s\" is %d minutes old, continue? " file-path minutes-since-download))
      't)))

(defun ask-to-store-link-p (entry-name)
  (y-or-n-p "Store link for %s?" entry-name))

(defun mod-time-lesser-p (a b)
  (let ((a-time (file-attribute-modification-time (cdr a)))
        (b-time (file-attribute-modification-time (cdr b))))
    (time-less-p b-time a-time)))


(defun get-org-headline-level-at-mark (mark)
  "Get the headline level (number of *) of the org headline at mark."
  (let ((current-headline)
        (pos 0))
    (save-excursion
      (goto-char mark)
      (setq current-headline (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      (while (string-match "*" current-headline pos)
        (setq pos (+ 1 pos)))
      (if (and (> pos 0) (char-equal (aref current-headline pos) ? ))
          pos
        'nil))))

(defun pull-paper ()
  (interactive)
  (let* ((last-downloaded-file-and-attr
          (car (sort (directory-files-and-attributes paper-download-path nil ".*\.pdf" 't) 'mod-time-lesser-p)))
         (file-name (car last-downloaded-file-and-attr))
         (entry-name (replace-regexp-in-string "_+" " " (string-trim-right file-name "\.pdf")))
         (file-mod-time (file-attribute-modification-time (cdr last-downloaded-file-and-attr)))
         (paper-index-marker (org-find-exact-headline-in-buffer paper-org-headline (current-buffer)))
         (org-paper-index-headline-level)
         (current-paper-headline-mark)
         (store-link))
    (when (resolve-file-timeline-p file-name file-mod-time)
      (unless (file-directory-p paper-storage-path)
        (make-directory paper-storage-path))
      (setq store-link (y-or-n-p (format "Store link for %s?" entry-name)))
      (rename-file (concat paper-download-path file-name) (concat paper-storage-path file-name))
      (save-excursion
        (org-goto-marker-or-bmk paper-index-marker)
        (org-narrow-to-subtree)
        (setq org-paper-index-headline-level (get-org-headline-level-at-mark paper-index-marker))
        (goto-char (point-max))
        (newline)
        (insert-char ?* org-paper-index-headline-level)
        (insert-char ? )
        (org-demote-subtree)
        (insert (format "TODO %s" entry-name))
        (setq current-paper-headline-mark (point))
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
        (goto-char current-paper-headline-mark)
        (org-fold-subtree t)
        (when store-link
          (org-store-link nil t))
        (widen)))))
