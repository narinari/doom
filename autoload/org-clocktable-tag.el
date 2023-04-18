;; autolooad/org-clocktable-tag.el -*- lexical-binding: t; -*-

(defun clocktable-by-tag/shift-cell (n)
  (let ((str ""))
    (dotimes (_ n)
      (setq str (concat str "| ")))
    str))

(defun clocktable-by-tag/insert-tag (params)
  (let ((tag (plist-get params :tags)))
    (insert "|--\n")
    (insert (format "| %s | *Tag time* |\n" tag))
    (let ((total 0))
         (let ((clock-data (org-clock-get-table-data (current-buffer) params)))
           (when (> (nth 1 clock-data) 0)
             (setq total (+ total (nth 1 clock-data)))
             ;; (insert (format "| | File *%s* | %.2f |\n"
             ;;         (file-name-nondirectory file)
             ;;         (/ (nth 1 clock-data) 60.0)))
             (dolist (entry (nth 2 clock-data))
               (insert (format "| | . %s%s | %s %.2f |\n"
                       (org-clocktable-indent-string (nth 0 entry))
                       (nth 1 entry)
                       (clocktable-by-tag/shift-cell (nth 0 entry))
                       (/ (nth 4 entry) 60.0))))))
    (save-excursion
      (re-search-backward "*Tag time*")
      (org-table-next-field)
      (org-table-blank-field)
      (insert (format "*%.2f*" (/ total 60.0))))
      (org-table-align))))

(defun clocktable-weekly-tag/insert-tags-row (weeknumber tags sub-total)
  (insert "|--\n")
  (insert (format "| W%s | Tag | *Tag time* |\n" weeknumber))
  (dolist (tag tags)
    (insert (format "| | %s | %.2f |\n" tag (/ (gethash tag sub-total) 60.0)))
    (puthash tag 0 sub-total))
  (insert (format "| | %s | %.2f |\n" "total" (/ (gethash "total" sub-total) 60.0)))
  (puthash "total" 0 sub-total))

(defun clocktable-weekly-tag/insert-tag (clock-data params)
  (let ((tags (plist-get params :tags))
        (sub-total (make-hash-table :test 'equal))
        (wn "0"))
    (dolist (tag tags)
      (puthash tag 0 sub-total)
      (puthash "total" 0 sub-total))
    (dolist (entry clock-data)
      (let ((level (nth 0 entry))
            (entry-tags (nth 2 entry))
            (time (nth 4 entry)))
        (when (eq 1 level)
          (let* ((day (date-to-time (concat (nth 1 entry) " 00:00:00")))
                 (cwn (format-time-string "%U" day)))
            (unless (equal wn cwn)
              (unless (equal wn "0")
                (clocktable-weekly-tag/insert-tags-row wn tags sub-total))
              (setq wn cwn))))
        (when (eq 2 level)
          (dolist (tag tags)
            (when (member tag entry-tags)
              (puthash tag (+ time (gethash tag sub-total)) sub-total)))
          (puthash "total" (+ time (gethash "total" sub-total)) sub-total))))
    (clocktable-weekly-tag/insert-tags-row wn tags sub-total)
    (save-excursion
      (re-search-backward "*Tag time*")
      (org-table-next-field)
      (org-table-blank-field)
      (org-table-align))))

(defun org-dblock-write:clocktable-by-tag (params)
  (insert "| Week | Headline | Time (h) |\n")
  (insert "|      |          | <r>  |\n")
  (let ((clock-data (org-clock-get-table-data (current-buffer) params)))
    (when (> (nth 1 clock-data) 0)
      (clocktable-weekly-tag/insert-tag (nth 2 clock-data) params))))

(defun formatted-copy ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t)))
      (kill-new
       (with-output-to-string
         (with-current-buffer buf
           (shell-command-on-region
            (point-min)
            (point-max)
            "pandoc -s -f html -t rtf"
            standard-output))))
      (kill-buffer buf))))

(provide 'org-clocktable-tag)
