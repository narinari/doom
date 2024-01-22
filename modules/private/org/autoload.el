;;;###autoload
(defun nari-org-insert-weblink ()
  (interactive)
  (let* ((pair (string-split "\n" (with-temp-buffer (clipboard-yank) (buffer-string))))
         (desc (first pair))
         (link (second pair)))
    (message pair)
    (insert (org-link-make-string link desc))))
