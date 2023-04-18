;;;###autoload
(defun *ivy--switch-buffer-action (buffer)
  "Switch to BUFFER.
BUFFER may be a string or nil."
  (with-ivy-window
    (if (zerop (length buffer))
        (display-buffer
         ivy-text nil 'force-same-window)
      (let ((virtual (assoc buffer ivy--virtual-buffers))
            (view (assoc buffer ivy-views)))
        (cond ((and virtual
                    (not (get-buffer buffer)))
               (find-file (cdr virtual)))
              (view
               (delete-other-windows)
               (let (
                     ;; silence "Directory has changed on disk"
                     (inhibit-message t))
                 (ivy-set-view-recur (cadr view))))
              (t
               (display-buffer
                buffer nil 'force-same-window)))))))


;;;###autoload
(defun +ivy/helpful-function (prompt)
  (helpful-function (intern prompt)))

