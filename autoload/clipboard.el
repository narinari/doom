;;; ~/.doom.d/autoload/clipboard.el -*- lexical-binding: t; -*-

;;;###autoload
(defun paste-to-tmux (text)
  (let* ((process-connection-type nil)
         (tmux (start-process "paste-to-tmux" "tmux-output" "tmux" "load-buffer" "-"))
         (name (process-name tmux)))
    (process-send-string name text)
    (process-send-eof name)))

;; using with interprogram-cut-function in config.el
;;
;; example
;; (when (getenv "TMUX")
;;   (setq interprogram-cut-function 'paste-to-pbcopy))
;;
;;;###autoload
(defun paste-to-pbcopy (text)
  (let* ((process-connection-type nil)
         (pbcopy (apply #'start-process
                        (append
                         (list "paste-to-pbcopy" "pbcopy-output" "nc")
                         (append (unless (eq system-type 'darwin) '("-N")) '("localhost" "52224")))))
         (name (process-name pbcopy)))
    (process-send-string name text)
    (process-send-eof name)))
