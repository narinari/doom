;;; config.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <narinari@ip-172-30-32-50>
;; Keywords: vc,

(defun tmux/send-keys-cmd (args &optional nonenter)
  (concat
   (format "send-keys %s" (shell-quote-argument args))
   (unless nonenter " Enter")))

(defun gh+tmux/pr-create ()
  "Create PR using gh CLI"
  (interactive)
  (let ((up-repo-name (gh/repository-name "upstream")))
    (+tmux
     ;; (format "split-window -v -l 15 'zsh -c \"%s && %s\"'"
     ;;         (concat "cd " (doom-project-root))
     ;;         (concat "gh pr create" (when up-repo-name (concat " -R " up-repo-name)))))))
     (format "split-window -v -l 15 \\; %s \\; %s"
             (tmux/send-keys-cmd (concat "cd " (doom-project-root)))
             (tmux/send-keys-cmd (concat "gh pr create" (when up-repo-name (concat " -R " up-repo-name))))
             ))))

(defun gh:reporitory-name (remote-name)
  (interactive "sRemote name: ")
  (message (gh/repository-name remote-name)))

(defun gh/repository-name (remote-name)
  (if-let ((url (gh/git-command "remote" "get-url" remote-name)))
      (gh/parse-repository-name url)))

(defun gh/parse-repository-name (url)
  (let* ((path (last (split-string url "/") 2))
         (org (car path))
         (repo (cadr path)))
    (concat org "/" (file-name-sans-extension repo))))

(defun gh/git-remote-list ()
  (if-let* ((lines (gh/git-command "remote")))
      (cl-loop for line in (split-string lines "\n" t)
               collect line)))

(defun gh/command-log-buffer ()
  (get-buffer-create "*gh/git log*"))

(defun gh/run-command (&rest args)
  (let ((cmd (mapconcat #'shell-quote-argument args " ")))
    (with-temp-buffer
      (let ((temp-buffer (current-buffer))
            (ret (call-process-shell-command cmd nil t)))
        (with-current-buffer (gh/command-log-buffer)
          (insert-buffer temp-buffer))
        (when (= 0 ret) (buffer-string))))))

(defun gh/git-command (&rest args)
  (apply #'gh/run-command "git" args))

(defun gh/gh-command (&rest args)
  (apply #'gh/run-command "gh" args))

(defun gh/refresh ()
  (when (featurep 'magit) (magit-refresh-all))
  (when (and (featurep 'treemacs) (eq 'visible (treemacs-current-visibility))) (treemacs-refresh))
  (when (featurep 'projectile) (projectile-invalidate-cache nil)))

(defun gh/checkout-pr (pr)
  (gh/gh-command "pr" "checkout" pr)
  (gh/refresh)
  )

(defun gh:checkout-pr (pr)
  (interactive "sPull request no: ")
  (gh/checkout-pr pr)
  (minibuffer-message "checkout pull requst."))
