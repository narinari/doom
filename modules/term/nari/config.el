(use-package! with-shell-interpreter)

(defun my/zsh-local ()
  (interactive)
  (with-shell-interpreter
   :path "~"
   :interpreter "zsh"
   :form
   (shell)))

(defun my/pj-root-local ()
  (interactive)
  (with-shell-interpreter
   :path (projectile-project-root)
   :interpreter "zsh"
   :form
   (shell)))

(defun my/bash-on-jarvis ()
  (interactive)
  (with-shell-interpreter
   :path "/ssh:pi@jarvis.local:~"
   :interpreter "bash"
   :form
   (let ((current-prefix-arg '(4)))     ; don't prompt for remote interperter path
     (shell))))

(defun my/bash-on-volumio()
  (interactive)
  (with-shell-interpreter
   :path "/ssh:volumio@volumio.local:~"
   :interpreter "bash"
   :form
   (let ((current-prefix-arg '(4)))     ; don't prompt for remote interperter path
     (shell))))

(defun my/bash-on-jarvis2()
  (interactive)
  (with-shell-interpreter
   :path "/ssh:pi@raspberrypi.local:~"
   :interpreter "bash"
   :form
   (let ((current-prefix-arg '(4)))     ; don't prompt for remote interperter path
     (shell))))
