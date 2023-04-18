;;; repl.el ---                                      -*- lexical-binding: t; -*-

;;;###autoload
(defun +nari/open-repl-sql ()
  (interactive)
  (call-interactively 'sql-connect))
