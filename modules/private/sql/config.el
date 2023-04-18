;;; config.el ---                                  -*- lexical-binding: t; -*-

(require 'xdg)

;; fromatter
(use-package! zetasql-formatter)

;; bigquery
(add-to-list 'auto-mode-alist '("\\.bq$" . sql-mode))

(set-repl-handler! 'sql-mode #'+nari/open-repl-sql)

(when (modulep! +lsp)
  (add-hook 'sql-mode-local-vars-hook #'lsp!)
  (setq lsp-sqls-workspace-config-path nil))

(defun nari/secret-file (filename)
  (concat
   (file-name-as-directory (xdg-data-home))
   (file-name-as-directory "secret")
   filename))
