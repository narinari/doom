;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! gitconfig-mode
	  :recipe (:host github :repo "magit/git-modes"
			 :files ("gitconfig-mode.el")))
(package! gitignore-mode
	  :recipe (:host github :repo "magit/git-modes"
			 :files ("gitignore-mode.el")))
(package! flycheck-aspell)
(package! async)
(package! calfw)
(package! calfw-org)
(package! dashboard)
(package! dired-open)
(package! dired-subtree)
(package! dmenu)
(package! elfeed-goodies)
(package! elpher)
(package! emojify)
(package! evil-tutor)
(package! ivy-posframe)
(package! mw-thesaurus)
(package! ox-gemini)
(package! ox-qmd :recipe (:host github :repo "0x60df/ox-qmd" :files ("ox-qmd.el")))
(package! peep-dired)
(package! password-store)
(package! rainbow-mode)
(package! resize-window)
(package! wc-mode)

;; * UI
(package! prettify-utils :recipe (:host github :repo "Ilazki/prettify-utils.el" :files ("*")))
(package! shr-tag-pre-highlight)
(package! hydra-posframe :recipe (:host github :repo "Ladicle/hydra-posframe" :files ("*.el")))
;; (package! all-the-icons-in-terminal
;;   :recipe (:host github
;;            :repo "uwabami/isfit-plus"
;;            :files (:defaults "data")))
(package! nerd-fonts :recipe (:host github :repo "twlz0ne/nerd-fonts.el" :files ("*.el")))
;; (package! icons-in-terminal)
;; (package! icons-in-terminal-dired :recipe (:host github :repo "takaxp/icons-in-terminal-dired"))
(package! all-the-icons)
(package! all-the-icons-dired)

(package! spacious-padding)

;; * Terms
(package! eat :recipe
  (:type git
   :host codeberg
   :repo "akib/emacs-eat"
   :files ("*.el" ("term" "term/*.el") "*.texi"
           "*.ti" ("terminfo/e" "terminfo/e/*")
           ("terminfo/65" "terminfo/65/*")
           ("integration" "integration/*")
           (:exclude ".dir-locals.el" "*-tests.el"))))

;; * Tools
(package! orgit)
(package! org-kanban)
(package! ox-qmd)
(package! alert)

(package! emamux)

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; * Writing
(package! academic-phrases)
(package! evil-string-inflection)

;; * Coding
(package! consult-yasnippet)
(package! helpful)
(package! tldr)
(package! electric-operator)
(package! lispy)
(package! lispyville)
(package! sed-mode)
(package! function-args)
(package! snakemake-mode)
(package! deadgrep)
(package! dap-mode)

;; * Lang
(package! terraform-mode)
(package! company-terraform)
(package! protobuf-mode)
(package! flow-minor-mode :recipe (:host github :repo "an-sh/flow-minor-mode" :files ("*.el")))
(package! flycheck-flow)
(package! fosi :recipe (:host github :repo "hotoku/fosi" :files ("elisp/*.el")))

;; (when (and
;;        (featurep! :lang go)
;;        (featurep! :completion company))
;;   (package! company-lsp))

(package! scad-mode)
(package! scad-preview)
(package! prettier-js)

(package! srcery-theme)
(package! modus-themes)
(package! ef-themes)

(package! go-translate)
