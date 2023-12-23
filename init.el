;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       company           ; the ultimate code completion backend
       ;; +childframe)
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;(ivy               ; a search engine for love and life
        ;; +prescient)
        ;; +childframe)
       (vertico +icon)   ; the search engine of the future

       :ui
       deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji
        +github
        +unicode)
       ;;fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       ;;indent-guides     ; highlighted indent columns
       (ligatures +extras)         ; ligatures and symbols to make your code pretty again
       ;;minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;;pretty-code       ; replace bits of code with pretty symbols
       ;;tabs              ; an tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       (vc-gutter +pretty) ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired            ; making dired pretty [functional]
        ;;+ranger         ; bringing the goodness of ranger to dired
        +icons          ; colorful icons for dired-mode
        )
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)           ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; the elisp shell that works everywhere
       shell             ; simple shell REPL for Emacs
       term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       (spell +spell-fu) ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;biblio            ; Writes a PhD for you (citation needed)
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       (lsp
        +eglot)
       (magit +forge)    ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       pass              ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       tmux              ; an API for interacting with tmux
       tree-sitter       ; syntax and parsing, sitting in a tree...
       ;;upload            ; map local to remote projects via ssh/ftp
       github

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       (tty                ; improve the terminal Emacs experience
        +osc)

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       ;;(cc +lsp)         ; C > C++ == 1
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;dhall
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
       fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       (go                 ; the hipster dialect
        +tree-sitter
        +lsp)
       ;;(graphql +lsp)    ; Give queries a REST
       (haskell +lsp)    ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       ;;json              ; At least it ain't XML
       ;;(java +lsp) ; the poster child for carpal tunnel syndrome
       (javascript          ; all(hope(abandon(ye(who(enter(here))))))
        +tree-sitter)
       (json
        +tree-sitter)
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean              ; for folks with too much to prove
       ;;ledger            ; be audit you can be
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       (nix               ; I hereby declare "nix geht mehr!"
        +tree-sitter)
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +dragndrop       ; drag & drop files/images into org buffers
        +hugo            ; use Emacs for hugo blogging
        ;;+jupyter        ; ipython/jupyter support for babel
        +journal
        +pandoc          ; export-with-pandoc support
        +pretty
        ;;+pomodoro        ; be fruitful with the tomato technique
        +present        ; using org-mode for presentations
        +roam2)
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       (purescript         ; javascript, but functional
        +lsp)
       ;;python            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       (ruby               ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
        +rails
        +tree-sitter
        +lsp)
       (rust               ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
        +lsp)
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web              ; the tubes
        +tree-sitter
        +html
        +css)
       (yaml             ; JSON, but readable
        +tree-sitter)
       ;;zig               ; C, but simpler

       :email
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;;irc               ; how neckbeards socialize
       (rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens)

       :private
       (sql +lsp))

;; * Config
(setq max-lisp-eval-depth 5000
      max-specpdl-size 50000
      user-full-name "TAKASHI Tanabe"
      user-mail-address "narinari@c-fo.com")

(dir-locals-set-class-variables
 'freee-develop-directory
 '((ruby-mode . ((eval . (setq-local
                          flycheck-command-wrapper-function
                          (lambda (command)
                            (append '("bundle" "exec") command))))))
   (enh-ruby-mode . ((eval . (progn
                               (setq-local
                                flycheck-command-wrapper-function
                                (lambda (command)
                                  (let ((wrapped (append '("bundle" "exec" "rubocop") (cdr command))))
                                    wrapped)))
                               (setq-local rubocop-check-command "bin/rubocop --format emacs")))))
   ))

 (dir-locals-set-directory-class
   "/home/narinari/dev/src/github.com/C-FO/" 'freee-develop-directory)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(safe-local-variable-values
;;    (quote
;;     ((eval setq-local flycheck-command-wrapper-function
;;            (lambda
;;              (command)
;;              (let
;;                  ((wrapped
;;                    (append
;;                     (quote
;;                      ("bundle" "exec" "rubocop"))
;;                     (cdr command))))
;;                wrapped)))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nav-flash-face ((t (:background "yellow" :foreground "black" :weight bold)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1897b97f63e91a792e8540c06402f29d5edcbfb0aafd64b1b14270663d6868ee" "f2b83b9388b1a57f6286153130ee704243870d40ae9ec931d0a1798a5a916e76" "b0fd04a1b4b614840073a82a53e88fe2abc3d731462d6fde4e541807825af342" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" default)))
 '(psc-ide-rebuild-on-save t)
 '(safe-local-variable-values
   (quote
    ((psc-ide-use-npm-bin . t)
     (eval progn
           (setq-local flycheck-command-wrapper-function
                       (lambda
                         (command)
                         (let
                             ((wrapped
                               (append
                                (quote
                                 ("bundle" "exec" "rubocop"))
                                (cdr command))))
                           wrapped)))
           (setq-local rubocop-check-command "bin/rubocop --format emacs"))
     (eval setq-local flycheck-command-wrapper-function
           (lambda
             (command)
             (let
                 ((wrapped
                   (append
                    (quote
                     ("bundle" "exec" "rubocop"))
                    (cdr command))))
               wrapped))
           rubocop-check-command "bin/rubocop --format emacs")))))
