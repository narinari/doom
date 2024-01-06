;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(when (file-exists-p "/etc/profiles/per-user/narinari/bin")
  (add-to-list 'exec-path "/etc/profiles/per-user/narinari/bin"))

;; Place your private configuration here
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)

;; (after! skk
;; (map! :g [?\S- ] #'skk-mode
;;       :g [?\M-`] #'skk-mode)

(setq org-directory "~/GoogleDrive/org"
      my/font    "SFMono Nerd Font"
      my/font-ja "Noto Sans CJK JP")

(when (window-system)
  (set-fontset-font t 'japanese-jisx0208 my/font-ja)
  (set-fontset-font t 'japanese-jisx0213.2004-1 my/font-ja)
  (set-fontset-font nil 'japanese-jisx0208 my/font-ja)
  ;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
  (set-fontset-font t 'symbol "Apple Color Emoji")
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'symbol "Symbola" nil 'append)
  (when (eq 'ns (window-system))
    (setq mac-right-option-modifier 'meta)))

;; when live in nix
(let ((nix-profile-path "/etc/profiles/per-user/narinari"))
  (when (file-exists-p nix-profile-path)
    (setq exec-path (cons (concat nix-profile-path "/bin") exec-path)
          jka-compr-shell (concat nix-profile-path "/bin/zsh"))))

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(evil-define-key 'normal ibuffer-mode-map
  (kbd "f c") 'ibuffer-filter-by-content
  (kbd "f d") 'ibuffer-filter-by-directory
  (kbd "f f") 'ibuffer-filter-by-filename
  (kbd "f m") 'ibuffer-filter-by-mode
  (kbd "f n") 'ibuffer-filter-by-name
  (kbd "f x") 'ibuffer-filter-disable
  (kbd "g h") 'ibuffer-do-kill-lines
  (kbd "g H") 'ibuffer-update)

;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months
(defun dt/year-calendar (&optional year)
  (interactive)
  (require 'calendar)
  (let* (
         (current-year (number-to-string (nth 5 (decode-time (current-time)))))
         (month 0)
         (year (if year year (string-to-number (format-time-string "%Y" (current-time))))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq displayed-month month)
    (setq displayed-year year)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; horizontal rows
    (dotimes (j 4)
      ;; vertical columns
      (dotimes (i 3)
        (calendar-generate-month
         (setq month (+ month 1))
         year
         ;; indentation / spacing between months
         (+ 5 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun dt/scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by year in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 0))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let* (
             (year (+ displayed-year arg)))
        (dt/year-calendar year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun dt/scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by year in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (dt/scroll-year-calendar-forward (- (or arg 1)) event))

(map! :leader
      :desc "Scroll year calendar backward" "<left>" #'dt/scroll-year-calendar-backward
      :desc "Scroll year calendar forward" "<right>" #'dt/scroll-year-calendar-forward)

(defalias 'year-calendar 'dt/year-calendar)

(use-package! calfw)
(use-package! calfw-org)

(setq centaur-tabs-set-bar 'over
      centaur-tabs-set-icons t
      centaur-tabs-gray-out-icons 'buffer
      centaur-tabs-height 24
      centaur-tabs-set-modified-marker t
      centaur-tabs-style "bar"
      centaur-tabs-modified-marker "•")
(map! :leader
      :desc "Toggle tabs globally" "t c" #'centaur-tabs-mode
      :desc "Toggle tabs local display" "t C" #'centaur-tabs-local-mode)
(evil-define-key 'normal centaur-tabs-mode-map (kbd "g <right>") 'centaur-tabs-forward        ; default Doom binding is 'g t'
  (kbd "g <left>")  'centaur-tabs-backward       ; default Doom binding is 'g T'
  (kbd "g <down>")  'centaur-tabs-forward-group
  (kbd "g <up>")    'centaur-tabs-backward-group)

(use-package! dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "\nKEYBINDINGS:\
\nFind file               (SPC .)     \
Open buffer list    (SPC b i)\
\nFind recent files       (SPC f r)   \
Open the eshell     (SPC e s)\
\nOpen dired file manager (SPC d d)   \
List of keybindings (SPC h b b)")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner (expand-file-name "doom-emacs-dash.png" doom-private-dir))  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "nf-oct-file")
                                    (bookmarks . "nf-oct-book"))))

(setq doom-fallback-buffer "*dashboard*")


(after! woman
  ;; PATCH modules/default/config.el
  ;; failed in macos
  (when (executable-find "manpath")
    (setq woman-manpath
          (split-string (cdr (doom-call-process "manpath"))
                        path-separator t))))

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
              (:map dired-mode-map
               :desc "Peep-dired image previews" "d p" #'peep-dired
               :desc "Dired view file" "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-kill-line
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)

;; nerd-icons
(use-package! nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "BerkeleyMono Nerd Font Mono")
  )
(use-package! nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
(use-package! nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))
(use-package! treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

;; Get file icons in dired
;; (after! all-the-icons
;;   (when (window-system)
;;     (use-package! all-the-icons-dired)
;;     (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

;; (map! :leader
;; :desc "Load new theme" "h t" #'consult-load-theme)
(setq doom-font (font-spec :family my/font :size 12)
      doom-variable-pitch-font (font-spec :family my/font :size 12)
      doom-big-font (font-spec :family my/font :size 19))
;; (after! doom-themes
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t
;;         doom-theme 'modus-vivendi)
;;   (doom-themes-visual-bell-config))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; (after! modus-themes
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs t
;;         modus-themes-region '(bg-only no-extend)
;;         modus-themes-syntax '(faint yellow-comments green-strings))
;;   (modus-themes-load-themes)
;;   (modus-themes-load-vivendi))

(use-package! ef-themes
  :init
  (setq ef-themes-headings
        '((0 . (1.5))
          (1 . (1.3))
          (2 . (1.2))
          (3 . (1.1))
          (4 . (1.0))
          (5 . (1.0)) ; absence of weight means `bold'
          (6 . (1.0))
          (7 . (1.0))
          (agenda-date . (semilight 1.5))
          (agenda-structure . (variable-pitch light 1.9))
          (t . (1.0)))
        ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t
        ef-themes-region '(intense neutral))
  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)
  ;; NEXT: hoge
  (defun my-ef-themes-hl-todo-faces ()
    "Configure `hl-todo-keyword-faces' with Ef themes colors.
The exact color values are taken from the active Ef theme."
    (ef-themes-with-colors
      (setq hl-todo-keyword-faces
            `(("HOLD" . ,yellow)
              ("TODO" . ,red)
              ("NEXT" . ,blue)
              ("THEM" . ,magenta)
              ("PROG" . ,cyan-warmer)
              ("OKAY" . ,green-warmer)
              ("DONT" . ,yellow-warmer)
              ("FAIL" . ,red-warmer)
              ("BUG" . ,red-warmer)
              ("DONE" . ,green)
              ("NOTE" . ,blue-warmer)
              ("KLUDGE" . ,cyan)
              ("HACK" . ,cyan)
              ("TEMP" . ,red)
              ("FIXME" . ,red-warmer)
              ("XXX+" . ,red-warmer)
              ("REVIEW" . ,red)
              ("DEPRECATED" . ,yellow)))))
  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-hl-todo-faces)
  :config
  (ef-themes-select 'ef-cherie :no-confirm))

(after! elfeed
  (add-hook! elfeed-show-mode-hook 'visual-line-mode)
  (setq elfeed-goodies/entry-pane-size 0.5)
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "J") 'elfeed-goodies/split-show-next
    (kbd "K") 'elfeed-goodies/split-show-prev)
  (setq elfeed-feeds (quote
                      (("https://www.reddit.com/r/linux.rss" reddit linux)
                       ("https://www.reddit.com/r/commandline.rss" reddit commandline)
                       ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
                       ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                       ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
                       ("https://hackaday.com/blog/feed/" hackaday linux)
                       ("https://opensource.com/feed" opensource linux)
                       ("https://linux.softpedia.com/backend.xml" softpedia linux)
                       ("https://itsfoss.com/feed/" itsfoss linux)
                       ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
                       ("https://www.phoronix.com/rss.php" phoronix linux)
                       ("http://feeds.feedburner.com/d0od" omgubuntu linux)
                       ("https://www.computerworld.com/index.rss" computerworld linux)
                       ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
                       ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
                       ("https://betanews.com/feed" betanews linux)
                       ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
                       ("https://distrowatch.com/news/dwd.xml" distrowatch linux)))
        +rss-elfeed-files '("elfeed.org"))
  (after! elfeed-goodies
    (evil-define-key 'normal elfeed-show-mode-map
      (kbd "J") 'elfeed-goodies/split-show-next
      (kbd "K") 'elfeed-goodies/split-show-prev)))

(after! emm
  (emms-all)
  (emms-default-players)
  (emms-mode-line 1)
  (emms-playing-time 1)
  (setq emms-source-file-default-directory "~/Music/"
        emms-playlist-buffer-name "*Music*"
        emms-info-asynchronously t
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
  (map! :leader
        (:prefix ("a" . "EMMS audio player")
         :desc "Go to emms playlist" "a" #'emms-playlist-mode-go
         :desc "Emms pause track" "x" #'emms-pause
         :desc "Emms stop track" "s" #'emms-stop
         :desc "Emms play previous track" "p" #'emms-previous
         :desc "Emms play next track" "n" #'emms-next))
  )

(use-package! emojify
  :hook (after-init . global-emojify-mode)
  :config (setq emojify-display-style 'unicode))

;; (use-package! all-the-icons-in-terminal
;;   :after all-the-icons)

(map! :leader
      (:prefix ("e". "evaluate/EWW")
       :desc "Evaluate elisp in buffer" "b" #'eval-buffer
       :desc "Evaluate defun" "d" #'eval-defun
       :desc "Evaluate elisp expression" "e" #'eval-expression
       :desc "Evaluate last sexpression" "l" #'eval-last-sexp
       :desc "Evaluate elisp in region" "r" #'eval-region))

(setq browse-url-browser-function 'eww-browse-url)
;; (setq browse-url-browser-function 'browse-url-default-browser)
(map! :leader
      :desc "Search web for text between BEG/END"
      "s w" #'eww-search-words
      (:prefix ("e" . "evaluate/EWW")
       :desc "Eww web browser" "w" #'eww
       :desc "Eww reload page" "R" #'eww-reload))

(defun dt/insert-todays-date (prefix)
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%m-%d-%Y")
                 ((equal prefix '(16)) "%A, %B %d, %Y"))))
    (insert (format-time-string format))))

(require 'calendar)
(defun dt/insert-any-date (date)
  "Insert DATE using the current locale."
  (interactive (list (calendar-read-date)))
  (insert (calendar-date-string date)))

(map! :leader
      (:prefix ("i d" . "Insert date")
       :desc "Insert any date" "a" #'dt/insert-any-date
       :desc "Insert todays date" "t" #'dt/insert-todays-date))

(after! ivy
  (setq ivy-posframe-display-functions-alist
        '((swiper                     . ivy-posframe-display-at-point)
          (complete-symbol            . ivy-posframe-display-at-point)
          (counsel-M-x                . ivy-display-function-fallback)
          (counsel-esh-history        . ivy-posframe-display-at-window-center)
          (counsel-describe-function  . ivy-display-function-fallback)
          (counsel-describe-variable  . ivy-display-function-fallback)
          (counsel-find-file          . ivy-display-function-fallback)
          (counsel-recentf            . ivy-display-function-fallback)
          (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
          (dmenu                      . ivy-posframe-display-at-frame-top-center)
          (nil                        . ivy-posframe-display))
        ivy-posframe-height-alist
        '((swiper . 20)
          (dmenu . 20)
          (t . 10))
        ivy-use-selectable-prompt t
        ivy-auto-select-single-candidate t
        ivy-rich-parse-remote-buffer nil
        +ivy-buffer-icons nil
        ivy-use-virtual-buffers nil
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
        ivy-rich-switch-buffer-name-max-length 50
        ivy-count-format "(%d/%d) "
        ivy-pre-prompt-function #'my-pre-prompt-function)
  (ivy-posframe-mode 1) ; 1 enables posframe-mode, 0 disables it.

  ;; +**** Common actions for counsel-ag, counsel-fzf, and counsel-recentf
  (defun my-counsel-fzf-in-default-dir (_arg)
    "Search the current directory with fzf."
    (counsel-fzf ivy-text default-directory))
  (defun my-counsel-fzf-in-dir (_arg)
    "Search again with new root directory."
    (counsel-fzf ivy-text
                 (read-directory-name
                  (concat (car (split-string counsel-fzf-cmd))
                          " in directory: "))))
  (defun my-counsel-ag-in-dir (_arg)
    "Search again with new root directory."
    (let ((current-prefix-arg '(4)))
      (counsel-ag ivy-text nil ""))) ;; also disable extra-ag-args
  (defun my-pre-prompt-function ()
    (if window-system
        (format "%s\n%s "
                (make-string (frame-width) ?\x5F) ;; "__"
                (all-the-icons-faicon "sort-amount-asc")) ;; ""
      (format "%s\n " (make-string (1- (frame-width)) ?\x2D))))

  (ivy-add-actions 'counsel-ag
                   '(("r" my-counsel-ag-in-dir "search in directory")))

  (ivy-add-actions 'counsel-fzf
                   '(("r" my-counsel-fzf-in-dir "search in directory")))

  (ivy-add-actions 'counsel-recentf
                   '(("g" my-counsel-ag-in-dir "switch to ag")
                     ("z" my-counsel-fzf-in-default-dir "switch to fzf")))

  (map! :leader
        (:prefix ("v" . "Ivy")
         :desc "Ivy push view" "v p" #'ivy-push-view
         :desc "Ivy switch view" "v s" #'ivy-switch-view)))

;; **** counsel-config
(after! counsel
  (setq ;; counsel-evil-registers-height 20
   ;; counsel-yank-pop-height 20
   counsel-org-goto-face-style 'org
   counsel-org-headline-display-style 'title
   counsel-org-headline-display-tags t
   counsel-org-headline-display-todo t)
  ;; **** counsel-load-theme
  ;; reset fringe after change theme
  ;; (advice-add #'counsel-load-theme :after #'solaire-mode-reset)
  ;; **** ivy-switch-buffer
  ;;  (advice-add 'ivy--switch-buffer-action :override #'*ivy--switch-buffer-action)
  (ivy-add-actions 'ivy-switch-buffer
                   '(("d" (lambda (buf) (display-buffer buf)) "display")))
  ;; **** counsel-M-x
  (ivy-add-actions 'counsel-M-x
                   `(("h" +ivy/helpful-function "Helpful"))))

(setq display-line-numbers-type t)
(map! :leader
      :desc "Comment or uncomment lines" "TAB TAB" #'comment-line
      (:prefix ("t" . "toggle")
       :desc "Toggle line numbers" "l" #'doom/toggle-line-numbers
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle truncate lines" "t" #'toggle-truncate-lines))

(set-face-attribute 'mode-line nil :font (concat my/font "-13"))
(after! doom-modeline
  ;;
  ;; evil-state
  ;;
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon nil
        doom-modeline-minor-modes nil
        doom-modeline-height 30     ;; sets modeline height
        doom-modeline-bar-width 5   ;; sets right bar width
        doom-modeline-persp-name t  ;; adds perspective name to modeline
        doom-modeline-persp-icon t  ;; adds folder icon next to persp name
        ;; evil-normal-state-tag   (propertize "<N>" 'face '((:background "green" :foreground "black")))
        ;; evil-emacs-state-tag    (propertize "<E>" 'face '((:background "orange" :foreground "black")))
        ;; evil-insert-state-tag   (propertize "<I>" 'face '((:background "red") :foreground "white"))
        ;; evil-motion-state-tag   (propertize "<M>" 'face '((:background "blue") :foreground "white"))
        ;; evil-visual-state-tag   (propertize "<V>" 'face '((:background "grey80" :foreground "black")))
        ;; evil-operator-state-tag (propertize "<O>" 'face '((:background "purple"))))
        )
  (set-cursor-color "cyan")
  (line-number-mode 0)
  (column-number-mode 0)
  (doom-modeline-def-modeline 'main
    '(bar
      workspace-name
      window-number
      modals
      ;; god-state
      ;; ryo-modal
      ;; xah-fly-keys
      matches
      buffer-info
      remote-host
      buffer-position
      parrot
      selection-info)
    '(misc-info
      persp-name
      lsp
      github
      debug
      minor-modes
      input-method
      major-mode
      process
      vcs
      checker)))

(xterm-mouse-mode 1)

(use-package! spacious-padding
  :ensure t
  :hook (after-init . spacious-padding-mode))

(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t))
(map! :leader
      :desc "Toggle neotree file viewer" "t n" #'neotree-toggle
      :desc "Open directory in neotree" "d n" #'neotree-dir)

(map! :leader
      (:prefix ("=" . "open file")
       :desc "Edit agenda file" "a" #'(lambda () (interactive) (find-file (expand-file-name "agenda.org" org-directory)))
       :desc "Edit doom config.org" "c" #'(lambda () (interactive) (find-file (expand-file-name "config.org" doom-private-dir)))
       :desc "Edit doom init.el" "i" #'(lambda () (interactive) (find-file (expand-file-name "init.el" doom-private-dir)))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file (expand-file-name "packages.el" doom-private-dir)))))
(map! :leader
      (:prefix ("= e" . "open eshell files")
       :desc "Edit eshell aliases" "a" #'(lambda () (interactive) (find-file (expand-file-name "eshell/aliases" doom-private-dir)))
       :desc "Edit eshell profile" "p" #'(lambda () (interactive) (find-file (expand-file-name "eshell/profile" doom-private-dir)))))

(after! org
  (setq +org-capture-todo-file "inbox.org"
        org-agenda-files (list
                          (expand-file-name +org-capture-todo-file org-directory)
                          (expand-file-name "projects.org" org-directory)
                          (expand-file-name "repeaters.org" org-directory))
        org-agenda-tags-column 75
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-deadline-warning-days 30
        org-return-follows-link t
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-item-bullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-hide-emphasis-markers t
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
        '(("google" . "http://www.google.com/search?q=")
          ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
          ("ddg" . "https://duckduckgo.com/?q=")
          ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
        '((sequence
           "TODO(t)"           ; A task that is ready to be tackled
           "BLOG(b)"           ; Blog writing assignments
           "GYM(g)"            ; Things to accomplish at the gym
           "PROJ(p)"           ; A project that contains other tasks
           "VIDEO(v)"          ; Video assignments
           "|"                 ; The pipe necessary to separate "active" states and "inactive" states
           "DONE(d)")          ; Task has been completed
          (sequence
           "WAITING(w@/!)"     ; Something is holding up this task
           "HOLD(h@/!)"
           "|"
           "CANCELLED(c@/!)")) ; Task has been cancelled
        org-agenda-current-time-string "← now"
        org-agenda-time-grid ;; Format is changed from 9.1
        '((daily today require-timed)
          (0900 01000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
          "-"
          "────────────────"
          )
        org-agenda-custom-commands
        (append org-agenda-custom-commands
                '((" " "Agenda"
                   ((agenda ""
                            ((org-agenda-span 'day)))
                    (todo "TODO"
                          ((org-agenda-overriding-header "Unscheduled tasks")
                           (org-agenda-files (list (expand-file-name +org-capture-todo-file org-directory)))
                           (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                           ))
                    (todo "TODO"
                          ((org-agenda-overriding-header "Unscheduled project tasks")
                           (org-agenda-files (list (expand-file-name "projects.org" org-directory)))
                           (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline)))))))))

  ;;; for source reading
  ;;; https://ladicle.com/post/20200625_123915/
  (defvar org-code-capture--store-file "")
  (defvar org-code-capture--store-header "")

  ;; This function is used in combination with a coding template of org-capture.
  (defun org-code-capture--store-here ()
    "Register current subtree as a capture point."
    (interactive)
    (setq org-code-capture--store-file (buffer-file-name))
    (setq org-code-capture--store-header (nth 4 (org-heading-components))))

  ;; This function is used with a capture-template for (function) type.
  ;; Look for headline that registered at `org-code-capture--store-header`.
  ;; If the matching subtree is not found, create a new Capture tree.
  (defun org-code-capture--find-store-point ()
    "Find registered capture point and move the cursor to it."
    (let ((filename (if (string= "" org-code-capture--store-file)
                        (format-time-string org-journal-file-format)
                      org-code-capture--store-file)))
      (set-buffer (org-capture-target-buffer filename)))
    (goto-char (point-min))
    (unless (derived-mode-p 'org-mode)
      (error
       "Target buffer \"%s\" for org-code-capture--find-store-file should be in Org mode"
       (current-buffer))
      (current-buffer))
    (if (re-search-forward org-code-capture--store-header nil t)
        (goto-char (point-at-bol))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* Capture\n")
      (beginning-of-line 0))
    (org-end-of-subtree))

  (defun nari/org-journal-new-entry-point ()
    (interactive)
    (org-journal-new-entry t)
    (outline-previous-heading)
    (unless (= (org-current-level) 1)
      (outline-up-heading 1)))

  (setq org-capture-templates
        (append org-capture-templates
                '(("l" "Work log" entry
                   (function nari/org-journal-new-entry-point)
                   "* %<%H:%M> %?\n")
                  ("d" "Draft" entry
                   (file+headline "draft.org" "Inbox")
                   "* %?\n %U\n"
                   :prepend t :kill-buffer t)
                  ("c"
                   "Store the code-reading notes with GitHub and file links for the current cursor position."
                   plain
                   (function org-code-capture--find-store-point)
                   "%^{Summary}\n%(with-current-buffer (org-capture-get :original-buffer) (browse-at-remote-get-url))\n# %a"
                   :immediate-finish t)
                  ("r"
                   "Immediately store GitHub and file links for the current cursor position to the current code-reading notes."
                   plain
                   (function org-code-capture--find-store-point)
                   "%(with-current-buffer (org-capture-get :original-buffer) (browse-at-remote-get-url))\n# %a"
                   :immediate-finish t))))

  (map! :leader
        :desc "Org babel tangle" "m B" #'org-babel-tangle))

;;(after! org-journal
;;  (defun org-journal-find-location ()
;;    ;; Open today's journal, but specify a non-nil prefix argument in order to
;;    ;; inhibit inserting the heading; org-capture will insert the heading.
;;    (org-journal-new-entry t)
;;    ;; Position point on the journal's top-level heading so that org-capture
;;    ;; will add the new entry as a child entry.
;;    (goto-char (point-min)))
;;
;;  (add-to-list org-capture-templates
;;               '("j" "Journal entry" entry (function org-journal-find-location)
;;                 "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))


(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 )

(use-package ox-man)
(use-package ox-gemini)

(setq org-journal-dir (expand-file-name "journal/" org-directory)
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%Y-%m-%d"
      org-journal-file-type 'monthly
      org-journal-file-format "%Y-%m.org"
      nari/org-journal-dir (expand-file-name "blog-journal/" org-directory)
      nari/org-journal-file-format (concat nari/org-journal-dir "%Y/%Y-%m-%d.org"))

(setq org-publish-use-timestamps-flag nil)
(setq org-export-with-broken-links t)
(setq org-publish-project-alist
      '(("distro.tube"
         :base-directory "~/nc/gitlab-repos/distro.tube/"
         :base-extension "org"
         :publishing-directory "~/nc/gitlab-repos/distro.tube/html/"
         :recursive t
         :exclude "org-html-themes/.*"
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t)
        ("org-static"
         :base-directory "~/Org/website"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/"
         :recursive t
         :exclude ".*/org-html-themes/.*"
         :publishing-function org-publish-attachment)
        ))

(after! org-roam
  (setq org-roam-directory (expand-file-name "roam" org-directory)))

(after! org-tree-slide
  (map! :map org-tree-slide-mode-map
        :n [next] #'org-tree-slide-move-next-tree
        :n [prior]  #'org-tree-slide-move-previous-tree))

(use-package! password-store)

(map! :leader
      :desc "Switch to perspective NAME" "DEL" #'persp-switch
      :desc "Switch to buffer in perspective" "," #'persp-switch-to-buffer
      :desc "Switch to next perspective" "]" #'persp-next
      :desc "Switch to previous perspective" "[" #'persp-prev
      :desc "Add a buffer current perspective" "+" #'persp-add-buffer
      :desc "Remove perspective by name" "-" #'persp-remove-by-name)

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(global-rainbow-mode 1 )

(map! :leader
      (:prefix ("r" . "registers")
       :desc "Copy to register" "c" #'copy-to-register
       :desc "Frameset to register" "f" #'frameset-to-register
       :desc "Insert contents of register" "i" #'insert-register
       :desc "Jump to register" "j" #'jump-to-register
       :desc "List registers" "l" #'list-registers
       :desc "Number to register" "n" #'number-to-register
       :desc "Interactively choose a register" "r" #'counsel-register
       :desc "View a register" "v" #'view-register
       :desc "Window configuration to register" "w" #'window-configuration-to-register
       :desc "Increment register" "+" #'increment-register
       :desc "Point to register" "SPC" #'point-to-register))

(setq shell-file-name "/bin/zsh"
      vterm-max-scrollback 5000)
(setq eshell-rc-script (expand-file-name "eshell/profile" doom-private-dir)
      eshell-aliases-file (expand-file-name "eshell/aliases" doom-private-dir)
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))
(add-hook! 'eshell-mode-hook 'with-editor-export-editor)
(map! :leader
      :desc "Eshell" "e s" #'eshell
      :desc "Eshell popup toggle" "e t" #'+eshell/toggle
      :desc "Counsel eshell history" "e h" #'counsel-esh-history
      :desc "Vterm popup toggle" "v t" #'+vterm/toggle)

(use-package! eat)

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)

(map! :leader
      (:prefix ("w" . "window")
       :desc "Winner redo" "<right>" #'winner-redo
       :desc "Winner undo" "<left>" #'winner-undo))

(map! :leader
      :desc "Zap to char" "z" #'zap-to-char
      :desc "Zap up to char" "Z" #'zap-up-to-char)

(setq request-storage-directory (concat doom-etc-dir "request/")
      enable-remote-dir-locals t
      tab-width 2
      window-combination-resize t
      projectile-project-search-path '("~/Projects/" "~/dev/src/github.com/narinari/" "~/dev/src/github.com/C-FO/" "~/dev/src/github.com/narinari-freee/")
      electric-pair-inhibit-predicate 'ignore
      persp-interactive-init-frame-behaviour-override -1
      +reference-field 'bioinfo
      bibtex-completion-bibliography (list (expand-file-name "reference/Bibliography.bib" org-directory))
      bibtex-completion-library-path (expand-file-name "reference/pdf/" org-directory)
      bibtex-completion-notes-path (expand-file-name "ref.org" org-directory)
      org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" "")
      org-tags-column -80
      org-tag-persistent-alist
      '(("happy" . ?h)
        ("ops" . ?o)
        ("etc" . ?e)
        ("break" . ?b)
        ("mtg" . ?m))
      org-ref-default-bibliography bibtex-completion-bibliography
      org-ref-bibliography-notes bibtex-completion-notes-path
      org-ref-pdf-directory bibtex-completion-library-path
      deft-directory org-directory
      deft-extensions '("org" "txt")
      deft-recursive t
      lsp-file-watch-threshold 2000
      twittering-connection-type-order '(curl wget urllib-http native urllib-https)
      visual-fill-column-center-text t
      evil-escape-key-sequence nil
      evil-goggles-duration 0.4
      line-spacing nil
      ;; nav-flash-use-pulse t
      frame-resize-pixelwise t)

(after! pangu-spacing
  (remove-hook 'text-mode-hook 'pangu-spacing-mode)
  (add-hook! org-mode 'pangu-spacing-mode))

(after! ace-window
  (setq aw-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?o)))

(after! avy
  (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?o)
        avy-all-windows t))

;; (use-package-hook! enh-ruby-mode
;;   :post-config
;;   (set-company-backend! 'enh-ruby-mode '((:separate company-gtags company-etags company-keywords)))
;;   t)

;; (set-company-backend! '(text-mode
;;                         markdown-mode
;;                         gfm-mode)
;;   '(:seperate company-ispell
;;               company-files
;;               company-yasnippet))

(after! browse-at-remote
  (setq browse-at-remote-prefer-symbolic nil)) ; nilにするとブランチ名ではなくハッシュ値を使う。ブランチ名にするとファイル移動などで追えなくなるため。

(after! scad-mode
  (add-hook! scad-mode-hook scad-preview-mode)
  (defhydra hydra-scad-preview (:color pink :hint nil)
    "
                                                                       ╔═════════╗
    Move^^^^^^^^^    Rotate^^^^^^^^       Zoom^^                                  ║ Preview ║
  ─────────────────────────────────────────────────────────────────────╨─────────╜
        ^_n_^           ^_N_^       _+_
        ^^↑^^           ^^↑^^       ^↑^
    _h_ ←   → _i_   _H_ ←   → _I_  Zoom^^^^^
        ^^↓^^           ^^↓^^       ^↓^
        ^_e_^           ^_E_^       _-_
  ╭──────────────────────────────────────────────────────────────────────────────╯
                           [_q_],[_<esc>_]: quit
"
    ("h" scad-preview-trnsx+)
    ("i" scad-preview-trnsx-)
    ("n" scad-preview-trnsz+)
    ("e" scad-preview-trnsz-)
    ("H" scad-preview-rotx+)
    ("I" scad-preview-rotx-)
    ("N" scad-preview-rotz+)
    ("E" scad-preview-rotz-)
    ("+" scad-preview-dist-)
    ("-" scad-preview-dist+)
    ("q" nil)
    ("<esc>" nil)
    )
  (map! :after scad-mode
        :localleader
        :map scad-preview--image-mode-map
        :desc "scad preview" "p" #'hydra-scad-preview/body
        )
  )
;; (after! hydra-posframe
;;   (hydra-posframe-parameters
;;     '((left-fringe . 5)
;;       (right-fringe . 5)))
;;   (add-hook! after-init hydra-posframe-enable))

(defhydra hydra-multiple-cursors (:color pink :hint nil)
  "
                                                                        ╔════════╗
    Point^^^^^^^^           Misc^^                                                ║ Cursor ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
     _k_    _K_     _D_    _p_    [_x_] undo all
     ^↑^    ^↑^     ^↑^    ^↑^    [_m_] mark all
    make^^ skip^^^ mark^^ navi^^  [_u_] undo last
     ^↓^    ^↓^     ^↓^    ^↓^
     _j_    _J_     _d_    _n_
  ╭──────────────────────────────────────────────────────────────────────────────╯
                           [_q_]: quit, [Click]: point
"
  ("d" evil-mc-make-and-goto-next-match)
  ("D" evil-mc-make-and-goto-prev-match)
  ("j" evil-mc-make-cursor-move-next-line)
  ("J" evil-mc-skip-and-goto-next-match)
  ("k" evil-mc-make-cursor-move-prev-line)
  ("K" evil-mc-skip-and-goto-prev-match)
  ("m" evil-mc-make-all-cursors :exit t)
  ("n" evil-mc-make-and-goto-next-cursor)
  ("N" evil-mc-make-and-goto-last-cursor)
  ("p" evil-mc-make-and-goto-prev-cursor)
  ("P" evil-mc-make-and-goto-first-cursor)
  ("x" evil-mc-undo-all-cursors :exit t)
  ("u" evil-mc-undo-last-added-cursor)
  ("q" nil))

(defhydra hydra-avy (:color pink :hint nil)
  "
                                                                        ╔════════╗
        ^^Goto^^        Kill^^        Yank^^        Move^^        Misc            ║  Jump  ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
    _c_ ← char^^        [_k_] region  [_y_] region  [_m_] region  [_n_] line number
    _a_ ← char2 → _b_   [_K_] line    [_Y_] line    [_M_] line    [_v_] Goto viewer
    _w_ ← word  → _W_   [_z_] zap^^^^                             [_o_] Goto clock
    _l_ ← line  → _e_   ^^^^^                                     _,_ ← f!y → _._
  ╭──────────────────────────────────────────────────────────────────────────────╯
                      [_q_]: quit, [_i_]: imenu, [_<SPC>_]: resume
"
  ("c" avy-goto-char :exit t)
  ("a" avy-goto-char-2 :exit t)
  ("b" avy-goto-char-below :exit t)
  ("w" avy-goto-word-1 :exit t)
  ("W" avy-goto-word-1-below :exit t)
  ("l" avy-goto-line :exit t)
  ("e" avy-goto-end-of-line :exit t)
  ("M" avy-move-line)
  ("m" avy-move-region)
  ("K" avy-kill-whole-line)
  ("k" avy-kill-region)
  ("Y" avy-copy-line :exit t)
  ("y" avy-copy-region :exit t)
  ("n" goto-line :exit t)
  ("o" org-clock-jump-to-current-clock :exit t)
  ("z" avy-zap-to-char-dwim :exit t)
  ("v" hydra-viewer/body :exit t)
  ("<SPC>" avy-resume :exit t)
  ("o" org-clock-jump-to-current-clock :exit t)
  ("i" counsel-imenu :exit t)
  ("," flymake-goto-previous-error)
  ("." flymake-goto-next-error)
  ("q" nil))

;; ** ui
;; *** indent-guides
(after! highlight-indent-guides
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t))

;; ** edit
(use-package! evil-string-inflection :ensure t)

;; *** company

(after! company
  ;; **** company-ui
  (setq company-tooltip-limit 10
        company-tooltip-minimum-width 80
        company-tooltip-minimum 10
        company-minimum-prefix-length 1
        company-idle-delay 0
        company-selection-wrap-around t
        completion-ignore-case t
        company-dabbrev-downcase nil
        company-box-icons-unknown 'fa_question_circle
        company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|_\\|-\\)"    ; -や_などを含む語句も補完
        company-box-icons-elisp
        '((fa_tag :face font-lock-function-name-face) ;; Function
          (fa_cog :face font-lock-variable-name-face) ;; Variable
          (fa_cube :face font-lock-constant-face) ;; Feature
          (md_color_lens :face font-lock-doc-face)) ;; Face
        company-box-icons-yasnippet 'fa_bookmark
        company-box-icons-lsp
        '((1 . fa_text_height) ;; Text
          (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
          (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
          (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
          (5 . (fa_cog :foreground "#FF9800")) ;; Field
          (6 . (fa_cog :foreground "#FF9800")) ;; Variable
          (7 . (fa_cube :foreground "#7C4DFF")) ;; Class
          (8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
          (9 . (fa_cube :foreground "#7C4DFF")) ;; Module
          (10 . (fa_cog :foreground "#FF9800")) ;; Property
          (11 . md_settings_system_daydream) ;; Unit
          (12 . (fa_cog :foreground "#FF9800")) ;; Value
          (13 . (md_storage :face font-lock-type-face)) ;; Enum
          (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
          (15 . md_closed_caption) ;; Snippet
          (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
          (17 . fa_file_text_o) ;; File
          (18 . md_refresh) ;; Reference
          (19 . fa_folder_open) ;; Folder
          (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
          (21 . (fa_square :face font-lock-constant-face)) ;; Constant
          (22 . (fa_cube :face font-lock-type-face)) ;; Struct
          (23 . fa_calendar) ;; Event
          (24 . fa_square_o) ;; Operator
          (25 . fa_arrows)))) ;; TypeParameter

;; *** file-templates
;; (defvar private-file-templates-dir
;;   (expand-file-name "templates/" (file-name-directory load-file-name))
;;   "The path to a directory of yasnippet folders to use for file templates.")
;; (add-to-list 'yas-snippet-dirs 'private-file-templates-dir 'append #'eq)
;; (set-file-template! "\\.vue$" ':trigger "__.vue" :mode 'web-mode)
(after! yasnippet
  (set-file-template! "org/interviews/.*/.*\\.org$" :trigger "__eval.org" :mode 'org-mode)
  (set-file-template! "org/interviews/.*/rookie/.*\\.org$" :trigger "__rookie.org" :mode 'org-mode)
  (yas-reload-all))

;; ** completion
;; *** vertio
(after! marginalia
  (setq marginalia-truncate-width (frame-width)))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; ** tools
;; *** magit
(after! magit
  (setq magit-log-margin-show-committer-date 't)
  (after! tramp
    (add-to-list 'tramp-methods
                 '("yadm"
                   (tramp-login-program "yadm")
                   (tramp-login-args (("enter")))
                   (tramp-login-env (("SHELL") ("/bin/sh")))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-args ("-c")))))
  (map! :leader
        (:prefix-map ("g" . "git")
         :desc "yadm status" "d" (lambda () (interactive)(magit-status "/yadm::"))))
  )

;; *** tree-sitter
;; https://blog.meain.io/2022/navigating-config-files-using-tree-sitter/
(when (modulep! :tools tree-sitter)
  (defvar meain/tree-sitter-config-nesting--queries '((json-mode . "(object (pair (string (string_content) @key) (_)) @item)")
                                                      (yaml-mode . "(block_mapping_pair (flow_node) @key (_)) @item")
                                                      (nix-mode . "(bind (attrpath (attr_identifier) @key)) @item")))
  (defun meain/tree-sitter-config-nesting ()
    (when-let* ((query-s (cdr (assq major-mode meain/tree-sitter-config-nesting--queries)))
                (query (tsc-make-query tree-sitter-language query-s))
                (root-node (tsc-root-node tree-sitter-tree))
                (matches (tsc-query-matches query root-node #'tsc--buffer-substring-no-properties)))
      (string-join
       (remove-if #'null
                  (seq-map (lambda (x)
                             (let* ((item (seq-elt (cdr x) 0))
                                    (key (seq-elt (cdr x) 1))
                                    (pos (tsc-node-byte-range (cdr item))))
                               (when (> (byte-to-position (cdr pos))
                                        (point)
                                        (byte-to-position (car pos)))
                                 (format "%s" (tsc-node-text (cdr key))))))
                           matches))
       ".")))
  (defun meain/get-config-nesting-paths ()
    "Get out all the nested paths in a config file."
    (when-let* ((query-s (cdr (assq major-mode meain/tree-sitter-config-nesting--queries)))
                (root-node (tsc-root-node tree-sitter-tree))
                (query (tsc-make-query tree-sitter-language query-s))
                (matches (tsc-query-matches query root-node #'tsc--buffer-substring-no-properties))
                (item-ranges (seq-map (lambda (x)
                                        (let ((item (seq-elt (cdr x) 0))
                                              (key (seq-elt (cdr x) 1)))
                                          (list (tsc-node-text (cdr key))
                                                (tsc-node-range (cdr key))
                                                (tsc-node-range (cdr item)))))
                                      matches))
                (parent-nodes '(("#" 0))))
      (mapcar (lambda (x)
                (let* ((current-end (seq-elt (cadr (cdr x)) 1))
                       (parent-end (cadar parent-nodes))
                       (current-key (car x)))
                  (progn
                    (if (> current-end parent-end)
                        (setq parent-nodes
                              (-filter (lambda (y) (< current-end (cadr y)))
                                       parent-nodes)))
                    (setq parent-nodes (cons (list current-key current-end) parent-nodes))
                    (list (reverse (mapcar #'car parent-nodes))
                          (seq-elt (cadr x) 0)))))
              item-ranges)))
  (defun meain/imenu-config-nesting-path ()
    "Return config-nesting paths for use in imenu"
    (mapcar (lambda (x)
              (cons (string-join (car x) ".") (cadr x)))
            (meain/get-config-nesting-paths)))
  (setq meain/tree-sitter-class-like '((rust-mode . (impl_item))
                                       (python-mode . (class_definition))))
  (setq meain/tree-sitter-function-like '((rust-mode . (function_item))
                                          (go-mode . (function_declaration method_declaration))
                                          (sh-mode . (function_definition))
                                          (python-mode . (function_definition))))
  (defun meain/tree-sitter-thing-name (kind)
    "Get name of tree-sitter KIND thing."
    (when-let (tree-sitter-mode
               (node-types (pcase kind
                             ('class-like meain/tree-sitter-class-like)
                             ('function-like meain/tree-sitter-function-like)))
               (node-at-point (cl-some #'tree-sitter-node-at-point
                                       (alist-get major-mode node-types)))
               (node-name (tsc-get-child-by-field node-at-point :name)))
      (tsc-node-text node-name)))
  ;; Connect to which-function for magit-log-trace-definition
  (setq which-func-functions
        (list
         (lambda () (meain/tree-sitter-thing-name 'function-like))
         (lambda () (meain/tree-sitter-thing-name 'class-like)))))

;; ** lang
;; *** prettier-js
(after! prettier-js
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'json-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

(use-package! flycheck-flow)
(use-package! flow-minor-mode
  :hook (js2-mode-hook . flow-minor-enable-automatically)
  :config
  (after! 'flycheck
    (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
    (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))
  (after! 'company
    (add-to-list 'company-backends 'company-flow)))

;; *** go
(after! go-mode
  (setq gofmt-command "goimports")
  (add-hook! 'before-save-hook #'gofmt-before-save))

(after! go-ts-mode
  (setq gofmt-command "goimports")
  (add-hook! 'before-save-hook #'gofmt-before-save)
  (when (modulep! :tools lsp +eglot)
    (after! eglot
      (add-to-list 'eglot-server-programs '(go-ts-mode "gopls")))))

;; *** ruby
(after! ruby-mode
  (when (modulep! :tools lsp +eglot)
    (after! eglot
      (add-to-list 'eglot-server-programs '(ruby-mode "ruby-lsp")))))

(after! dap-mode
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions repl controls tooltip))
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-auto-configure-mode 1)
  (require 'dap-go)
  (require 'dap-ruby)
  (require 'dap-node)
  (dap-node-setup)
  )

(after! 'flycheck
  (setq-default flycheck-disabled-checkers (append '(go-golint) (default-value 'flycheck-disabled-checkers)))) ;; golint はdeprecateされたので無効にする

;; *** sql
(load (nari/secret-file "secret-sql"))

;; *** markdown
(after! markdown-mode
  (defalias #'markdown-use-region-p #'use-region-p)) ;; patch for removed function call in doom markdown module
(use-package! fosi) ;; previewer

;; *** nix
(after! nix-mode
  (when (modulep! :lang nix +tree-sitter)
    (setq-hook! nix-mode
      imenu-create-index-function #'meain/imenu-config-nesting-path)))

;; *** json
(after! json-mode
  (when (modulep! :lang json +tree-sitter)
    (setq-hook! json-mode
      imenu-create-index-function #'meain/imenu-config-nesting-path)))

;; *** yaml
(after! yaml-mode
  (when (modulep! :lang yaml +tree-sitter)
    (add-hook! yaml-mode-local-vars
               :append #'tree-sitter!)
    (remove-hook 'yaml-mode-hook 'yaml-set-imenu-generic-expression) ;; don't use default one
    (setq-hook! yaml-mode
      imenu-create-index-function #'meain/imenu-config-nesting-path)))

(after! format-all
  (defun format-all--buffer-hard-rubocop-nari
      (ok-statuses error-regexp root-files executable &rest args)
    (let ((ok-statuses (or ok-statuses '(0)))
          (args (format-all--flatten-once args))
          (default-directory (format-all--locate-default-directory root-files)))
      (when format-all-debug
        (message "Format-All: Running: %s"
                 (mapconcat #'shell-quote-argument (cons executable args) " "))
        (message "Format-All: Directory: %s" default-directory))
      (format-all--buffer-thunk
       (lambda (input)
         (let ((src (current-buffer)))
           (with-temp-buffer
             (let* ((status (apply #'call-process-region input nil
                                   executable nil t
                                   nil args))
                    (errput (progn
                              (goto-char (point-min))
                              (message (buffer-name))
                              (message (buffer-substring 1 10))
                              (let* ((delimiter "====================\n")
                                     (start (search-forward delimiter))
                                     (end (point-max))
                                     (errbuf (current-buffer)))
                                (with-current-buffer src
                                  (erase-buffer)
                                  (insert-buffer-substring errbuf start end))
                                (delete-region (- start (length delimiter)) (point-max)))
                              (buffer-string)))
                    (errorp (or (not (member status ok-statuses))
                                (and error-regexp
                                     (save-match-data
                                       (string-match error-regexp errput))))))
               (list errorp errput))))))))

  ;; Rubocop for format-all
  ;; from: https://github.com/lassik/emacs-format-all-the-code/pull/107
  (defun regexp-or (&rest args)
    (string-join args "|"))

  (defun format-all--ruby-gem-bundled-p (gem-name)
    "Internal helper function to check if GEM-NAME is listed in the current project's Gemfile.lock."
    (let* ((lockfile "Gemfile.lock")
           (dir (locate-dominating-file (or (buffer-file-name) ".") lockfile)))
      (and dir
           (with-temp-buffer
             (insert-file-contents (expand-file-name lockfile dir))
             (re-search-forward (format "^    %s " (regexp-quote gem-name)) nil t))
           t)))

  (defun format-all--buffer-hard-ruby
      (gem-name ok-statuses error-regexp root-files executable &rest args)
    "Internal helper function to implement ruby based formatters.
GEM-NAME is the name of a Ruby gem required to run EXECUTABLE.
For OK-STATUSES, ERROR-REGEXP, ROOT-FILES, EXECUTABLE and ARGS, see `format-all--buffer-hard'."
    (let* ((error-regexp
            (apply #'regexp-or
                   "Bundler::GemNotFound"
                   (concat "bundler: failed to load command: "
                           (regexp-quote executable))
                   (concat (regexp-or "bundle" (regexp-quote executable))
                           ": command not found")
                   (if error-regexp (list error-regexp))))
           (command-args
            (append (if (format-all--ruby-gem-bundled-p gem-name)
                        '("bundle" "exec"))
                    (cons executable (format-all--flatten-once args)))))
      (format-all--buffer-hard-rubocop-nari
       ok-statuses error-regexp root-files
       (car command-args)
       (cdr command-args))))

  (define-format-all-formatter rubocop
                               (:executable "rubocop")
                               (:install "gem install rubocop")
                               (:modes ruby-mode enh-ruby-mode)
                               (:format
                                (format-all--buffer-hard-ruby
                                 "rubocop" '(0 1) nil nil
                                 "rubocop"
                                 "--auto-correct"
                                 "--format" "quiet"
                                 ;; "--stderr"
                                 "--stdin"
                                 (or (buffer-file-name) (buffer-name))))))

(use-package! go-translate
  :commands (gts-do-translate)
  :config
  (let* ((auth-source-creation-prompts '((key . "API Key")))
         (found (nth 0(auth-source-search :host "deepl.com" :max 1 :create t)))
         (deepl-api-key (when found (plist-get found :key)))
         (engines (list
                   (gts-google-engine)
                   (gts-bing-engine))))

    (setq gts-translate-list '(("en" "ja") ("ja" "en")))
    (setq gts-default-translator
          (gts-translator
           :picker (gts-noprompt-picker)
           :engines (if deepl-api-key
                        (cons
                         (gts-deepl-engine :auth-key deepl-api-key :pro t)
                         engines)
                      engines)
           :render (gts-buffer-render)))))

;;
;;; Keybinds

(map! :leader
      :desc "multi-cursor" "M" #'hydra-multiple-cursors/body
      :desc "avy"    "A" #'hydra-avy/body
      :desc "window" "W" #'hydra-frame-window/body
      :desc "Work log New Entry" "l" (lambda() (interactive) (org-capture nil "l"))
      :desc "New entry" "n j j" #'org-journal-new-entry
      ;; #'+hydra/window-nav/body
      (:prefix-map ("D" . "debug")
       :desc "Start DAP debug" "d" #'dap-debug
       :desc "Hydra" "h" #'dap-hydra))
(map! :map minibuffer-local-map
      "M-;"               #'embark-act
      "C-c M-;"           #'embark-export)
(map! :map vertico-map
      "M-q" #'vertico-quick-insert
      "C-q" #'vertico-quick-exit)
;; delete character without yanking
(map! :n "x" #'delete-char)
