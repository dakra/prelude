;;; package --- personal  -*- lexical-binding: t -*-

;;; Commentary:
;;; Emacs config

;;; Code:

(require 'prelude-packages nil 'noerror)

(prelude-require-packages
 '(moe-theme
   ;;color-theme-sanityinc-solarized
   smart-mode-line-powerline-theme

   ;; coding major/minor modes
   graphviz-dot-mode
   ))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; If we start in daemon mode, load all modules straight away
;; and ensure that all packages are installed
(when (daemonp)
  (setq use-package-always-demand t)
  (setq use-package-always-ensure t)

  (global-set-key (kbd "C-x C-c") 'dakra-delete-frame-maybe-kill-buffer))

(setq user-full-name "Daniel Kraus"
      user-mail-address "daniel@kraus.my")

;; Always just use left-to-right text
(setq bidi-display-reordering nil)

(use-package whitespace
  :config
  ;; highlight lines with more than 110 characters
  (setq whitespace-line-column 110))

;; Only remove trailing whitespaces that I put there
(use-package ws-butler
  :commands ws-butler-mode
  :diminish ws-butler-mode
  :init
  (add-hook 'text-mode-hook 'ws-butler-mode)
  (add-hook 'prog-mode-hook 'ws-butler-mode))

;;(show-paren-mode t)
;;(setq show-paren-style 'expression)

(use-package which-key
  :config (which-key-mode 1)
  :diminish which-key-mode)

(use-package beacon
  :config (beacon-mode 1)
  :diminish beacon-mode)

(use-package lua-mode
  :mode "\\.lua\\'")
(use-package ng2-mode :defer t)
(use-package nginx-mode
  :mode ("/etc/nginx/conf.d/.*" "/etc/nginx/.*\\.conf\\'"))
(use-package litable  ; live preview for elisp
  :commands litable-mode)

(use-package pkgbuild-mode
  :mode "PKGBUILD\\'")


;; Debugging
(use-package realgud :defer t)


;; Type like a hacker
(use-package hacker-typer
  :commands (hacker-typer hackerman)
  :config (setq hacker-typer-remove-comments t))

(use-package color-identifiers-mode
  :disabled t  ; TODO: play with and see if I like it
  :commands global-color-identifiers-mode
  :init (add-hook 'after-init-hook 'global-color-identifiers-mode))

;; Goto last change
(use-package goto-chg
  :commands (goto-last-change goto-last-change-reverse)
  :bind (("C-c \\" . goto-last-change)
         ("C-c |" . goto-last-change-reverse)))


(require 'powerline)
(require 'moe-theme)
;; Show highlighted buffer-id as decoration. (Default: nil)
(setq moe-theme-highlight-buffer-id t)

(setq moe-theme-resize-markdown-title '(1.7 1.5 1.3 1.1 1.0 1.0))
(setq moe-theme-resize-org-title '(1.6 1.2 1.0 1.0 1.0 1.0 1.0 1.0 1.0))
(setq moe-theme-resize-rst-title '(1.7 1.5 1.3 1.1 1.0 1.0))

;; XXX: smart-mode-line theme is better?
;;(powerline-moe-theme)
(moe-dark)
(set-face-attribute 'mu4e-header-highlight-face nil :background "#626262" :foreground "#eeeeee")


;; save and restore buffer and cursor positions (but don't restore window layout)
;;(desktop-save-mode 1)
;;(setq desktop-restore-frames nil)

;; disable arrow keys to be forced to learn emacs
;;(setq guru-warn-only nil)

;; disable guru-mode completely
(setq prelude-guru nil)


;; imenu

;; Recenter window after imenu jump so cursor doesn't end up on the last line
(add-hook 'imenu-after-jump-hook 'recenter)  ; or 'reposition-window

;; eshell

;;(setq eshell-list-files-after-cd t)
;;(setq eshell-ls-initial-args "-alh")

;; We're in emacs, so 'cat' is nicer there than 'less'
(setenv "PAGER" "cat")

;; Fixme eshell-mode-map maps to global keybindings? Check "C-d"
;; Isssue: https://github.com/jwiegley/use-package/issues/332
(use-package eshell :ensure nil
  :commands eshell
  :bind (("C-x m" . eshell)
         ("C-x M" . dakra-eshell-always)
         ;;:map eshell-mode-map
         ;;("M-P" . eshell-previous-prompt)
         ;;("C-d" . ha/eshell-quit-or-delete-char)
         ;;("M-N" . eshell-next-prompt)
         ;;("M-R" . eshell-list-history)
         ;;("M-r" . dakra-eshell-read-history)
         )
  :config
  (defun dakra-eshell-always ()
    "Start a regular shell if you prefer that."
    (interactive)
    (eshell t))

  (require 'em-smart)
  (setq-default eshell-where-to-jump 'begin)
  (setq-default eshell-review-quick-commands nil)
  (setq-default eshell-smart-space-goes-to-end t)

  (require 'em-hist)
  ;; Some ideas from https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
  (setq-default eshell-scroll-to-bottom-on-input 'all
                eshell-error-if-no-glob t
                eshell-hist-ignoredups t
                ;;eshell-save-history-on-exit nil
                eshell-visual-commands '("ptpython" "ipython" "tail" "vi" "vim"
                                         "tmux" "screen" "top" "htop" "less" "more" "ncftp")
                eshell-prefer-lisp-functions nil)

  ;; (eshell/alias "e" "find-file-other-window $1")
  ;; (eshell/alias "emacs" "find-file $1")
  ;; (eshell/alias "ee" "find-file $1")

  ;; (eshell/alias "gd" "magit-diff-unstaged")
  ;; (eshell/alias "gds" "magit-diff-staged")

  ;; Show git info in prompt
  (use-package eshell-git-prompt
    :config ;;(eshell-git-prompt-use-theme 'powerline)
    ;; FIXME: Wait for powerline font
    (eshell-git-prompt-use-theme 'robbyrussell))

  (defun dakra-eshell-read-history ()
    (interactive)
    (insert
     (completing-read "Eshell history: "
                      (delete-dups
                       (ring-elements eshell-history-ring)))))

  ;; Used to C-d exiting from a shell? Want it to keep working, but still allow deleting a character?
  ;; We can have it both
  (require 'em-prompt)
  (defun ha/eshell-quit-or-delete-char (arg)
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp nil))
        (progn
          (eshell-life-is-too-much) ; Why not? (eshell/exit)
          (ignore-errors
            (progn
              ;; Remove frame if eshell is only window (otherwise just close window)
              (if (one-window-p)
                  (delete-frame)
                (delete-window)))))
      (delete-char arg)))

  ;; Fixme eshell-mode-map maps to global keybindings? Check "C-d"
  ;; Isssue: https://github.com/jwiegley/use-package/issues/332
  (add-hook 'eshell-mode-hook (lambda ()
                                (local-set-key (kbd "M-P") 'eshell-previous-prompt)
                                (local-set-key (kbd "M-N") 'eshell-next-prompt)
                                (local-set-key (kbd "M-R") 'eshell-list-history)
                                (local-set-key (kbd "M-r") 'dakra-eshell-read-history)
                                (local-set-key (kbd "C-d") 'ha/eshell-quit-or-delete-char)
                                ))
  ;; Functions starting with `eshell/' can be called directly from eshell
  ;; with only the last part. E.g. (eshell/foo) will call `$ foo'
  (defun eshell/d (&rest args)
    (dired (pop args) "."))

  (defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))   ;; The echo command suppresses output

  (defun eshell/f (filename &optional dir)
    "Searches in the current directory for files that match the
given pattern. A simple wrapper around the standard 'find'
function."
    (let ((cmd (concat
                "find " (or dir ".")
                "      -not -path '*/.git*'"
                " -and -not -path '*node_modules*'"
                " -and -not -path '*classes*'"
                " -and "
                " -type f -and "
                "-iname '" filename "'")))
      (message cmd)
      (shell-command-to-string cmd)))

  (defun eshell/ef (filename &optional dir)
    "Searches for the first matching filename and loads it into a
file to edit."
    (let* ((files (eshell/f filename dir))
           (file (car (s-split "\n" files))))
      (find-file file)))

  (defun eshell/find (&rest args)
    "Wrapper around the ‘find’ executable."
    (let ((cmd (concat "find " (string-join args))))
      (shell-command-to-string cmd)))

  (defun execute-command-on-file-buffer (cmd)
    (interactive "sCommand to execute: ")
    (let* ((file-name (buffer-file-name))
           (full-cmd (concat cmd " " file-name)))
      (shell-command full-cmd)))

  (defun execute-command-on-file-directory (cmd)
    (interactive "sCommand to execute: ")
    (let* ((dir-name (file-name-directory (buffer-file-name)))
           (full-cmd (concat "cd " dir-name "; " cmd)))
      (shell-command full-cmd)))
  )


(defun xah-paste-or-paste-previous ()
  "Paste. When called repeatedly, paste previous.
This command calls `yank', and if repeated, call `yank-pop'.

URL `http://ergoemacs.org/emacs/emacs_paste_or_paste_previous.html'
Version 2017-01-11"
  (interactive)
  (progn
    (when (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (if (eq real-last-command this-command)
        (yank-pop 1)
      (yank))))
;;(global-set-key (kbd "C-y") 'xah-paste-or-paste-previous)

;;; toggle narrow or widen (region or defun) with C-x n
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first.  Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

;; Associate more files with conf-mode
(use-package conf-mode :ensure nil
  :mode ("mbsyncrc\\'" "msmtprc\\'" "pylintrc\\'"))

;; Edit GNU gettext PO files
(use-package po-mode
  :mode ("\\.po\\'" "\\.po\\."))


(use-package emms
  :commands (emms emms-play-url emms-play-file emms-play-dired)
  :bind (:map dired-mode-map
         ("P" . emms-play-dired)))

(use-package emms-player-simple-mpv
  :after emms
  :config
  (require 'emms-player-simple-mpv-control-functions)
  (define-emms-simple-player-mpv my-mpv '(file url streamlist playlist)
    (concat "\\`\\(http[s]?\\|mms\\)://\\|"
            (apply #'emms-player-simple-regexp
                   "aac" "pls" "m3u"
                   emms-player-base-format-list))
    "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

  (emms-player-simple-mpv-add-to-converters
   'emms-player-my-mpv "." '(playlist)
   (lambda (track-name) (format "--playlist=%s" track-name)))

  (add-to-list 'emms-player-list 'emms-player-my-mpv)

  (require 'emms-player-simple-mpv-e.g.hydra)
  ;; Setting it to a global key bind would be useful.
  ;; (global-set-key (kbd "<f2> m") 'emms-player-simple-mpv-hydra/body)
  (defalias 'emms-mpv-hydra 'emms-player-simple-mpv-hydra/body)

  (require 'emms-player-simple-mpv-e.g.time-display)
  (require 'emms-player-simple-mpv-e.g.playlist-fname))

;; dired config mostly from https://github.com/Fuco1/.emacs.d/blob/master/files/dired-defs.org
(use-package dired :ensure nil
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; dired list size in human-readable format and list directories first
  (setq dired-listing-switches "-hal --group-directories-first")
  )

(use-package wdired :ensure nil
  :after dired
  :commands wdired-change-to-wdired-mode
  :bind (:map dired-mode-map
         ("C-c C-e" . wdired-change-to-dired-mode)))

(use-package dired-x :ensure nil
  :after dired
  :config
  (defconst my-dired-media-files-extensions
    '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg")
    "Media files.")

  (add-to-list 'dired-guess-shell-alist-user
               (list (concat "\\."
                             (regexp-opt my-dired-media-files-extensions)
                             "\\'")
                     "mpv")))

(use-package dired-rainbow
  :after dired
  :config
  (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
  (dired-rainbow-define xml "#b4fa70" ("xml" "xsd" "xsl" "xslt" "wsdl"))

  (dired-rainbow-define document font-lock-function-name-face ("doc" "docx" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub"))
  (dired-rainbow-define excel "#3465a4" ("xlsx"))
  (dired-rainbow-define media "#ce5c00" my-dired-media-files-extensions)
  (dired-rainbow-define image "#ff4b4b" ("jpg" "png" "jpeg" "gif"))

  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define sourcefile "#fcaf3e" ("py" "c" "cc" "cpp" "h" "java" "pl" "rb" "R"
                                              "php" "go" "rust" "js" "ts" "hs"))

  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#ad7fa8" ("zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar"
                                              "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#e6a8df" ("deb" "rpm"))
  (dired-rainbow-define encrypted "LightBlue" ("gpg" "pgp"))

  (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*"))

(use-package dired-collapse
  :after dired
  :commands dired-collapse-mode
  :init (add-hook 'dired-mode-hook 'dired-collapse-mode))

;; Browse compressed archives in dired (requires `avfs' to be installed)
(use-package dired-avfs
  :after dired)

(use-package dired-open
  :after dired
  :commands dired-open-file
  :bind (:map dired-mode-map
         ("RET" . dired-open-file)
         ([return] . dired-open-file)
         ("f" . dired-open-file))
  :config
  (setq dired-open-functions '(dired-open-by-extension dired-open-guess-shell-alist dired-open-subdir)))

(use-package dired-ranger
  :after dired
  :commands (dired-ranger-copy dired-ranger-paste dired-ranger-move
                               dired-ranger-bookmark dired-ranger-bookmark-visit)
  :init
  (bind-keys :map dired-mode-map
             :prefix "c"
             :prefix-map dired-ranger-map
             :prefix-docstring "Map for ranger operations."
             ("c" . dired-ranger-copy)
             ("p" . dired-ranger-paste)
             ("m" . dired-ranger-move))

  (bind-keys :map dired-mode-map
    ("'" . dired-ranger-bookmark)
    ("`" . dired-ranger-bookmark-visit)))

(use-package dired+
  :init
  ;; Show details by default  (diredp hides it)
  (setq diredp-hide-details-initially-flag nil)

  (diredp-toggle-find-file-reuse-dir 1)  ; reuse dired buffers
  (setq diredp-dwim-any-frame-flag t)

  :config
  ;; Remove stupid font-locking
  (setf (nth 3 diredp-font-lock-keywords-1)
        ;; Properly handle the extensions
        '("[^ .\\/]\\(\\.[^. /]+\\)$" 1 diredp-file-suffix))
  (setf (nth 4 diredp-font-lock-keywords-1)
        ;; Properly handle the extensions
        '("\\([^ ]+\\) -> .+$" 1 diredp-symlink))
  (setf (nth 6 diredp-font-lock-keywords-1)
        (list (concat
               "^  \\(.*\\("
               (concat
                (mapconcat
                 'regexp-quote
                 (or (and (boundp 'dired-omit-extensions)
                          dired-omit-extensions)
                     completion-ignored-extensions)
                 "[*]?\\|")
                "[*]?")        ; Allow for executable flag (*).
               "\\)\\)$") ; Do not treat compressed files as garbage... why the hell!
              1 diredp-ignored-file-name t))
  )

;; Display the recursive size of directories in Dired
(use-package dired-du
  :commands dired-du-mode
  :config
  ;; human readable size format
  (setq dired-du-size-format t))

;; Easily diff 2 marked files in dired
(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))
(define-key dired-mode-map "e" 'ora-ediff-files)

(use-package async
  :config (dired-async-mode 1))

(defun ora-dired-rsync (dest)
  (interactive
   (list
    (expand-file-name
     (read-file-name
      "Rsync to:"
      (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files
                nil current-prefix-arg))
        ;; the rsync command
        (tmtxt/rsync-command
         "rsync -arvz --progress "))
    ;; add all selected file names as arguments
    ;; to the rsync command
    (dolist (file files)
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument file)
                    " ")))
    ;; append the destination
    (setq tmtxt/rsync-command
          (concat tmtxt/rsync-command
                  (shell-quote-argument dest)))
    ;; run the async shell command
    (async-shell-command tmtxt/rsync-command "*rsync*")
    ;; finally, switch to that window
    (other-window 1)))
(define-key dired-mode-map "Y" 'ora-dired-rsync)

;; Switch on 'umlaut-mode' for easier Umlaut usage
(define-minor-mode umlaut-mode
  "A mode for conveniently using Umlauts in Emacs"
  nil
  :lighter " äöü"
  :keymap '(("\M-a" . (lambda () (interactive) (insert ?ä)))
            ("\M-o" . (lambda () (interactive) (insert ?ö)))
            ("\M-u" . (lambda () (interactive) (insert ?ü)))
            ("\M-s" . (lambda () (interactive) (insert ?ß)))
            ("\M-A" . (lambda () (interactive) (insert ?Ä)))
            ("\M-O" . (lambda () (interactive) (insert ?Ö)))
            ("\M-U" . (lambda () (interactive) (insert ?Ü)))
            ("\M-e" . (lambda () (interactive) (insert ?€)))
            ("\M-p" . (lambda () (interactive) (insert ?£)))
            ("\M-S" . (lambda () (interactive) (insert "SS")))))

;; Hydras

(use-package hydra
  :config
  (defhydra hydra-folding (:color red)
    "
  _o_pen node    _n_ext fold       toggle _f_orward
  _c_lose node   _p_revious fold   toggle _a_ll
  "
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("f" origami-forward-toggle-node)
    ("a" origami-toggle-all-nodes))

  (defhydra hydra-multiple-cursors (:hint nil)
    "
         ^Up^            ^Down^        ^Other^
    ----------------------------------------------
    [_p_]   Next    [_n_]   Next    [_l_] Edit lines
    [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
    [_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
    ^ ^             ^ ^             [_q_] Quit
    "
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil))

  (global-set-key
   (kbd "C-c C-s")
   (defhydra hydra-scratchpad (:hint nil)
     "
     _p_ython    _e_lisp        _s_ql
     _g_o        _j_avascript   _t_ypescript
     _r_ust      _R_est-client  _h_tml
     _o_rg-mode  _T_ext         _m_arkdown
     "
     ("p" (switch-to-buffer "*python*scratchpad.py"))
     ("e" (switch-to-buffer "*elisp*scratchpad.el"))
     ("s" (switch-to-buffer "*sql*scratchpad.sql"))
     ("g" (switch-to-buffer "*go*scratchpad.go"))
     ("j" (switch-to-buffer "*js*scratchpad.js"))
     ("t" (switch-to-buffer "*ts*scratchpad.ts"))
     ("r" (switch-to-buffer "*rust*scratchpad.rs"))
     ("R" (switch-to-buffer "*rest*scratchpad.rest"))
     ("h" (switch-to-buffer "*html*scratchpad.html"))
     ("o" (switch-to-buffer "*org*scratchpad.org"))
     ("T" (switch-to-buffer "*text*scratchpad.txt"))
     ("m" (switch-to-buffer "*markdown*scratchpad.md"))))

  (defun my/insert-unicode (unicode-name)
    "Same as C-x 8 enter UNICODE-NAME."
    (insert-char (cdr (assoc-string unicode-name (ucs-names)))))

  (global-set-key
   (kbd "C-x 9")
   (defhydra hydra-unicode (:hint nil)
     "
     Unicode  _c_ €   _a_ ä   _A_ Ä
              _d_ °   _o_ ö   _O_ Ö
              _e_ €   _u_ Ü   _U_ Ü
              _p_ £   _s_ ß
              _m_ µ
              _r_ →
     "
     ("a" (my/insert-unicode "LATIN SMALL LETTER A WITH DIAERESIS"))
     ("A" (my/insert-unicode "LATIN CAPITAL LETTER A WITH DIAERESIS"))
     ("o" (my/insert-unicode "LATIN SMALL LETTER O WITH DIAERESIS")) ;;
     ("O" (my/insert-unicode "LATIN CAPITAL LETTER O WITH DIAERESIS"))
     ("u" (my/insert-unicode "LATIN SMALL LETTER U WITH DIAERESIS")) ;;
     ("U" (my/insert-unicode "LATIN CAPITAL LETTER U WITH DIAERESIS"))
     ("s" (my/insert-unicode "LATIN SMALL LETTER SHARP S"))
     ("c" (my/insert-unicode "COPYRIGHT SIGN"))
     ("d" (my/insert-unicode "DEGREE SIGN"))
     ("e" (my/insert-unicode "EURO SIGN"))
     ("p" (my/insert-unicode "POUND SIGN"))
     ("r" (my/insert-unicode "RIGHTWARDS ARROW"))
     ("m" (my/insert-unicode "MICRO SIGN"))))

  (defhydra hydra-org-template (:color blue :hint nil)
    "
    _c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
    _l_atex   _E_xample   _p_ython        inde_x_:
    _a_scii   _v_erse     _P_yton session _I_NCLUDE:
    _s_rc     _n_ote      _i_python       _H_TML:
    _h_tml    ^ ^         plant_u_ml      _A_SCII:
    "
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("n" (let (text) ; org-reveal speaker notes
           (when (region-active-p)
             (setq text (buffer-substring (region-beginning) (region-end)))
             (delete-region (region-beginning) (region-end)))
           (insert "#+BEGIN_NOTES\n\n#+END_NOTES")
           (forward-line -1)
           (when text (insert text))))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("x" (hot-expand "<i"))
    ("e" (hot-expand "<s" "emacs-lisp"))
    ("t" (hot-expand "<s" "emacs-lisp :tangle yes"))
    ("p" (hot-expand "<s" "python"))
    ("P" (hot-expand "<s" "python" ":session :exports both"))
    ("i" (hot-expand "<s" "ipython"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))

  (defun hot-expand (str &optional mod header)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADERS: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end))
        (deactivate-mark))
      (when header (insert "#+HEADERS: " header))
      (insert str)
      (org-try-structure-completion)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (or (region-active-p) (looking-back "^"))
          (hydra-org-template/body)
        (self-insert-command 1))))
  (require 'org-link-edit)
  (defun jk/unlinkify ()
    "Replace an org-link with the description, or if this is absent, the path."
    (interactive)
    (let ((eop (org-element-context)))
      (when (eq 'link (car eop))
        (message "%s" eop)
        (let* ((start (org-element-property :begin eop))
               (end (org-element-property :end eop))
               (contents-begin (org-element-property :contents-begin eop))
               (contents-end (org-element-property :contents-end eop))
               (path (org-element-property :path eop))
               (desc (and contents-begin
                          contents-end
                          (buffer-substring contents-begin contents-end))))
          (setf (buffer-substring start end)
                (concat (or desc path)
                        (make-string (org-element-property :post-blank eop) ?\s)))))))

  (define-key org-mode-map (kbd "C-c )")
    (defhydra hydra-org-link-edit (:color red)
      "Org Link Edit"
      (")" org-link-edit-forward-slurp "forward slurp")
      ("}" org-link-edit-forward-barf "forward barf")
      ("(" org-link-edit-backward-slurp "backward slurp")
      ("{" org-link-edit-backward-barf "backward barf")
      ("r" jk/unlinkify "remove link")
      ("q" nil "cancel" :color blue)))
  )

;; Tramp config

;; Only for debugging slow tramp connections
;;(setq tramp-verbose 7)

;; Skip version control for tramp files
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Turn of auto-save for tramp files
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; Use ControlPath from .ssh/config
(setq tramp-ssh-controlmaster-options "")

;; See https://www.gnu.org/software/tramp/#Ad_002dhoc-multi_002dhops
;; For all hosts, except my local one, first connect via ssh, and then apply sudo -u root:
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))
(add-to-list 'tramp-default-proxies-alist
             '("localhost" nil nil))
;; add tramp proxy for atomx user
(add-to-list 'tramp-default-proxies-alist '(nil "atomx" "/ssh:%h:"))


;; Nicer mark ring navigation (C-x C-SPC or C-x C-Left/Right)
(use-package back-button
  :diminish back-button-mode
  :config (back-button-mode 1))

(use-package imenu-anywhere
  :bind (("M-i" . helm-imenu-anywhere)
         :map prelude-mode-map
         ("C-c i" . helm-imenu-anywhere))
  :config
  (defun imenu-use-package ()
    (add-to-list 'imenu-generic-expression
                 '("Packages" "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  (add-hook 'emacs-lisp-mode-hook #'imenu-use-package))

;;; Helm config
(use-package helm
  :commands (helm-imenu helm-resume helm-execute-persistent-action helm-select-action)
  :diminish helm-mode
  :bind (("C-x r b" . helm-filtered-bookmarks)  ; Use helm bookmarks
         ("C-c j" . helm-imenu)
         ("C-c C-r" . helm-resume)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)  ; Rebind tab to run persistent action
         ("C-i" . helm-execute-persistent-action)  ; Make TAB work in terminals
         ("C-z" . helm-select-action)  ; List actions
         )
  :config
  ;; keep follow-mode in between helm sessions once activated
  (setq helm-follow-mode-persistent t)

  ;; Smaller helm window
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 30)
  (helm-autoresize-mode 1)

  ;; Don't show details in helm-mini for tramp buffers
  (setq helm-buffer-skip-remote-checking t)

  (require 'helm-bookmark)
  ;; Show bookmarks (and create bookmarks) in helm-mini
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found))

  ;; helm "hacks" like better path expandsion
  (use-package helm-ext
    :config
    ;; Skip . and .. for non empty dirs
    (helm-ext-ff-enable-skipping-dots t)

    ;; Enable zsh/fish shell like path expansion
    (helm-ext-ff-enable-zsh-path-expansion t)
    (helm-ext-ff-enable-auto-path-expansion t)

    ;; Don't use minibuffer if there's something there already
    (helm-ext-minibuffer-enable-header-line-maybe t)))

(use-package helm-backup
  :commands (helm-backup-versioning helm-backup)
  :init
  (setq helm-backup-path "~/.emacs.d/.helm-backup")
  (add-hook 'after-save-hook 'helm-backup-versioning))

;; use swiper with helm backend for search
(use-package swiper-helm
  :bind ("\C-s" . swiper-helm))


;; wolfram alpha queries (M-x wolfram-alpha)
(use-package wolfram
  :config
  (setq wolfram-alpha-app-id "KTKV36-2LRW2LELV8"))


;; Autofill (e.g. M-x autofill-paragraph or M-q) to 80 chars (default 70)
;; set with 'custom' since it's buffer-local variable
(setq-default fill-column 80)
(setq comment-auto-fill-only-comments t)  ; Only auto-fill comments
;; Use auto-fill in all modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Increase fill-column for programming to 100
(defun dakra-prog-mode-init ()
  (setq fill-column 100))
(add-hook 'prog-mode-hook 'dakra-prog-mode-init)

(font-lock-add-keywords
 nil '(("\\<\\(\\(FIX\\(ME\\)?\\|XXX\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
        1 font-lock-warning-face t)))

;; Use 'C-c S' or 'M-s M-w' for 'eww-search-words' current region
(define-key prelude-mode-map (kbd "C-c S") nil)  ; remove default crux find-shell-init keybinding
(global-set-key (kbd "C-c S") 'eww-search-words)

(use-package browse-url :ensure nil
  :commands (browse-url browse-url-at-point)
  :bind (:map prelude-mode-map ("C-c u" . browse-url-at-point)))


(use-package eww :ensure nil
  :config (setq eww-search-prefix "https://google.com/search?q="))


;; comment-dwim-2 is a replacement for the Emacs' built-in command
;; comment-dwim which includes more comment features, including:
;; - commenting/uncommenting the current line (or region, if active)
;; - inserting an inline comment
;; - killing the inline comment
;; - reindenting the inline comment
;; comment-dwim-2 picks one behavior depending on the context but
;; contrary to comment-dwim can also be repeated several times to
;; switch between the different behaviors
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; Do action that normally works on a region to the whole line if no region active.
;; That way you can just C-w to copy the whole line for example.
(use-package whole-line-or-region
  :diminish whole-line-or-region-mode
  :config (whole-line-or-region-global-mode t))

(use-package projectile
  :config
  ;; Shorten the mode line
  (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))
  ;; Automatically switch python venv
  (add-hook 'projectile-after-switch-project-hook 'venv-projectile-auto-workon)
  ;; cache projectile project files
  ;; projectile-find-files will be much faster for large projects.
  ;; C-u C-c p f to clear cache before search.
  (setq projectile-enable-caching t))

;; this is already done in helm-projectile
;;(setq projectile-switch-project-action 'helm-projectile)


;; highlight indentations in python
(use-package highlight-indent-guides
  :init (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-auto-odd-face-perc 15)
  (setq highlight-indent-guides-auto-even-face-perc 15)
  (setq highlight-indent-guides-auto-character-face-perc 20))

;; activate character folding in searches i.e. searching for 'a' matches 'ä' as well
(setq search-default-mode 'char-fold-to-regexp)

;; Always display nice unicode fonts
(use-package unicode-fonts
  :disabled t  ;; Makes startup slow and not really necessary
  :commands unicode-fonts-setup
  :init (unicode-fonts-setup)
  :config (use-package persistent-soft))

;; emoji font
;; package ttf-symbola has to be installed
;; Just use "C-x 8 RET <type name>" insead
(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend))
;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)

(use-package company
  :demand t
  :diminish company-mode
  :bind (:map company-active-map
         ([return] . nil)
         ("RET" . nil)
         ;;("TAB" . company-complete-selection)
         ;;([tab] . company-complete-selection)
         ("C-j" . company-complete-selection))
  :config
  (setq company-idle-delay 0.2)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1)
  ;; Aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;;(setq company-dabbrev-downcase nil)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  ;;(setq company-tooltip-flip-when-above t)
  ;; start autocompletion only after typing
  (setq company-begin-commands '(self-insert-command))
  (global-company-mode 1)

  (use-package company-emoji
    :disabled t
    :config (add-to-list 'company-backends 'company-emoji))

  (use-package company-quickhelp
    :disabled t
    :config (company-quickhelp-mode 1))

  (use-package slime-company
    :config (slime-setup '(slime-fancy slime-company)))

  ;; Add yasnippet support for all company backends
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package company-insert-selected :ensure nil :load-path "repos/company-insert-selected"
  :after company
  :bind (:map company-active-map
         ("TAB" . company-select-first-then-next)
         ("<tab>" . company-select-first-then-next)
         ("<S-tab>" . company-select-previous-then-none)
         ("<backtab>" . company-select-previous-then-none))
  :config
  ;;(unbind-key "<return>" company-active-map)
  ;;(unbind-key "RET" company-active-map)

  (setq company-frontends '(company-insert-selected-frontend
                            company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  (setq company-selection-wrap-around t))
;; Rust
;; You may need installing the following packages on your system:
;; * rustc (Rust Compiler)
;; * cargo (Rust Package Manager)
;; * racer (Rust Completion Tool)
;; * rustfmt (Rust Tool for formatting code)
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (use-package flycheck-rust
    :commands flycheck-rust-setup
    :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (use-package cargo
    :commands cargo-minor-mode
    :init (add-hook 'rust-mode-hook #'cargo-minor-mode))
  (use-package racer
    :commands racer-mode
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)

    (add-hook 'racer-mode-hook #'company-mode)
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)))

;; C/C++
(use-package irony
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (use-package company-irony
    :after company
    :config (add-to-list 'company-backends 'company-irony))
  (use-package irony-eldoc
    :init (add-hook 'irony-mode-hook 'irony-eldoc)))

;; Automatically insert 'end' in ruby and Elixir
(use-package ruby-end
  :disabled t  ; not needed anymore?!
  :commands ruby-end-mode
  :config
  (add-to-list 'elixir-mode-hook
               (defun auto-activate-ruby-end-mode-for-elixir-mode ()
                 (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                      "\\(?:^\\|\\s-+\\)\\(?:do\\)")
                 (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
                 (ruby-end-mode +1))))

;; Elixir
(use-package elixir-mode
  :mode ("\\.ex\\'" "\\.exs\\'" "\\.elixir\\'")
  :config
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "fn" "end"
                   :when '(("SPC" "RET"))
                   :actions '(insert navigate))
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(sp-ruby-def-post-handler)
                   :actions '(insert navigate)))
  (use-package alchemist))


(use-package fabric
  :defer t)

(use-package fish-mode
  :mode "\\.fish\\'")

(use-package jira-markup-mode
  :mode ("\\.confluence\\'" "/itsalltext/.*jira.*\\.txt$"))

(use-package markdown-mode
  :mode (("/itsalltext/.*\\(gitlab\\|github\\).*\\.txt$" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode))
  :config
  ;; use pandoc with source code syntax highlighting to preview markdown (C-c C-c p)
  (setq markdown-command "pandoc -s --highlight-style pygments -f markdown_github -t html5"))

;; Some things don't work well with fish, just always use bash
(setq shell-file-name "/bin/sh")

;; always loop GIF images
(setq image-animate-loop t)

;; send alerts by default to D-Bus
(setq alert-default-style 'notifications)

;; let emacs work nicely with i3; i3-emacs is not on melpa; manually installed
;; used together with i3 keyboard shortcut (S-e) to `emacsclient -cn -e '(switch-to-buffer nil)`
(use-package i3
  :if (or (daemonp) window-system)
  :ensure nil
  :commands i3-command
  :bind (("C-x 2" . i3-split-vertically)
         ("C-x 3" . i3-split-horizontally))
  :init
  (defun i3-split-vertically ()
    "Like (split-window-vertically) but when in graphic mode
split via i3 and create a new Emacs frame."
    (interactive)
    (if (display-graphic-p)
        (progn (i3-command 0 "split vertical")
               (make-frame))
      (split-window-vertically)))
  (defun i3-split-horizontally ()
    "Like (split-window-horizontally) but when in graphic mode
split via i3 and create a new Emacs frame."
    (interactive)
    (if (display-graphic-p)
        (progn (i3-command 0 "split horizontal")
               (make-frame))
      (split-window-horizontally)))
  :config
  (use-package i3-integration
    :ensure nil
    :disabled t
    :config
    (i3-one-window-per-frame-mode-off)
    (i3-advise-visible-frame-list-off)))

(use-package frames-only-mode
  :if (or (daemonp) window-system)
  :config
  ;; Open (e)shell in new frame instead of the current one
  (add-to-list 'display-buffer-alist '("\\`\\*e?shell" display-buffer-pop-up-frame))
  ;; Set config because magit-commit-show-diff defaults to nil
  (setq frames-only-mode-configuration-variables
        (list (list 'pop-up-frames 'graphic-only)
              (list 'mouse-autoselect-window nil)
              (list 'focus-follows-mouse nil)
              (list 'frame-auto-hide-function 'delete-frame)
              (list 'org-agenda-window-setup 'other-frame)
              (list 'org-src-window-setup 'other-frame)
              (list 'ediff-window-setup-function 'ediff-setup-windows-plain)
              (list 'ido-default-buffer-method 'selected-window)
              ;;(list 'magit-commit-show-diff t)
              (list 'flycheck-display-errors-function #'frames-only-mode-flycheck-display-errors)))
  ;; Add function that calls (display-buffer) if you want to exclude it from frames-only-mode
  (add-to-list 'frames-only-mode-use-window-functions 'undo-tree-visualize)
  (add-to-list 'frames-only-mode-use-window-functions 'po-edit-string)
  (frames-only-mode))

(use-package edit-server
  :if (daemonp)
  :config
  (edit-server-start)
  (setq edit-server-url-major-mode-alist
        '(("reddit\\.com" . markdown-mode)
          ("github\\.com" . gfm-mode)
          ("gitlab\\.com" . gfm-mode)
          ("gitlab\\.paesslergmbh\\.de" . gfm-mode)
          ("jira.paesslergmbh.de" . jira-markup-mode))))


(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox-developer")


(setq sml/theme 'powerline)  ; smart-mode-line theme

(use-package docker
  :config (setq docker-keymap-prefix "C-c C-d"))
(use-package dockerfile-mode
  :mode "Dockerfile\\'")
(use-package docker-compose-mode
  :mode "docker-compose.*\.yml\\'")
(use-package docker-tramp)

;; Replace zap-to-char functionaity with the more powerful zop-to-char
(use-package zop-to-char
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package diff-hl
  :commands (turn-on-diff-hl-mode diff-hl-dired-mode diff-hl-magit-post-refresh)
  :init
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'conf-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; XXX: not sure if git gutter is really nicer than diff-hl
;; diff-hl comes pre-packaged with prelude but doesn't
;; have those *-hunk commands

;;;; disable diff-hl that's enabled in prelude-editor.el:393
;;(global-diff-hl-mode -1)
;;(remove-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
;;;; use git-gutter everywhere
;;(global-git-gutter-mode t)
;;(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;;(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;;(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)

;; allow horizontal scrolling with "M-x >"
(put 'scroll-left 'disabled nil)

(setq ffap-machine-p-known 'reject)  ; don't "ping Germany" when typing test.de<TAB>


(use-package origami
  :bind (:map origami-mode-map
              ("C-c C-o" . hydra-folding/body))
  :init (add-hook 'prog-mode-hook (lambda () (origami-mode))))
;;(require 'origami)
;;(define-key origami-mode-map (kbd "C-c C-o") 'origami-recursively-toggle-node)
;;(global-origami-mode)

;; auto kill buffer when closing window
(defun maybe-delete-frame-buffer (frame)
  "When a dedicated FRAME is deleted, also kill its buffer.
A dedicated frame contains a single window whose buffer is not
displayed anywhere else."
  (let ((windows (window-list frame)))
    (when (eq 1 (length windows))
      (let ((buffer (window-buffer (car windows))))
        (when (eq 1 (length (get-buffer-window-list buffer nil t)))
          (kill-buffer buffer))))))
;;(add-to-list 'delete-frame-functions #'maybe-delete-frame-buffer)

(defun dakra-delete-frame-maybe-kill-buffer (p)
  "Call (delete-frame) or (kill-buffer) and (delete-frame) when called with prefix argument."
  (interactive "P")
  (when p
    (maybe-delete-frame-buffer (selected-frame)))
  (delete-frame))

;; use outline-cycle (from outline-magic) in outline-minor-mode
;; Tried outshine (and navi) but seems buggy and all
;; features I don't use
(use-package outline-magic
  ;;:disabled nil  ; outshine is the newer version
  :commands outline-cycle
  :bind (:map outline-minor-mode-map ("<C-tab>" . outline-cycle))
  :init
  (add-hook 'message-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

  ;; https://stackoverflow.com/questions/4079648/combine-python-mode-with-org-mode-for-emacs/29057808#29057808
  (defun python-mode-outline-hook ()
    (outline-minor-mode)
    (setq outline-level 'python-outline-level)
    (setq outline-regexp
          (rx (or
               ;; Commented outline heading
               (group
                (* space)  ; 0 or more spaces
                (one-or-more (syntax comment-start))
                (one-or-more space)
                ;; Heading level
                (group (repeat 1 8 "\*"))  ; Outline stars
                (one-or-more space))

               ;; Python keyword heading
               (group
                ;; Heading level
                (group (* space)) ; 0 or more spaces
                bow
                ;; Keywords
                (or "class" "def" "else" "elif" "except" "for" "if" "try" "while")
                eow)))))

  (defun python-outline-level ()
    (or
     ;; Commented outline heading
     (and (string-match (rx
                         (* space)
                         (one-or-more (syntax comment-start))
                         (one-or-more space)
                         (group (one-or-more "\*"))
                         (one-or-more space))
                        (match-string 0))
          (- (match-end 0) (match-beginning 0)))

     ;; Python keyword heading, set by number of indentions
     ;; Add 8 (the highest standard outline level) to every Python keyword heading
     (+ 8 (- (match-end 0) (match-beginning 0)))))

  (add-hook 'python-mode-hook 'python-mode-outline-hook)
  )

(use-package navi-mode
  :disabled t
  :commands navi-mode)
(use-package outshine
  :disabled t
  :bind (:map outline-minor-mode-map ("<C-tab>" . outline-cycle))
  :init
  ;;(defvar outline-minor-mode-prefix "\M-#")

  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)

  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'clojure-mode-hook 'outline-minor-mode)
  (add-hook 'ledger-mode-hook 'outline-minor-mode)
  (add-hook 'message-mode-hook 'outline-minor-mode)

  (defun restclient-mode-outline-hook ()
    (outline-minor-mode)
    (setq outline-regexp "#+"))
  (add-hook 'restclient-mode-hook 'restclient-mode-outline-hook)

  ;; https://stackoverflow.com/questions/4079648/combine-python-mode-with-org-mode-for-emacs/29057808#29057808
  (defun python-mode-outline-hook ()
    (outline-minor-mode)
    (setq outline-level 'python-outline-level)
    (setq outline-regexp
          (rx (or
               ;; Commented outline heading
               (group
                (* space)  ; 0 or more spaces
                (one-or-more (syntax comment-start))
                (one-or-more space)
                ;; Heading level
                (group (repeat 1 8 "\*"))  ; Outline stars
                (one-or-more space))

               ;; Python keyword heading
               (group
                ;; Heading level
                (group (* space)) ; 0 or more spaces
                bow
                ;; Keywords
                (or "class" "def" "else" "elif" "except" "for" "if" "try" "while")
                eow)))))

  (defun python-outline-level ()
    (or
     ;; Commented outline heading
     (and (string-match (rx
                         (* space)
                         (one-or-more (syntax comment-start))
                         (one-or-more space)
                         (group (one-or-more "\*"))
                         (one-or-more space))
                        (match-string 0))
          (- (match-end 0) (match-beginning 0)))

     ;; Python keyword heading, set by number of indentions
     ;; Add 8 (the highest standard outline level) to every Python keyword heading
     (+ 8 (- (match-end 0) (match-beginning 0)))))

  (add-hook 'python-mode-hook 'python-mode-outline-hook)
  ;;:config
  ;; Call `outshine-speed-command-help' to get an overview of speed commands
  ;;(setq outshine-use-speed-commands t)
  )

(use-package cython-mode
  :mode ("\\.pyd\\'" "\\.pyi\\'" "\\.pyx\\'")
  :config
  (use-package flycheck-cython))


;; SQL
(use-package sql
  :config
  (sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\|mysql\\) ?\\[?[_a-zA-Z0-9]*\\]?> ")

  (setq sql-product 'mysql)
  (setq sql-connection-alist
        '((atomx-local-api (sql-product 'mysql)
                           (sql-server "localhost")
                           (sql-user "root")
                           (sql-database "api"))
          (atomx-remote-api (sql-product 'mysql)
                            (sql-server "127.0.0.1")
                            (sql-port 3307)
                            (sql-user "root")
                            (sql-database "api")
                            (sql-mysql-options '("-A")))
          (paessler-docker (sql-product 'mysql)
                           (sql-server "127.0.0.1")
                           (sql-port 3308)
                           (sql-user "root")
                           (sql-database "paessler_com2"))))

  (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))

  (defun dakra/sql-atomx-local-api ()
    (interactive)
    (dakra/sql-connect 'mysql 'atomx-local-api))

  (defun dakra/sql-atomx-remote-api ()
    (interactive)
    (dakra/sql-connect 'mysql 'atomx-remote-api))

  (defun dakra/sql-paessler-docker ()
    (interactive)
    (dakra/sql-connect 'mysql 'paessler-docker))

  (defun dakra/sql-connect (product connection)
    ;; load the password
    (require 'dakra-passwords "~/.emacs.d/personal/dakra-passwords.el.gpg")

    ;; update the password to the sql-connection-alist
    (let ((connection-info (assoc connection sql-connection-alist))
          (sql-password (car (last (assoc connection dakra-sql-passwords)))))
      (delete sql-password connection-info)
      (nconc connection-info `((sql-password ,sql-password)))
      (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
      (add-to-list 'sql-connection-alist connection-info))

    ;; connect to database
    (setq sql-product product)
    (sql-connect connection)
    (rename-buffer (format "*SQL-%s*" connection)))

  (setq sql-mysql-login-params
        '((user :default "daniel")
          (database :default "api")
          (server :default "localhost")))

  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)
              ))
  (add-hook 'sql-mode
            (lambda ()
              (setq sql-set-product 'mysql))))

;; Smart indentation for SQL files
(use-package sql-indent :ensure nil :load-path "repos/emacs-sql-indent"
  :commands sqlind-setup
  :init (add-hook 'sql-mode-hook 'sqlind-setup))

;; Capitalize keywords in SQL mode
(use-package sqlup-mode
  :commands sqlup-mode
  :init
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
  :config
  ;; Don't capitalize `name` or 'type' keyword
  (add-to-list 'sqlup-blacklist "name")
  (add-to-list 'sqlup-blacklist "type"))


(use-package emmet-mode
  :commands emmet-mode
  :bind (:map emmet-mode-keymap
              ("<backtab>" . emmet-expand-line)
              ("\C-c TAB" . emmet-expand-line)
              ("C-M-p" . emmet-prev-edit-point)
              ("C-M-n" . emmet-next-edit-point))
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  :config
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-move-cursor-after-expanding t)

  (use-package helm-emmet))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package scss-mode
  :commands scss-mode
  :config
  ;;(setq css-indent-offset 2)
  (rainbow-mode +1)
  ;; turn off annoying auto-compile on save
  (setq scss-compile-at-save nil))

(use-package sass-mode
  :mode ("\\.sass\\'"))

;;; python

;; FIXME: change stuff from prelude-python.el to here
(use-package python
  :mode (("\\.py\\'" . python-mode)
         ("\\.xsh\\'" . python-mode))  ; Xonsh script files
  :interpreter ("python" . python-mode)
  :config
  (defhydra hydra-python-test (python-mode-map "C-c C-t" :color blue)
    "Run Python Tests"
    ("f" python-test-function "Function")
    ("m" python-test-method "Method")
    ("c" python-test-class "Class")
    ("F" python-test-file "File")
    ("p" python-test-project "Project")
    ("q" nil "Cancel"))
  (define-key python-mode-map (kbd "C-c C-t") 'hydra-python/body)

  (defun py-isort-add-import-whole-line-or-region ()
    "Import module(s) from region or whole line."
    (interactive)
    (whole-line-or-region-call-with-region 'py-isort-add-import-region))

  (defhydra hydra-python (python-mode-map "C-c C-p" :color blue :hint nil)
    "
           ^Tests^           ^Import^                ^Other^
    -------------------------------------------------------
    [_F_]   Function    [_f_] From ... import     [_P_] Run python
    [_m_]   Method      [_i_] Import              [_I_] Pippel
    [_c_]   Class       [_l_] Import line/region
    [_t_]   File        [_r_] Remove imports
    [_p_]   Project     [_s_] Sort imports        [_q_] Cancel
    "
    ("f" py-isort-add-from-import)
    ("i" py-isort-add-import)
    ("l" py-isort-add-import-whole-line-or-region)
    ("r" py-isort-remove-import)
    ("s" py-isort-buffer)

    ("F" python-test-function)
    ("m" python-test-method)
    ("c" python-test-class)
    ("t" python-test-file)
    ("p" python-test-project)

    ("P" run-python)
    ("I" pippel-list-packages)
    ("q" nil))
  (define-key python-mode-map (kbd "C-c C-p") 'hydra-python/body)

  :init
  (add-hook 'python-mode-hook
            '(lambda ()
               (setq-local imenu-create-index-function
                           #'python-imenu-create-flat-index)

               (subword-mode)
               (anaconda-mode)
               (anaconda-eldoc-mode)
               ))
  ;;(add-hook 'python-mode-hook 'subword-mode)
  ;;(add-hook 'python-mode-hook 'which-function-mode)
  ;;(add-hook 'python-mode-hook 'anaconda-mode)
  ;; Enable eldoc for python
  ;;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  )

(use-package anaconda-mode  ; :ensure nil :load-path "repos/anaconda-mode"
  :commands (anaconda-mode anaconda-eldoc-mode)
  :config
  (use-package company-anaconda
    :config (add-to-list 'company-backends 'company-anaconda))
  :diminish anaconda-mode)


;; package-list-packages like interface for python packages
(use-package pippel
  :commands (pippel-list-packages pippel-install-package))

;; Syntax highlighting for requirements.txt files
(use-package pip-requirements
  :mode (("\\.pip\\'" . pip-requirements-mode)
         ("requirements.*\\.txt\\'" . pip-requirements-mode)
         ("requirements\\.in" . pip-requirements-mode)))

(use-package sphinx-mode
  :commands sphinx-mode)


(use-package python-test :defer t  ;; :load-path "repos/python-test.el"
  :init (setq python-test-backend 'pytest))


;; Enable (restructured) syntax highlighting for python docstrings
(use-package python-docstring
  :commands python-docstring-mode
  :init (add-hook 'python-mode-hook 'python-docstring-mode)
  :diminish python-docstring-mode)

(use-package pydoc
  :bind (:map anaconda-mode-map
              ("M-?" . pydoc-at-point))
  :commands (pydoc-at-point pydoc-browse))

;; Automatically sort and format python imports
(use-package py-isort  :ensure nil :load-path "repos/py-isort.el"
  :commands (py-isort-buffer py-isort-region)
  :config
  ;;(add-hook 'before-save-hook 'py-isort-before-save)
  (setq py-isort-options '("--line-width=100"
                           "--multi-line=3"
                           "--trailing-comma"
                           "--force-grid-wrap"
                           "--thirdparty=rethinkdb")))

;; accept 'UTF-8' (uppercase) as a valid encoding in the coding header
(define-coding-system-alias 'UTF-8 'utf-8)

(use-package restclient
  :commands restclient-mode
  :mode ("\\.rest\\'" . restclient-mode)
  :init
  (defun restclient-mode-outline-hook ()
    (outline-minor-mode)
    (setq outline-regexp "##+"))
  (add-hook 'restclient-mode-hook 'restclient-mode-outline-hook)
  :config
  (use-package restclient-helm)
  (use-package company-restclient
    :after company
    :config (add-to-list 'company-backends 'company-restclient)))

;; activate virtualenv for flycheck
;; (from https://github.com/lunaryorn/.emacs.d/blob/master/lisp/flycheck-virtualenv.el)
(require 'flycheck)

(declare-function python-shell-calculate-exec-path "python")

(defun flycheck-virtualenv-executable-find (executable)
  "Find an EXECUTABLE in the current virtualenv if any."
  (if (bound-and-true-p python-shell-virtualenv-root)
      (let ((exec-path (python-shell-calculate-exec-path)))
        (executable-find executable))
    (executable-find executable)))

(defun flycheck-virtualenv-setup ()
  "Setup Flycheck for the current virtualenv."
  (setq-local flycheck-executable-find #'flycheck-virtualenv-executable-find))

(add-hook 'python-mode-hook #'flycheck-virtualenv-setup)

;;; XXX: Wait for official mypy support
;;; See https://github.com/flycheck/flycheck/pull/1080
(add-to-list 'flycheck-checkers 'python-mypy)

;;;; mypy support
(flycheck-def-option-var flycheck-python-mypy-use-python-2 nil (python-mypy)
  "Whether to pass --py2 to mypy."
  :type 'boolean
  :safe #'booleanp
  :package-version '("flycheck" . "30"))

(flycheck-def-option-var flycheck-python-mypy-silent-imports nil (python-mypy)
  "Whether to disable type-checking of imported modules."
  :type 'boolean
  :safe #'booleanp
  :package-version '("flycheck" . "30"))

(flycheck-define-checker python-mypy
  "A Python type checker using mypy.
See URL `http://www.mypy-lang.org/'."
  :command ("/home/daniel/.virtualenvs/atomx/bin/mypy"
            "--shadow-file" source-original source
            (option-flag "--py2" flycheck-python-mypy-use-python-2)
            (option-flag "--ignore-missing-imports" flycheck-python-mypy-silent-imports)
            "--check-untyped-defs"
            "--warn-redundant-casts"
            "--warn-unused-ignores"
            "--hide-error-context"
            "--strict-optional"
            "--follow-imports=skip"
            source-original)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" (optional column ":")
            " error:" (message) line-end))
  :next-checkers (python-flake8)
  :modes python-mode)


;; FIXME: set in python2 projects?
;;(setq flycheck-python-mypy-use-python-2 t)

;; Ignore import errors that don't have typings
(setq flycheck-python-mypy-silent-imports t)

;; use both pylint and flake8 in flycheck
;;(flycheck-add-next-checker 'python-flake8 'python-pylint 'python-mypy)
;;(flycheck-add-next-checker 'python-flake8 'python-mypy)

;; XXX: Disable mypy?
;;(add-to-list 'flycheck-disabled-checkers 'python-mypy)

(setq flycheck-flake8-maximum-line-length 110)

;; ipython5 uses prompt_toolkit which doesn't play nice with emacs
;; when setting interpreter to 'ipython', you need additional '--simple-prompt' arg
(setq python-shell-interpreter "python")
;;(setq python-shell-interpreter-args "-i")
;; FIXME: run new python interpreter on projectile-switch-project?
;; and only run pshell when it's a pyramid project.
;;(setq python-shell-interpreter "python"
;;      python-shell-interpreter-args "--simple-prompt -i /home/daniel/.virtualenvs/atomx/lib/python3.5/site-packages/pyramid/scripts/pshell.py /home/daniel/atomx/api/development.ini")

(use-package virtualenvwrapper :ensure nil :load-path "repos/virtualenvwrapper.el"
  :commands (venv-workon venv-projectile-auto-workon)
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) ;; if you want eshell support
  (setq venv-location "/home/daniel/.virtualenvs/")
  ;;(venv-workon '"atomx")  ; default venv after a starting emacs
  )


(defcustom python-autopep8-path (executable-find "autopep8")
  "autopep8 executable path."
  :group 'python
  :type 'string)

(defun python-autopep8 ()
  "Automatically formats Python code to conform to the PEP 8 style guide.
$ autopep8 --in-place --aggressive --aggressive <filename>"
  (interactive)
  (when (eq major-mode 'python-mode)
    (shell-command
     (format "%s --in-place --max-line-length %s --aggressive %s" python-autopep8-path
             whitespace-line-column
             (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))


;; importmagic
;; FIXME: very buggy yet 15.12.2016
;; importmagic itself buggy: https://github.com/alecthomas/importmagic
;; Always reorder imports; No way to put each import on a new line..
;; maybe always call py-isort after calling importmagic?
;;(require 'importmagic)
;;(add-hook 'python-mode-hook 'importmagic-mode)
;;(define-key importmagic-mode-map (kbd "C-c C-i") 'importmagic-fix-symbol-at-point)
;;(add-to-list 'helm-boring-buffer-regexp-list "\\*epc con")


;; disable auto escape quote feature of smartparens
;;(setq sp-escape-quotes-after-insert nil
;;      sp-escape-wrapped-region nil)

;; open current line/region/dired/commit in github
(use-package browse-at-remote
  :init
  (defun dakra-browse-at-remote (p)
    "Like (browse-at-remote) but when called will also copy the url
in the kill ring and when called with one prefix argument
copy the url in the kill ring instead of opening in the brower
and when called with 2 prefix arguments only open in browser."
    (interactive "P*")
    (if (eq (car p) 4)
        (browse-at-remote-kill)
      (if (eq (car p) 16)
          (browse-at-remote)
        (browse-at-remote-kill)
        (browse-at-remote))))
  :bind (:map prelude-mode-map ("C-c G" . dakra-browse-at-remote))
  :config
  (add-to-list 'browse-at-remote-remote-type-domains '("gitlab.paesslergmbh.de" . "gitlab"))
  (setq browse-at-remote-prefer-symbolic nil))

(use-package with-editor
  :commands (with-editor-export-editor)
  :init
  ;; Use local Emacs instance as $EDITOR (e.g. in `git commit' or `crontab -e')
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'term-mode-hook   'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor))

;; FIXME: find another gh lib. only works for public repos and unmaintained
;; just type 'fixes #' and get github issue autocompletion
(use-package github-issues
  :disabled t
  :defer t
  :init (add-hook 'git-commit-mode-hook 'git-commit-insert-issue-mode))

;; Nicer diff (should be taken from global .config/git/config)
(setq vc-git-diff-switches '("--indent-heuristic"))

(use-package ediff :ensure nil
  :config
  ;; Split ediff windows horizontally by default
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package magit
  :defines (magit-ediff-dwim-show-on-hunks)
  :config
  ;; Always highlight word differences in diff
  (setq magit-diff-refine-hunk 'all)

  ;; Only show 2 ediff panes
  (setq magit-ediff-dwim-show-on-hunks t)

  ;; Display magit status in full fram
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package gist
  :commands (gist-list gist-fork gist-fetch gist-buffer gist-buffer-private
                       gist-region gist-region-private gist-list gist-list-starred))

;; use magithub instead
;; github pull request support for magit
(use-package magit-gh-pulls
  :disabled t
  :defer t
  :init (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package magithub
  :disabled t  ; doesn't work to well yet. 'api not responding'. 7.8.2017
  :after magit
  :config
  (setq magithub-api-timeout 5)
  (magithub-feature-autoinject t)

  ;; Fix for emacs 26
  (defun ghubp--post-process (object &optional preserve-objects) object))


(use-package helpful
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-c h f" . helpful-function)
         ("C-c h v" . helpful-variable)
         ("C-c h c" . helpful-command)
         ("C-c h m" . helpful-macro)))

(use-package symbol-overlay
  :commands symbol-overlay-mode
  :init
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (setq symbol-overlay-temp-in-scope t)
  :bind (("C-c s" . symbol-overlay-put)
         :map symbol-overlay-mode-map
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         :map symbol-overlay-map
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("C-c C-s r" . symbol-overlay-rename)
         ("C-c C-s k" . symbol-overlay-remove-all)
         ("C-c C-s q" . symbol-overlay-query-replace)
         ("C-c C-s t" . symbol-overlay-toggle-in-scope)
         ("C-c C-s n" . symbol-overlay-jump-next)
         ("C-c C-s p" . symbol-overlay-jump-prev))
  :config
  (setq symbol-overlay-map (make-sparse-keymap))  ; Remove all default bindings
  ;;(set-face-background 'symbol-overlay-temp-face "gray30")
  :diminish symbol-overlay-mode)


;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g p" . dumb-jump-back)
         ("M-g q" . dumb-jump-quick-look)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm))

;; change `find-file` so all files that belong to root are opened as root
;; too often unintentional changes. just use 'M-x crux-sudo-edit' when needed
;;(crux-reopen-as-root-mode)

;; ledger-mode for bookkeeping
(defun ledger-mode-outline-hook ()
  (outline-minor-mode)
  (setq outline-regexp "[#;]+"))

(use-package hledger-mode
  ;;:disabled t  ;; Think ledger-mode is better.. needs more experimenting
  ;;:mode "\\.ledger\\'"
  :commands (hledger-mode hledger-jentry hledger-run-command)
  :bind (:map hledger-mode-map
         ("C-c e" . hledger-jentry)
         ("C-c j" . hledger-run-command)
         ("M-p" . hledger/prev-entry)
         ("M-n" . hledger/next-entry))
  :init (add-hook 'hledger-mode-hook 'ledger-mode-outline-hook)
  :config
  (setq hledger-jfile "/home/daniel/cepheus/finances.ledger")
  ;; Auto-completion for account names
  (add-to-list 'company-backends 'hledger-company)

  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry)))

(use-package ledger-mode
  ;;:disabled t  ;; try hledger
  :mode "\\.ledger\\'"
  :init
  ;; http://unconj.ca/blog/using-hledger-with-ledger-mode.html
  ;; Required to use hledger instead of ledger itself.
  (setq ledger-mode-should-check-version nil
        ledger-report-links-in-register nil
        ledger-binary-path "hledger")

  (add-hook 'ledger-mode-hook 'ledger-mode-outline-hook)
  :config
  (setq ledger-use-iso-dates t)  ; Use YYYY-MM-DD format

  (add-to-list 'ledger-reports
               (list "monthly expenses"
                     (concat "%(binary) -f %(ledger-file) balance expenses "
                             "--tree --no-total --row-total --average --monthly")))
  ;; disable whitespace-mode in ledger reports
  (add-hook 'ledger-report-mode-hook (lambda () (whitespace-mode -1)))
  (setq ledger-post-amount-alignment-column 60))


;;; Lisp in python vm
(use-package hy-mode
  :mode "\\.hy\\'"
  :init
  (add-hook 'hy-mode-hook 'paredit-mode)
  (add-hook 'hy-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'hy-mode-hook #'aggressive-indent-mode))

(use-package aggressive-indent
  :commands aggressive-indent-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'lisp-mode #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode)
  (add-hook 'js2-mode-hook #'aggressive-indent-mode))

(use-package easy-escape
  :commands easy-escape-minor-mode
  :diminish easy-escape-minor-mode
  :init
  ;; Nicer elisp regex syntax highlighting
  (add-hook 'lisp-mode-hook 'easy-escape-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'easy-escape-minor-mode))

;; From: https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
;; redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same
         ;; line as calculate-lisp-indent-last-sexp.  Note that first
         ;; thing on that line has to be complete sexp since we are
         ;; inside the innermost containing sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (goto-char indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (goto-char orig-point)
                (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))


;; octave
(use-package octave :ensure nil
  :mode ("\\.m\\'" . octave-mode)
  :interpreter ("octave" . octave-mode)
  :bind (:map octave-mode-map ("C-x C-e" . octave-send-region-or-line))
  :config
  (setq octave-block-offset 4)
  (defun octave-send-region-or-line ()
    (interactive)
    (if (region-active-p)
        (octave-send-region (region-beginning) (region-end))
      (octave-send-line)))
  (add-hook 'octave-mode-hook
            (lambda ()
              (abbrev-mode 1)
              (auto-fill-mode 1)
              (show-paren-mode 1)
              ;;(define-key octave-mode-map (kbd "C-x C-e") 'octave-send-region-or-line)
              (eldoc-mode 1)
              (if (eq window-system 'x)
                  (font-lock-mode 1)))))


(use-package prettier-js
  :commands prettier-js
  ;;:init (add-hook 'js2-mode-hook (lambda () (add-hook 'before-save-hook 'prettier-before-save)))
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "all"
                           ;;"--tab-width" "4"
                           "--single-quote" "true"
                           "--bracket-spacing" "false"
                           ))
  ;; prettier "--print-width" argument is read from 'fill-column' variable
  (setq prettier-js-width-mode 'fill))

(use-package json-mode
  :mode "\\.json\\'")

(use-package js2-mode
  :mode ("\\.js\\'" "\\.pac\\'" "\\.node\\'")
  :config
  ;; electric-layout-mode doesn't play nice with smartparens
  (setq-local electric-layout-rules '((?\; . after)))
  (setq mode-name "JS2")
  (js2-imenu-extras-mode +1)

  (setq js2-basic-offset 2)  ; set javascript indent to 2 spaces
  )

;; Connect to chrome
;; chromium --remote-debugging-port=9222 https://localhost:3000
;; then in emacs
;; M-x indium-connect-to-chrome

;; or node
;; node --inspect myfile.js
;; node with breakpoint at first line
;; node --inspect --debug-brk myfile.js
;; then open the url that node prints:
;; chrome-devtools://inspector.html?...&ws=127.0.0.1:PORT/PATH
;; then in emacs:
;; M-x indium-connect-to-nodejs RET 127.0.0.1 RET PORT RET PATH, PORT, PATH

;; place `.indium' file in static root folder.

(use-package indium
  :commands indium-interaction-mode
  :init (add-hook 'js-mode-hook #'indium-interaction-mode)
  :config (setq indium-update-script-on-save t))

(use-package js2-refactor
  :commands js2-refactor-mode
  :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (define-key js2-refactor-mode-map (kbd "C-c r")
    (defhydra js2-refactor-hydra (:color blue :hint nil)
      "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
[_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
[_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp
[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
[_q_]  quit"
      ("ee" js2r-expand-node-at-point)
      ("cc" js2r-contract-node-at-point)
      ("ef" js2r-extract-function)
      ("em" js2r-extract-method)
      ("tf" js2r-toggle-function-expression-and-declaration)
      ("ta" js2r-toggle-arrow-function-and-expression)
      ("ip" js2r-introduce-parameter)
      ("lp" js2r-localize-parameter)
      ("wi" js2r-wrap-buffer-in-iife)
      ("ig" js2r-inject-global-in-iife)
      ("ag" js2r-add-to-globals-annotation)
      ("ev" js2r-extract-var)
      ("iv" js2r-inline-var)
      ("rv" js2r-rename-var)
      ("vt" js2r-var-to-this)
      ("ao" js2r-arguments-to-object)
      ("ti" js2r-ternary-to-if)
      ("sv" js2r-split-var-declaration)
      ("ss" js2r-split-string)
      ("uw" js2r-unwrap)
      ("lt" js2r-log-this)
      ("dt" js2r-debug-this)
      ("sl" js2r-forward-slurp)
      ("ba" js2r-forward-barf)
      ("k" js2r-kill)
      ("q" nil)
      )))

;; use tern for js autocompletion
(use-package tern
  :disabled t  ; We use tide (typescript) also for javascript files
  :commands tern-mode
  :init (add-hook 'js-mode-hook 'tern-mode)
  :config
  (use-package company-tern
    :config
    (setq company-tern-property-marker "")  ; don't show circles for properties
    (add-to-list 'company-backends 'company-tern)))

(use-package skewer-mode
  :disabled t  ; Use indium
  :commands skewer-mode
  :init
  (setq httpd-port 8079)  ; set port for simple-httpd used by skewer
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))


;; TypeScript
(use-package tide
  :commands (setup-tide-mode tide-mode)
  :init
  (setq typescript-indent-level 2)
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;;(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))  ; add tide yasnippets as company backend
    ;;(company-mode +1)
    )
  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  ;; https://www.reddit.com/r/emacs/comments/68zacv/using_tidemode_to_typecheck_javascript/
  (add-hook 'js2-mode-hook #'setup-tide-mode)

  :config
  ;; formats the buffer before saving
  ;; FIXME: auto indent doesn't respect editorconfig
  ;;(add-hook 'before-save-hook 'tide-format-before-save)
  ;; format options
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
  ;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options
  )


(use-package undo-tree
  :demand t
  :bind ("C-z" . undo-tree-undo)  ;; Don't (suspend-frame)
  :config
  (setq undo-tree-visualizer-timestamps t)  ; show timestamps in undo-tree
  ;;(setq undo-tree-visualizer-diff t)

  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)

  (global-undo-tree-mode)
  :diminish undo-tree-mode)


;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; Paste with middle mouse button doesn't move the curser
(setq mouse-yank-at-point t)


;; Smart region guesses what you want to select by one command:
;; - If you call this command multiple times at the same position, it
;;   expands the selected region (with `er/expand-region').
;; - Else, if you move from the mark and call this command, it selects
;;   the region rectangular (with `rectangle-mark-mode').
;; - Else, if you move from the mark and call this command at the same
;;   column as mark, it adds a cursor to each line (with `mc/edit-lines').
(use-package smart-region
  ;; C-SPC is smart-region
  :bind (([remap set-mark-command] . smart-region)))


;; "C-=" is not valid ascii sequence in terminals
;;(global-set-key (kbd "C-@") 'er/expand-region)

(use-package selected
  :demand t
  :commands selected-minor-mode
  :init (defvar selected-org-mode-map (make-sparse-keymap))
  :config (selected-global-mode)
  :bind (
         :map selected-keymap
         ("q" . selected-off)
         ("u" . upcase-region)
         ("d" . downcase-region)
         ("w" . count-words-region)
         ("m" . apply-macro-to-region-lines)
         ;; multiple cursors
         ("a" . mc/mark-all-dwim)
         ("A" . mc/mark-all-like-this)
         ("m" . mc/mark-more-like-this-extended)
         ("p" . mc/mark-previous-like-this)
         ("P" . mc/unmark-previous-like-this)
         ("n" . mc/mark-next-like-this)
         ("N" . mc/unmark-next-like-this)
         ("r" . mc/edit-lines)
         :map selected-org-mode-map
         ("t" . org-table-convert-region))
  :diminish selected-minor-mode)
;; Change to selected? https://github.com/Kungsgeten/selected.el
;; https://www.reddit.com/r/emacs/comments/63mx6f/how_do_you_use_the_selectel_package_share_some/
;; (require 'region-bindings-mode)
;; (region-bindings-mode-enable)

;; (define-key region-bindings-mode-map "\M-a" 'mc/mark-all-dwim)
;; (define-key region-bindings-mode-map "\M-A" 'mc/mark-all-like-this)
;; (define-key region-bindings-mode-map "\M-p" 'mc/mark-previous-like-this)
;; (define-key region-bindings-mode-map "\M-P" 'mc/unmark-previous-like-this)
;; (define-key region-bindings-mode-map "\M-n" 'mc/mark-next-like-this)
;; (define-key region-bindings-mode-map "\M-N" 'mc/unmark-next-like-this)
;; (define-key region-bindings-mode-map "\M-m" 'mc/mark-more-like-this-extended)

(use-package multiple-cursors
  :bind (("C-c m" . mc/mark-all-dwim)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this))
  :init (setq mc/list-file "~/.emacs.d/personal/.mc-lists.el")
  :config
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

  (with-eval-after-load 'multiple-cursors-core
    (define-key mc/keymap (kbd "M-T") 'mc/reverse-regions)
    (define-key mc/keymap (kbd "C-,") 'mc/unmark-next-like-this)
    (define-key mc/keymap (kbd "C-.") 'mc/skip-to-next-like-this)))


;; key bindings - misc

(use-package dakra :load-path "repos/dakra.el" :ensure nil
  :bind (("C-x k" . dakra-kill-this-buffer) ; Don't prompt which buffer to kill. Always use current-buffer
         ("M-u" . dakra-upcase-dwim)
         ("M-l" . dakra-downcase-dwim)
         ("M-c" . dakra-capitalize-dwim)
         ("C-x o" . dakra-next-window-or-frame)
         ("C-x O" . dakra-previous-window-or-frame)))

(use-package god-mode
  ;; Make god-mode a little bit more vi-like
  :bind (("<escape>" . god-local-mode)
         :map god-local-mode-map ("i" . god-local-mode))
  :config
  ;; change curser to bar when in god-mode
  (defun god-update-cursor ()
    "Toggle curser style to bar when in god-mode"
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'bar
                        'box)))
  (add-hook 'god-mode-enabled-hook 'god-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-update-cursor))


;; scroll 4 lines up/down w/o moving pointer
;;(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   1)) )
;;(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 1)) )

;; remove flyspess 'C-;' keybinding so we can use it for avy jump
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") nil))

(use-package avy
  :bind ("C-;" . avy-goto-char-timer)
  :config
  (setq avy-background t)
  (setq avy-style 'at-full)
  (setq avy-timeout-seconds 0.3))

(use-package ace-link
  :commands (ace-link-eww ace-link-org ace-link-woman)
  :bind (:map Info-mode-map ("o" . ace-link-info)
              :map help-mode-map ("o" . ace-link-help)
              :map compilation-mode-map ("o" . ace-link-compilation)
              :map org-mode-map ("M-o" . ace-link-org))
  :init
  (eval-after-load "woman"
    `(define-key woman-mode-map ,"o" 'ace-link-woman))
  (eval-after-load "eww"
    `(progn
       (define-key eww-link-keymap ,"o" 'ace-link-eww)
       (define-key eww-mode-map ,"o" 'ace-link-eww))))

;; Spellcheck setup

;; Show helm-list of correct spelling suggesions
(use-package flyspell-correct-helm
  :bind (:map flyspell-mode-map
              ("C-." . flyspell-correct-previous-word-generic)))

;; Automatically guess languages and switch ispell

(use-package guess-language
  :commands guess-language-mode
  :init
  ;; Only guess language for emails
  (add-hook 'mu4e-compose-mode-hook 'guess-language-mode)
  ;;(add-hook 'text-mode-hook (lambda () (guess-language-mode 1)))
  ;;(add-hook 'org-mode-hook (lambda () (guess-language-mode 1)))
  :config
  (setq guess-language-langcodes '((en . ("en_GB" "English"))
                                   (de . ("de_DE" "German"))))
  (setq guess-language-languages '(en de))
  (setq guess-language-min-paragraph-length 35)
  )


(use-package web-mode
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.tpl\\'" "\\.blade\\.php\\'" "\\.jsp\\'" "\\.as[cp]x\\'"
         "\\.erb\\'" "\\.html.?\\'" "/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" "\\.jinja2?\\'")
  :config
  ;;(setq web-mode-engines-alist '(("django"  . "/templates/.*\\.html\\'")))
  (setq web-mode-engines-alist '(("django" . "\\.jinja2?\\'")))

  ;; make web-mode play nice with smartparens
  (setq web-mode-enable-auto-pairing nil)

  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
                   :unless '(sp-in-string-p)
                   :post-handlers '(((lambda (&rest _ignored)
                                       (just-one-space)
                                       (save-excursion (insert " ")))
                                     "SPC" "=" "#")))
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>"))

  ;; Flyspell setup
  ;;http://blog.binchen.org/posts/effective-spell-check-in-emacs.html

  ;; {{ flyspell setup for web-mode
  (defun web-mode-flyspell-verify ()
    (let* ((f (get-text-property (- (point) 1) 'face))
           rlt)
      (cond
       ;; Check the words with these font faces, possibly.
       ;; this *blacklist* will be tweaked in next condition
       ((not (memq f '(web-mode-html-attr-value-face
                       web-mode-html-tag-face
                       web-mode-html-attr-name-face
                       web-mode-constant-face
                       web-mode-doctype-face
                       web-mode-keyword-face
                       web-mode-comment-face ;; focus on get html label right
                       web-mode-function-name-face
                       web-mode-variable-name-face
                       web-mode-css-property-name-face
                       web-mode-css-selector-face
                       web-mode-css-color-face
                       web-mode-type-face
                       web-mode-block-control-face)))
        (setq rlt t))
       ;; check attribute value under certain conditions
       ((memq f '(web-mode-html-attr-value-face))
        (save-excursion
          (search-backward-regexp "=['\"]" (line-beginning-position) t)
          (backward-char)
          (setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$"
                                  (thing-at-point 'symbol)))))
       ;; finalize the blacklist
       (t
        (setq rlt nil)))
      rlt))
  (put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)

  ;; Don't display doublon (double word) as error
  (defvar flyspell-check-doublon t
    "Check doublon (double word) when calling `flyspell-highlight-incorrect-region'.")
  (make-variable-buffer-local 'flyspell-check-doublon)

  (defadvice flyspell-highlight-incorrect-region (around flyspell-highlight-incorrect-region-hack activate)
    (if (or flyspell-check-doublon (not (eq 'doublon (ad-get-arg 2))))
        ad-do-it))

  (defun web-mode-hook-setup ()
    (flyspell-mode 1)
    (setq flyspell-check-doublon nil))

  (add-hook 'web-mode-hook 'web-mode-hook-setup)
  ;; } flyspell setup

  (setq web-mode-markup-indent-offset 2)
  ;; auto close tags in web-mode
  (setq web-mode-enable-auto-closing t))


;; Spell check camel case strings
(setq ispell-program-name "aspell"
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      ispell-extra-args '("--sug-mode=ultra"
                          "--run-together"
                          "--run-together-limit=5"
                          "--run-together-min=2"))

;; Javascript and ReactJS setup
(defun js-flyspell-verify ()
  (let* ((f (get-text-property (- (point) 1) 'face)))
    ;; *whitelist*
    ;; only words with following font face will be checked
    (memq f '(js2-function-call
              js2-function-param
              js2-object-property
              font-lock-variable-name-face
              font-lock-string-face
              font-lock-function-name-face))))
(put 'js2-mode 'flyspell-mode-predicate 'js-flyspell-verify)
(put 'rjsx-mode 'flyspell-mode-predicate 'js-flyspell-verify)
;; }}

;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;; Don't use Camel Case when correcting a word
(defun flyspell-detect-ispell-args (&optional run-together)
  "if RUN-TOGETHER is true, spell check the CamelCase words."
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; Force the English dictionary for aspell
      ;; Support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra"))
      (if run-together
          (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
     ((string-match "hunspell$" ispell-program-name)
      ;; Force the English dictionary for hunspell
      (setq args "")))
    args))

(setq-default ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))
;; flyspell-correct-helm uses this function
(defadvice flyspell-correct-word-generic (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defun text-mode-hook-setup ()
  ;; Turn off RUN-TOGETHER option when spell check text-mode
  (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
(add-hook 'text-mode-hook 'text-mode-hook-setup)

;; end spell checking

(use-package iedit
  :init (setq iedit-toggle-key-default nil)
  :bind ("C-c ;" . iedit-mode))


;; FIXME: Don't always load yasnippet
(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ;; Complete yasnippets with company. No need for extra bindings
              ;;("TAB"     . nil)  ; Remove Yasnippet's default tab key binding
              ;;([tab]     . nil)
              ("<backtab>" . yas-expand)  ; Set Yasnippet's key binding to shift+tab
              ("\C-c TAB" . yas-expand)  ; Alternatively use Control-c + tab
              )
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/personal/snippets")
  (yas-global-mode 1))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package toml-mode
  :mode ("\\.toml\\'" "Cargo.lock\\'"))

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :config
  (add-hook 'yaml-mode-hook 'whitespace-mode)
  (add-hook 'yaml-mode-hook 'subword-mode)
  (add-hook 'yaml-mode-hook
            (lambda () (add-hook 'before-save-hook 'whitespace-cleanup nil t))))

(use-package shrink-whitespace
  :bind ("M-SPC" . shrink-whitespace))

(use-package editorconfig
  :config (editorconfig-mode 1)
  :diminish editorconfig-mode)

(use-package systemd
  :mode ("\\.service\\'" "\\.timer\\'"))

;; Turn off auto rever messages
(setq auto-revert-verbose nil)


;;; don't show some modes that are always on in the mode line
(diminish 'auto-revert-mode)
(diminish 'flyspell-mode)
(diminish 'guru-mode)
(diminish 'prelude-mode)
(diminish 'smartparens-mode)
(diminish 'whitespace-mode)


;; backup

(setq create-lockfiles nil)  ; disable lock file symlinks

(setq backup-directory-alist `((".*" . "~/.emacs.d/.backups")))

(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      )

(use-package keychain-environment
  :commands keychain-refresh-environment
  :init
  ;; Load ssh/gpg agent environment after 2 minutes. If the agent isn't started yet (not entered password),
  ;; we have to call (keychain-refresh-environment) interactively later
  (if (daemonp)
      (run-at-time "2 min" nil 'keychain-refresh-environment)))
;;; personal.el ends here
