;;; package --- personal

;;; Commentary:
;;; Emacs config

;;; Code:

(require 'prelude-packages nil 'noerror)

(prelude-require-packages
 '(
   moe-theme
   ;;color-theme-sanityinc-solarized
   smart-mode-line-powerline-theme

   eshell-git-prompt
   flyspell-correct-helm
   guess-language  ; switch ispell automatically between languages
   keychain-environment  ; reload keychain info for ssh/gpg agent
   dired+

   ;; typing helpers
   back-button  ; nicer mark ring navigation (C-x C-SPC or C-x C-Left/Right)
   goto-chg  ; goto last change
   helm-ext  ; helm "hacks" like better path expandsion
   multiple-cursors
   region-bindings-mode  ; clone cursor with n,p when region selected

   ;; coding major/minor modes
   easy-escape  ; Nicer elisp regex syntax highlighting
   graphviz-dot-mode
   litable  ; live preview for elisp
   realgud
   skewer-mode  ; js live reloading
   tide  ; typescript
   ))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setq use-package-always-ensure t)


(use-package lua-mode :defer t)
(use-package ng2-mode :defer t)
(use-package nginx-mode :defer t)
(use-package fish-mode :defer t)

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

;; display custom agenda when starting emacs
;;(add-hook 'emacs-startup-hook (lambda () (org-agenda nil " ")))

;; recenter window after imenu jump so cursor doesn't end up on the last line
(add-hook 'imenu-after-jump-hook 'recenter)  ; or 'reposition-window


;; eshell
;;(setq eshell-list-files-after-cd t)
;;(setq eshell-ls-initial-args "-alh")

;; We're in emacs, so 'cat' is nicer there than 'less'
(setenv "PAGER" "cat")

;; Show git info in prompt
(eshell-git-prompt-use-theme 'powerline)


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
(global-set-key (kbd "C-y") 'xah-paste-or-paste-previous)

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

;; dired list size in human-readable format and list directories first
(setq dired-listing-switches "-hal --group-directories-first")
(setq dired-dwim-target t)
(setq diredp-dwim-any-frame-flag t)
(diredp-toggle-find-file-reuse-dir 1)  ; reuse dired buffers

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

  (defhydra hydra-python (python-mode-map "C-c C-t")
    "Run Python Tests"
    ("f" python-test-function "Function")
    ("m" python-test-method "Method")
    ("c" python-test-class "Class")
    ("F" python-test-file "File")
    ("p" python-test-project "Project")
    ("q" nil "Cancel"))
  (define-key python-mode-map (kbd "C-c C-t") 'hydra-python/body)

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
  _l_atex   _E_xample   _p_erl          _i_ndex:
  _a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
  _s_rc     _n_ote      plant_u_ml      _H_TML:
  _h_tml    ^ ^         ^ ^             _A_SCII:
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
  ("i" (hot-expand "<i"))
  ("e" (hot-expand "<s" "emacs-lisp"))
  ("p" (hot-expand "<s" "perl"))
  ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
  ("P" (hot-expand "<s" "perl" ":results output :exports both :shebang \"#!/usr/bin/env perl\"\n"))
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

;; FIXME: New emacs no tramp-file-name-regexp ??
;; Skip version control for tramp files
;;(setq vc-ignore-dir-regexp
;;      (format "\\(%s\\)\\|\\(%s\\)"
;;              vc-ignore-dir-regexp
;;              tramp-file-name-regexp))

;; Turn of auto-save for tramp files
;;(add-to-list 'backup-directory-alist
;;             (cons tramp-file-name-regexp nil))

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



(use-package back-button
  :config (back-button-mode 1))



;; Helm config

;; keep follow-mode in between helm sessions once activated
(setq helm-follow-mode-persistent t)

(define-key prelude-mode-map (kbd "C-c i") 'helm-imenu-anywhere)
(define-key prelude-mode-map (kbd "C-c j") 'helm-imenu)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)  ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)  ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action)  ; list actions using C-z
;; use helm bookmarks
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)

;; use swiper with helm backend for search
(use-package swiper-helm
  :bind ("\C-s" . swiper-helm))


;; Smaller helm window
(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 30)
(helm-autoresize-mode 1)

;; Don't show details in helm-mini for tramp buffers
(setq helm-buffer-skip-remote-checking t)

;; Show bookmarks (and create bookmarks) in helm-mini
(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-bookmarks
                                  helm-source-bookmark-set
                                  helm-source-buffer-not-found))

;; Skip . and .. for non empty dirs
(helm-ext-ff-enable-skipping-dots t)

;; Enable zsh/fish shell like path expansion
(helm-ext-ff-enable-zsh-path-expansion t)
(helm-ext-ff-enable-auto-path-expansion t)

;; Don't use minibuffer if there's something there already
(helm-ext-minibuffer-enable-header-line-maybe t)

;; wolfram alpha queries (M-x wolfram-alpha)
(use-package wolfram
  :config
  (setq wolfram-alpha-app-id "KTKV36-2LRW2LELV8"))


;; Autofill (e.g. M-x autofill-paragraph or M-q) to 80 chars (default 70)
;; set with 'custom' since it's buffer-local variable
;; (setq fill-column 80)

;; Use 'C-c S' or 'M-s M-w' for 'eww-search-words' current region
(define-key prelude-mode-map (kbd "C-c S") nil)  ; remove default crux find-shell-init keybinding
(global-set-key (kbd "C-c S") 'eww-search-words)

(define-key prelude-mode-map (kbd "C-c u") 'browse-url-at-point)


(setq eww-search-prefix "https://google.com/search?q=")

;; Do action that normally works on a region to the whole line if no region active.
;; That way you can just C-w to copy the whole line for example.
(use-package whole-line-or-region
  :diminish whole-line-or-region-mode
  :config (whole-line-or-region-mode t))

;; cache projectile project files
;; projectile-find-files will be much faster for large projects.
;; C-u C-c p f to clear cache before search.
(setq projectile-enable-caching t)

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
              ("TAB" . company-complete-selection)
              ([tab] . company-complete-selection)
              ("C-j" . company-complete-selection))
  :config
  (setq company-idle-delay 0.2)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  ;;(setq company-dabbrev-downcase nil)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  ;; start autocompletion only after typing
  (setq company-begin-commands '(self-insert-command))
  (global-company-mode 1)

  (use-package company-emoji
    :disabled t
    :config (add-to-list 'company-backends 'company-emoji))

  (use-package company-quickhelp
    :config (company-quickhelp-mode 1))

  (use-package slime-company
    :config (slime-setup '(slime-fancy slime-company)))

  (use-package company-tern
    :config
    (setq company-tern-property-marker "")  ; don't show circles for properties
    (add-to-list 'company-backends 'company-tern))

  (use-package company-restclient
    :config (add-to-list 'company-backends 'company-restclient))

  ;; Add yasnippet support for all company backends
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package fabric
  :defer t)

(use-package fish-mode
  :mode "\\.fish\\'")

(use-package jira-markup-mode
  :mode ("\\.confluence\\'" "/itsalltext/.*jira.*\\.txt$"))

(use-package markdown-mode
  :mode "/itsalltext/.*\\(gitlab\\|github\\).*\\.txt$"
  :config
  ;; use pandoc with source code syntax highlighting to preview markdown (C-c C-c p)
  (setq markdown-command "pandoc -s --highlight-style pygments -f markdown_github -t html5"))

;; always loop GIF images
(setq image-animate-loop t)

;; send alerts by default to D-Bus
(setq alert-default-style 'notifications)

;; let emacs work nicely with i3; i3-emacs is not on melpa; manually installed
;; used together with i3 keyboard shortcut (S-e) to `emacsclient -cn -e '(switch-to-buffer nil)`
(use-package i3
  :ensure nil)
(use-package i3-integration
  :ensure nil
  :config
  (i3-one-window-per-frame-mode-on)
  (i3-advise-visible-frame-list-on)
  )


(defmacro dakra-define-up/downcase-dwim (case)
  (let ((func (intern (concat "dakra-" case "-dwim")))
        (doc (format "Like `%s-dwim' but %s from beginning when no region is active." case case))
        (case-region (intern (concat case "-region")))
        (case-word (intern (concat case "-word"))))
    `(defun ,func (arg)
       ,doc
       (interactive "*p")
       (save-excursion
         (if (use-region-p)
             (,case-region (region-beginning) (region-end))
           (beginning-of-thing 'symbol)
           (,case-word arg)))
       )))
(dakra-define-up/downcase-dwim "upcase")
(dakra-define-up/downcase-dwim "downcase")
(dakra-define-up/downcase-dwim "capitalize")
(global-set-key (kbd "M-u") 'dakra-upcase-dwim)
(global-set-key (kbd "M-l") 'dakra-downcase-dwim)
(global-set-key (kbd "M-c") 'dakra-capitalize-dwim)


(defun dakra-other-window-or-frame ()
  "Call `other-window' if more than one window is visible, `other-frame' otherwise."
  (interactive)
  (if (one-window-p)
      (other-frame 1)
    (other-window 1)))

(global-set-key (kbd "C-x o") 'dakra-other-window-or-frame)
;; since i3-mode always creates new frames instead of windows
;; rebind "C-x o" to switch frames if we use X11
;;(global-set-key (kbd "C-x o") (lambda ()
;;                                (interactive)
;;                                (if (display-graphic-p)
;;                                    (other-frame 1)
;;                                  (other-window 1))))
;;(global-set-key (kbd "C-x O") (lambda ()
;;                                (interactive)
;;                                (if (display-graphic-p)
;;                                    (other-frame -1)
;;                                  (other-window -1))))
;; C-x 2/3 should create a new frame in X
(global-set-key (kbd "C-x 2") (lambda ()
                                (interactive)
                                (if (display-graphic-p)
                                    (progn (i3-command 0 "split vertical")
                                           (new-frame))
                                  (split-window-horizontally))))
(global-set-key (kbd "C-x 3") (lambda ()
                                (interactive)
                                (if (display-graphic-p)
                                    (progn (i3-command 0 "split horizontal")
                                           (new-frame))
                                  (split-window-horizontally))))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(defun dakra-toggle-browser ()
  (interactive)
  (if (eq browse-url-browser-function 'eww-browse-url)
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "firefox")
    (setq browse-url-browser-function 'eww-browse-url)))


(setq sml/theme 'powerline)  ; smart-mode-line theme

(use-package docker
  :config (setq docker-keymap-prefix "C-c C-d"))
(use-package dockerfile-mode
  :mode "Dockerfile\\'")
(use-package docker-tramp)

(use-package smart-region
  ;; C-SPC is smart-region
  :bind (([remap set-mark-command] . smart-region)))


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
(add-to-list 'delete-frame-functions #'maybe-delete-frame-buffer)


;; use outline-cycle (from outline-magic) in outline-minor-mode
(use-package outline-magic
  :bind (:map outline-minor-mode-map ("<C-tab>" . outline-cycle)))


;; SQL
(require 'sql)
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
                          (sql-database "api"))
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
  (sql-connect connection))

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
            (setq sql-set-product 'mysql)))


;; Capitalize keywords in SQL mode
(use-package sqlup-mode
  :defer t
  :init
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
  :config
  ;; Don't capitalize `name` or 'type' keyword
  (add-to-list 'sqlup-blacklist "name")
  (add-to-list 'sqlup-blacklist "type"))


;; use tern for js autocompletion
(add-hook 'js-mode-hook (lambda () (tern-mode t)))

(setq httpd-port 8079)  ; set port for simple-httpd used by skewer
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(use-package emmet-mode
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


(use-package scss-mode
  :defer t
  :config
  ;; turn off annoying auto-compile on save
  (setq scss-compile-at-save nil)
  (prelude-css-mode-defaults))


;; python

;; package-list-packages like interface for python packages
(use-package pippel :defer t)

;; Syntax highlighting for requirements.txt files
(use-package pip-requirements)

(use-package sphinx-mode)

;; FIXME: change stuff from prelude-python.el to here
;;(use-package anaconda-mode  :mode ("\\.py\\'" "\\.xsh\\'"))

;; Xonsh scripts are python files
(add-to-list 'auto-mode-alist '("\\.xsh$" . python-mode))

;; Enable eldoc for python
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)


(use-package python-test :defer t :load-path "repos/python-test.el"
  :init (setq python-test-backend 'pytest))


;; Enable (restructured) syntax highlighting for python docstrings
(use-package python-docstring
  :init (add-hook 'python-mode-hook 'python-docstring-mode))

;; Automatically sort and format python imports
(use-package py-isort :defer t
  :config
  ;;(add-hook 'before-save-hook 'py-isort-before-save)
  (setq py-isort-options '("--line-width=100"
                           "--multi_line=3"
                           "--trailing-comma"
                           "--force-grid-wrap"
                           "--thirdparty=rethinkdb")))

;; accept 'UTF-8' (uppercase) as a valid encoding in the coding header
(define-coding-system-alias 'UTF-8 'utf-8)

(use-package restclient
  :mode "\\.rest\\'"
  :config (use-package restclient-helm))

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

;; Ignore import errors that don't have typings
(setq flycheck-python-mypy-silent-imports t)

;; use both pylint and flake8 in flycheck
;;(flycheck-add-next-checker 'python-flake8 'python-pylint 'python-mypy)
;;(flycheck-add-next-checker 'python-flake8 'python-mypy)
(setq flycheck-flake8-maximum-line-length 110)

;; ipython5 uses prompt_toolkit which doesn't play nice with emacs
;; when setting interpreter to 'ipython', you need additional '--simple-prompt' arg
(setq python-shell-interpreter "python")
;;(setq python-shell-interpreter-args "-i")
;; FIXME: run new python interpreter on projectile-switch-project?
;; and only run pshell when it's a pyramid project.
;;(setq python-shell-interpreter "python"
;;      python-shell-interpreter-args "--simple-prompt -i /home/daniel/.virtualenvs/atomx/lib/python3.5/site-packages/pyramid/scripts/pshell.py /home/daniel/atomx/api/development.ini")

(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) ;; if you want eshell support
  (setq venv-location "/home/daniel/.virtualenvs/")
  ;;(venv-workon '"atomx")  ; default venv after a starting emacs
  (setq projectile-switch-project-action '(lambda ()
                                            (venv-projectile-auto-workon)
                                            (helm-projectile))))


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
;;(require 'importmagic)
;;(add-hook 'python-mode-hook 'importmagic-mode)
;;(define-key importmagic-mode-map (kbd "C-c C-i") 'importmagic-fix-symbol-at-point)
;;(add-to-list 'helm-boring-buffer-regexp-list "\\*epc con")


;; disable auto escape quote feature of smartparens
(setq sp-escape-quotes-after-insert nil
      sp-escape-wrapped-region nil)

;; open current line/region/dired/commit in github
(use-package browse-at-remote
  :bind (:map prelude-mode-map ("C-c G" . browse-at-remote)))

;; FIXME: find another gh lib. only works for public repos and unmaintained
;; just type 'fixes #' and get github issue autocompletion
(use-package github-issues
  :disabled t
  :defer t
  :init (add-hook 'git-commit-mode-hook 'git-commit-insert-issue-mode))

;; Nicer diff (should be taken from global .config/git/config)
(setq vc-git-diff-switches '("--indent-heuristic"))

;; Always highlight word differences in diff
(setq magit-diff-refine-hunk 'all)

(use-package gist
  :defer t)

;; github pull request support for magit
(use-package magit-gh-pulls
  :defer t
  :init (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)


(use-package symbol-overlay
  :commands symbol-overlay-mode
  ;;:preface (defvar symbol-overlay-mode-map nil)
  :init
  (add-hook 'prog-mode-hook 'symbol-overlay-mode)
  (setq symbol-overlay-temp-in-scope t)
  :bind (("M-n" . symbol-overlay-switch-forward)
         ("M-p" . symbol-overlay-switch-backward)
         :map prelude-mode-map
         ("C-c s" . symbol-overlay-put)
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
  (setq symbol-overlay-map (make-sparse-keymap)  ;; Remove all default bindings
        symbol-overlay-temp-face '((:background "gray30"))))


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
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm))

;; change `find-file` so all files that belong to root are opened as root
;; too often unintentional changes. just use 'M-x crux-sudo-edit' when needed
;;(crux-reopen-as-root-mode)

;; ledger-mode for bookkeeping
(use-package ledger-mode
  :mode "\\.ledger\\'"
  :config
  ;; disable whitespace-mode in ledger
  (add-hook 'ledger-report-mode-hook (lambda () (whitespace-mode -1)))
  (setq ledger-post-amount-alignment-column 60))
;;(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
;;(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))


(use-package aggressive-indent
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'lisp-mode #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode)
  (add-hook 'js2-mode-hook #'aggressive-indent-mode))

(use-package easy-escape
  :defer t
  :diminish easy-escape-minor-mode
  :init
  ;; Nicer elisp regex syntax highlighting
  (add-hook 'lisp-mode-hook 'easy-escape-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'easy-escape-minor-mode))


;; octave
(setq octave-block-offset 4)
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
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
            (define-key octave-mode-map (kbd "C-x C-e") 'octave-send-region-or-line)
            (eldoc-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

(setq whitespace-line-column 110)  ; highlight lines with more than 110 characters

;;(show-paren-mode t)
;;(setq show-paren-style 'expression)


(use-package prettier-js
  :ensure nil
  ;;:init (add-hook 'js2-mode-hook (lambda () (add-hook 'before-save-hook 'prettier-before-save)))
  :config
  (setq prettier-args '(
                        "--trailing-comma" "all"
                        "--print-width" "100"
                        "--single-quote" "true"
                        "--bracket-spacing" "false"
                        ))
  (setq prettier-target-mode "js2-mode"))

(use-package json-mode
  :mode "\\.json\\'")

(use-package js2-mode
  :mode ("\\.js\\'" "\\.pac\\'" "node")
  :config
  ;; electric-layout-mode doesn't play nice with smartparens
  (setq-local electric-layout-rules '((?\; . after)))
  (setq mode-name "JS2")
  (js2-imenu-extras-mode +1)

  (setq js2-basic-offset 2)  ; set javascript indent to 2 spaces
  )


;; TypeScript
(setq typescript-indent-level 2)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (company-mode +1)
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))  ; add tide yasnippets as company backend
  )

;; aligns annotation to the right hand side
;;(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options

(add-hook 'typescript-mode-hook #'setup-tide-mode)


(setq undo-tree-visualizer-timestamps t)  ; show timestamps in undo-tree

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

;; "C-=" is not valid ascii sequence in terminals
;;(global-set-key (kbd "C-@") 'er/expand-region)

;; Change to selected? https://github.com/Kungsgeten/selected.el
;; https://www.reddit.com/r/emacs/comments/63mx6f/how_do_you_use_the_selectel_package_share_some/
(require 'region-bindings-mode)
(region-bindings-mode-enable)

(define-key region-bindings-mode-map "\M-a" 'mc/mark-all-dwim)
(define-key region-bindings-mode-map "\M-A" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "\M-p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "\M-P" 'mc/unmark-previous-like-this)
(define-key region-bindings-mode-map "\M-n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "\M-N" 'mc/unmark-next-like-this)
(define-key region-bindings-mode-map "\M-m" 'mc/mark-more-like-this-extended)

(global-set-key (kbd "C-c m") 'mc/mark-all-dwim)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(with-eval-after-load 'multiple-cursors-core
  (define-key mc/keymap (kbd "M-T") 'mc/reverse-regions)
  (define-key mc/keymap (kbd "C-,") 'mc/unmark-next-like-this)
  (define-key mc/keymap (kbd "C-.") 'mc/skip-to-next-like-this))

;; key bindings - misc

;; Normally "C-x k" prompts you which buffer to kill. Remap to always kill the current buffer
(defun dakra-kill-this-buffer ()
  "Like (kill-this-buffer) but independent of the menu bar."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'dakra-kill-this-buffer)

(require 'god-mode)
;; Make god-mode a little bit more vi-like
(global-set-key (kbd "<escape>") 'god-local-mode)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)

;; change curser to bar when in god-mode
(defun god-update-cursor ()
  "Toggle curser style to bar when in god-mode"
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'bar
                      'box)))
(add-hook 'god-mode-enabled-hook 'god-update-cursor)
(add-hook 'god-mode-disabled-hook 'god-update-cursor)

;; goto last change
(global-set-key (kbd "C-c \\") 'goto-last-change)
(global-set-key (kbd "C-c |") 'goto-last-change-reverse)

;; scroll 4 lines up/down w/o moving pointer
;;(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   1)) )
;;(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 1)) )

;; remove flyspess 'C-;' keybinding so we can use it for avy jump
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") nil))
(setq avy-timeout-seconds 0.3)  ; only wait 0.3 seconds for char timeout (default 0.5)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)


;; Spellcheck setup

;; Show helm-list of correct spelling suggesions
(require 'flyspell-correct-helm)
(define-key flyspell-mode-map (kbd "C-.") 'flyspell-correct-previous-word-generic)

;; Automatically guess languages
(require 'guess-language)
(setq guess-language-langcodes '((en . ("en_GB" "English"))
                                 (de . ("de_DE" "German"))))
(setq guess-language-languages '(en de))
(setq guess-language-min-paragraph-length 35)
;; Only guess language for emails
(add-hook 'mu4e-compose-mode-hook (lambda () (guess-language-mode 1)))
;;(add-hook 'text-mode-hook (lambda () (guess-language-mode 1)))
;;(add-hook 'org-mode-hook (lambda () (guess-language-mode 1)))
;;(add-hook 'mu4e-compose-mode-hook (lambda () (guess-language-mode 1)))



(use-package web-mode
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.tpl\\'" "\\.blade\\.php\\'" "\\.jsp\\'" "\\.as[cp]x\\'"
         "\\.erb\\'" "\\.html?\\'" "/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'")
  :config
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

  ;;(setq web-mode-engines-alist '(("django"  . "/templates/.*\\.html\\'")))
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


(use-package yaml-mode
  :mode "\\.yaml\\'"
  :config
  (add-hook 'yaml-mode-hook 'whitespace-mode)
  (add-hook 'yaml-mode-hook 'subword-mode)
  (add-hook 'yaml-mode-hook
            (lambda () (add-hook 'before-save-hook 'whitespace-cleanup nil t))))

(use-package editorconfig
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

(use-package systemd)

;; Turn off auto rever messages
(setq auto-revert-verbose nil)


;;; don't show some modes that are always on in the mode line
(diminish 'auto-revert-mode)
(diminish 'back-button-mode)
(diminish 'beacon-mode)
(diminish 'flyspell-mode)
(diminish 'guru-mode)
(diminish 'helm-mode)
(diminish 'prelude-mode)
(diminish 'smartparens-mode)
(diminish 'which-key-mode)
(diminish 'whitespace-mode)
(diminish 'anaconda-mode)


;; backup

(setq create-lockfiles nil)  ; disable lock file symlinks

(setq backup-directory-alist `((".*" . "~/.emacs.d/.backups")))

(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      )

;; Load ssh/gpg agent environment after 2 minutes. If the agent isn't started yet (not entered password),
;; we have to call (keychain-refresh-environment) interactively later
(run-at-time "2 min" nil 'keychain-refresh-environment)
;;; personal.el ends here
