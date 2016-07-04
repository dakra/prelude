;;; package --- personal

;;; Commentary:
;;; Emacs config

;;; Code:

(require 'prelude-packages nil 'noerror)

(prelude-require-packages
 '(
   color-theme-sanityinc-solarized
   smart-mode-line-powerline-theme

   sr-speedbar  ; open speedbar inside the frame
   projectile-speedbar

   ;; typing helpers
   yasnippet
   emmet-mode
   helm-emmet
   whole-line-or-region  ; operate on current line if region is undefined
   highlight-symbol  ; highlight all symbols like the one under the cursor
   multiple-cursors
   region-bindings-mode  ; clone cursor with n,p when region selected
   swiper-helm  ; C-s search with helm

   ;; git / github
   browse-at-remote  ; "C-G" opens current buffer on github
   gist
   ;;github-issues
   magit-gh-pulls

   ;; coding major/minor modes
   lua-mode
   dockerfile-mode
   systemd
   company-tern
   skewer-mode  ; js live reloading
   tide  ; typescript
   sqlup-mode  ; make sql keywords automatically upercase
   realgud
   virtualenvwrapper
   restclient
   restclient-helm
   company-restclient
   ))


;; disable arrow keys to be forced to learn emacs
(setq guru-warn-only nil)

(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/personal/snippets")

(whole-line-or-region-mode t)

(setq company-idle-delay 0)  ; show auto completion instantly
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing


;; let emacs work nicely with i3; i3-emacs is not on melpa; manually installed
;; used together with i3 keyboard shortcut (S-e) to `emacsclient -cn -e '(switch-to-buffer nil)`
(load-file "~/.emacs.d/personal/i3-emacs/i3.el")
(load-file "~/.emacs.d/personal/i3-emacs/i3-integration.el")
(require 'i3)
(require 'i3-integration)
(i3-one-window-per-frame-mode-on)
(i3-advise-visible-frame-list-on)

;; since i3-mode always creates new frames instead of windows
;; rebind "C-x o" to switch frames if we use X11
(global-set-key (kbd "C-x o") (lambda ()
                                (interactive)
                                (if (display-graphic-p)
                                    (other-frame 1)
                                  (other-window 1))))
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (if (display-graphic-p)
                                    (other-frame -1)
                                  (other-window -1))))
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
      browse-url-generic-program "google-chrome")

(setq sml/theme 'powerline)  ; smart-mode-line theme

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(speedbar-add-supported-extension ".hs")
(speedbar-add-supported-extension ".scala")
(speedbar-add-supported-extension ".md")


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


;; yank text to X11 clipboard
(defun yank-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
      (progn
        (shell-command-on-region (region-beginning) (region-end) "xsel -i")
        (message "Yanked region to clipboard!")
        (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

(global-set-key (kbd "C-c y") 'yank-to-x-clipboard)


(setq ffap-machine-p-known 'reject)  ; don't "ping Germany" when typing test.de<TAB>

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


;; FIXME: make sql-mode recognize mariadb prompt
(defun sqli-add-hooks ()
  (sql-set-product-feature 'mysql :prompt-regexp "^\(?:mysql\\|mariadb\).*> "))
(add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)
;;(sql-set-product-feature 'mysql :prompt-regexp "^MariaDB \[[a-zA-Z]+]\]> ")

;; Capitalize keywords in SQL mode
(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)

;; use tern for js autocompletion
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-to-list 'company-backends 'company-tern)
(setq company-tern-property-marker "")  ; don't show circles for properties

(setq httpd-port 8079)  ; set port for simple-httpd used by skewer
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.


;; python

(setq python-shell-interpreter "ipython")

;; XXX: run python once automatically?
;; run-python once for eldoc
;; (defun run-python-once ()
;;   (remove-hook 'anaconda-mode-hook 'run-python-once)
;;   (run-python))
;; (add-hook 'anaconda-mode-hook 'run-python-once)

;;(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "/home/daniel/.virtualenvs/")

(defcustom python-autopep8-path (executable-find "autopep8")
  "autopep8 executable path."
  :group 'python
  :type 'string)

(defun python-autopep8 ()
  "Automatically formats Python code to conform to the PEP 8 style guide.
$ autopep8 --in-place --aggressive --aggressive <filename>"
  (interactive)
  (when (eq major-mode 'anaconda-mode)
    (shell-command
     (format "%s --in-place --aggressive %s" python-autopep8-path
             (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))


;; auto completion in restclient-mode
(add-to-list 'company-backends 'company-restclient)


;; open current line/region/dired/commit in github
(define-key prelude-mode-map (kbd "C-c G") nil)
(global-set-key (kbd "C-c G") 'browse-at-remote)

;; FIXME: find another gh lib. only works for public repos and unmaintained
;; just type 'fixes #' and get github issue autocompletion
;;(add-hook 'git-commit-mode-hook 'git-commit-insert-issue-mode)

;; github pull request support for magit
;;(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

;; auto highlight all occurences of symbol under cursor
(add-hook 'prog-mode-hook #'highlight-symbol-mode)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; change `find-file` so all files that belong to root are opened as root
(crux-reopen-as-root-mode)

;; ledger-mode for bookkeeping
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))


(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (show-paren-mode 1)
            (eldoc-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))


(setq whitespace-line-column 120)  ; highlight lines with more than 120 characters

(setq js2-basic-offset 2)  ; set javascript indent to 2 spaces

;; auto close tags in web-mode
(setq web-mode-enable-auto-closing t)
;;(setq web-mode-enable-auto-pairing t)  ; doesn't play nice with smartparens


;; TypeScript

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (company-mode +1))

;; aligns annotation to the right hand side
;;(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options

(add-hook 'typescript-mode-hook #'setup-tide-mode)


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


;; "C-=" is not valid ascii sequence in terminals
;;(global-set-key (kbd "C-@") 'er/expand-region)

;; multi cursor
(setq mc/list-file "~/.emacs.d/personal/.mc-lists.el")

(require 'region-bindings-mode)
(region-bindings-mode-enable)

(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)

(global-set-key (kbd "C-c C-d") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)


;; key bindings - misc

;; remove flyspess 'C-;' keybinding so we can use it for avy jump
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") nil))
(setq avy-timeout-seconds 0.4)  ; only wait 0.4 seconds for char timeout (default 0.5)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)

(global-set-key "\C-s" 'swiper-helm)  ; use swiper with helm backend for search

(require 'projectile-speedbar)
(global-set-key [f5] 'projectile-speedbar-toggle)

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

;;; personal.el ends here
