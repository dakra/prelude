;;; package --- personal

;;; Commentary:
;;; Emacs config

;;; Code:

(require 'prelude-packages nil 'noerror)

(prelude-require-packages
 '(
   color-theme-sanityinc-solarized
   smart-mode-line-powerline-theme

   ;;sr-speedbar  ; open speedbar inside the frame
   ;;projectile-speedbar
   wolfram

   ;; typing helpers
   emmet-mode
   goto-chg  ; goto last change
   helm-emmet
   highlight-indent-guides
   highlight-symbol  ; highlight all symbols like the one under the cursor
   iedit
   multiple-cursors
   origami  ; code folding
   region-bindings-mode  ; clone cursor with n,p when region selected
   smart-region
   swiper-helm  ; C-s search with helm
   whole-line-or-region  ; operate on current line if region is undefined
   yasnippet

   ;; git / github
   browse-at-remote  ; "C-G" opens current buffer on github
   gist
   ;;github-issues
   magit-gh-pulls

   ;; coding major/minor modes
   lua-mode
   dockerfile-mode
   nginx-mode
   graphviz-dot-mode
   systemd
   skewer-mode  ; js live reloading
   tide  ; typescript
   ng2-mode
   sqlup-mode  ; make sql keywords automatically upercase
   sphinx-mode
   realgud
   fabric
   virtualenvwrapper
   slime-company
   company-tern
   company-emoji
   company-restclient
   company-quickhelp
   restclient
   restclient-helm
   outline-magic  ; better outline-mode
   ))


;; temporary fixes:
;; emacs 25 -> 26 they renamed some functions that make 'which-key' fail
(defalias 'display-buffer-in-major-side-window 'window--make-major-side-window)

;; save and restore buffer and cursor positions (but don't restore window layout)
;;(desktop-save-mode 1)
;;(setq desktop-restore-frames nil)

;; disable arrow keys to be forced to learn emacs
;;(setq guru-warn-only nil)

;; display custom agenda when starting emacs
(add-hook 'emacs-startup-hook (lambda () (org-agenda nil " ")))


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

;; C-SPC is smart-region
(smart-region-on)

;; dired list size in human-readable format and list directories first
(setq dired-listing-switches "-hal --group-directories-first")

;; wolfram alpha queries (M-x wolfram-alpha)
(require 'wolfram)
(setq wolfram-alpha-app-id "KTKV36-2LRW2LELV8")

;; Autofill (e.g. M-x autofill-paragraph or M-q) to 80 chars (default 70)
(setq fill-column 80)

;; Use 'C-c S' or 'M-s M-w' for 'eww-search-words' current region
(define-key prelude-mode-map (kbd "C-c S") nil)  ; remove default crux find-shell-init keybinding
(global-set-key (kbd "C-c S") 'eww-search-words)

(setq eww-search-prefix "https://google.com/search?q=")

;; Do action that normally works on a region to the whole line if no region active.
;; That way you can just C-w to copy the whole line for example.
(whole-line-or-region-mode t)

;; cache projectile project files
;; projectile-find-files will be much faster for large projects.
;; C-u C-c p f to clear cache before search.
(setq projectile-enable-caching t)

;; highlight indentations in python
(setq highlight-indent-guides-method 'character)
(add-hook 'python-mode-hook 'highlight-indent-guides-mode)
;;(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; activate character folding in searches i.e. searching for 'a' matches 'ä' as well
(setq search-default-mode 'char-fold-to-regexp)

;; emoji font
;; package ttf-symbola has to be installed
(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend))

;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)

;; company-mode config

(require 'slime-company)
(slime-setup '(slime-fancy slime-company))

(require 'company-emoji)
(add-to-list 'company-backends 'company-emoji)

;; FIXME: can't set it to lower value because of anaconda bug:
;; https://github.com/proofit404/anaconda-mode/issues/183
(setq company-idle-delay 0.5)  ; show auto completion almost instantly
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;; don't auto complete with <return> but with C-j
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "C-j") #'company-complete-selection))

;; show help popup when completing with company
(company-quickhelp-mode 1)

;; keep follow-mode in between helm sessions once activated
(setq helm-follow-mode-persistent t)


;; always loop GIF images
(setq image-animate-loop t)

;; send alerts by default to D-Bus
(setq alert-default-style 'notifications)

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

(defun dakra-toggle-browser ()
  (interactive)
  (if (eq browse-url-browser-function 'eww-browse-url)
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "google-chrome")
    (setq browse-url-browser-function 'eww-browse-url)))


(setq sml/theme 'powerline)  ; smart-mode-line theme

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; XXX: never use speedbar. disable for now
;;(speedbar-add-supported-extension ".hs")
;;(speedbar-add-supported-extension ".scala")
;;(speedbar-add-supported-extension ".md")
;;(require 'projectile-speedbar)
;;(global-set-key [f5] 'projectile-speedbar-toggle)



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

(require 'origami)
(define-key origami-mode-map (kbd "C-c C-o") 'origami-recursively-toggle-node)
(add-hook 'prog-mode-hook
          (lambda () (origami-mode)))
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
(eval-after-load 'outline
  '(progn
     (require 'outline-magic)
     (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))

;; SQL
(require 'sql)
(sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) \\[[_a-zA-Z]*\\]> ")

(setq sql-product 'mysql)
(setq sql-connection-alist
      '((atomx-local-api (sql-product 'mysql)
                         (sql-server "localhost")
                         (sql-user "daniel")
                         (sql-database "api"))
        (atomx-remote-api (sql-product 'mysql)
                          (sql-server "127.0.0.1")
                          (sql-port 3307)
                          (sql-user "root")
                          (sql-database "api"))))

(setq sql-mysql-login-params (append sql-mysql-login-params '(port)))

(defun dakra-sql-atomx-local-api ()
  (interactive)
  (dakra-sql-connect 'mysql 'atomx-local-api))

(defun dakra-sql-atomx-remote-api ()
  (interactive)
  (dakra-sql-connect 'mysql 'atomx-remote-api))

(defun dakra-sql-connect (product connection)
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
(require 'sqlup-mode)
(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
;; Don't capitalize `name` keyword
(add-to-list 'sqlup-blacklist "name")

;; use tern for js autocompletion
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-to-list 'company-backends 'company-tern)
(setq company-tern-property-marker "")  ; don't show circles for properties

(setq httpd-port 8079)  ; set port for simple-httpd used by skewer
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(define-key emmet-mode-keymap (kbd "<backtab>") 'emmet-expand-line)
(define-key emmet-mode-keymap (kbd "\C-c TAB") 'emmet-expand-line)

(define-key emmet-mode-keymap (kbd "C-M-p") 'emmet-prev-edit-point)
(define-key emmet-mode-keymap (kbd "C-M-n") 'emmet-next-edit-point)
(define-key emmet-mode-keymap (kbd "TAB") 'emmet-next-edit-point)

(setq emmet-move-cursor-between-quotes t)
(setq emmet-move-cursor-after-expanding t)


;; python

;; accept 'UTF-8' (uppercase) as a valid encoding in the coding header
(define-coding-system-alias 'UTF-8 'utf-8)


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

;; use both pylint and flake8 in flycheck
(flycheck-add-next-checker 'python-flake8 'python-pylint)
(setq flycheck-flake8-maximum-line-length 120)

;; XXX: Emacs25 python hangs when this is set to `true`
(setq python-shell-completion-native-enable nil)

;; ipython5 uses prompt_toolkit which doesn't play nice with emacs
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")
;; FIXME: run new python interpreter on projectile-switch-project?
;; and only run pshell when it's a pyramid project.
;;(setq python-shell-interpreter "python"
;;      python-shell-interpreter-args "--simple-prompt -i /home/daniel/.virtualenvs/atomx/lib/python3.5/site-packages/pyramid/scripts/pshell.py /home/daniel/atomx/api/development.ini")

;; XXX: run python once automatically?
;; run-python once for eldoc
;; (defun run-python-once ()
;;   (remove-hook 'anaconda-mode-hook 'run-python-once)
;;   (run-python))
;; (add-hook 'anaconda-mode-hook 'run-python-once)

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "/home/daniel/.virtualenvs/")
(venv-workon '"atomx")  ; default venv after a starting emacs
(setq projectile-switch-project-action '(lambda ()
                                          (venv-projectile-auto-workon)
                                          (projectile-find-file)))

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
;;(add-hook 'prog-mode-hook #'highlight-symbol-mode)
(define-key prelude-mode-map (kbd "C-c s") nil)  ; remove default crux swap windows keybinding
(global-set-key (kbd "C-c s") 'highlight-symbol)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; change `find-file` so all files that belong to root are opened as root
;; too often unintentional changes. just use 'M-x crux-sudo-edit' when needed
;;(crux-reopen-as-root-mode)

;; ledger-mode for bookkeeping
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))


;; aggressive indent everywhere
;;(global-aggressive-indent-mode 1)
;; disable for modes where proper indentation can't be reliably determined
;;(add-to-list 'aggressive-indent-excluded-modes 'python-mode)
;;(add-to-list 'aggressive-indent-excluded-modes 'haml-mode)
;; or opt-in for some only with:
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)
(add-hook 'js2-mode-hook #'aggressive-indent-mode)

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


(setq whitespace-line-column 120)  ; highlight lines with more than 120 characters

(setq js2-basic-offset 2)  ; set javascript indent to 2 spaces
(setq web-mode-markup-indent-offset 2)
;; auto close tags in web-mode
(setq web-mode-enable-auto-closing t)
;;(setq web-mode-enable-auto-pairing t)  ; doesn't play nice with smartparens

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


;; "C-=" is not valid ascii sequence in terminals
;;(global-set-key (kbd "C-@") 'er/expand-region)

;; multi cursor
(setq mc/list-file "~/.emacs.d/personal/.mc-lists.el")

(require 'region-bindings-mode)
(region-bindings-mode-enable)

(define-key region-bindings-mode-map "\M-a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "\M-p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "\M-n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "\M-m" 'mc/mark-more-like-this-extended)

(global-set-key (kbd "C-c m") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)


;; key bindings - misc

;; Normally "C-x k" prompts you which buffer to kill. Remap to always kill the current buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(require 'god-mode)
;; Make god-mode a little bit more vi-like
(global-set-key (kbd "<escape>") 'god-local-mode)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)

;; goto last change
(global-set-key (kbd "C-c \\") 'goto-last-change)
(global-set-key (kbd "C-c |") 'goto-last-change-reverse)

;; scroll 4 lines up/down w/o moving pointer
(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   1)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 1)) )

;; remove flyspess 'C-;' keybinding so we can use it for avy jump
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-;") nil))
(setq avy-timeout-seconds 0.4)  ; only wait 0.4 seconds for char timeout (default 0.5)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)

(global-set-key "\C-s" 'swiper-helm)  ; use swiper with helm backend for search


(setq iedit-toggle-key-default nil)
(require 'iedit)
(global-set-key (kbd "C-c ;") 'iedit-mode)


(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/personal/snippets")
(yas-global-mode 1)

;; Remove Yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
;; Alternatively use Control-c + tab
(define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)

;; Add yasnippet support for all company backends
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


;;; Don't show some modes that are always on in the mode line
(diminish 'yas-minor-mode)
(diminish 'guru-mode)
(diminish 'company-mode)
(diminish 'helm-mode)
(diminish 'whole-line-or-region-mode)
(diminish 'prelude-mode)
(diminish 'which-key-mode)
(diminish 'beacon-mode)

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
