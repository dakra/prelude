;;; package --- dakra-org

;;; Commentary:
;;; org-mode config

;;; Code:

(require 'prelude-packages nil 'noerror)
(prelude-require-packages
 '(
   htmlize
   ob-ipython
   ob-restclient
   org-download
   org-pomodoro
   orgit
   ox-jira
   ))

;; Install newest org and org-plus-contrib packages
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(require 'org-habit)  ; track habits

(require 'org-man)  ; make org-links work with man pages
(setq org-man-command 'woman)  ; open org-link man pages with woman

(require 'ox-jira)  ; for jira export (then copy&paste to ticket)

(add-hook 'org-mode-hook 'org-indent-mode)

;;(setq org-list-indent-offset 1)

(setq org-directory "~/org/")

(setq org-agenda-files '("~/org"))

;; Show headings up to level 2 by default when opening an org files
(setq org-startup-folded 'content)
;; overwrite org function to only show 'org-content' of level 2
(defun org-set-startup-visibility ()
  "Set the visibility required by startup options and properties."
  (cond
   ((eq org-startup-folded t)
    (org-overview))
   ((eq org-startup-folded 'content)
    (org-content 2))
   ((or (eq org-startup-folded 'showeverything)
	(eq org-startup-folded nil))
    (outline-show-all)))
  (unless (eq org-startup-folded 'showeverything)
    (when org-hide-block-startup (org-hide-block-all))
    (org-set-visibility-according-to-property 'no-cleanup)
    (org-cycle-hide-archived-subtrees 'all)
    (org-cycle-hide-drawers 'all)
    (org-cycle-show-empty-lines t)))

;; org-mode keybindings
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Make links work like chasing definitions in source code.
             (org-defkey org-mode-map "\M-." 'org-open-at-point)
             (org-defkey org-mode-map "\M-," 'org-mark-ring-goto)

             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c[" 'undefined)
             (org-defkey org-mode-map "\C-c]" 'undefined))
          'append)

;; Global key bindings
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f6>") 'org-agenda)
(global-set-key (kbd "<f7>") 'org-clock-goto)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)

;; install the restapi branch from org-jira (not on melpa yet)
;;(quelpa '(org-jira :fetcher github :repo "baohaojun/org-jira" :branch "restapi") :upgrade nil)
;;(require 'org-jira)
(setq jiralib-url "https://jira.paesslergmbh.de")

;; we never want to upgrade org-jira (to the soap version on master)
;; This snippet prevents package-menu to update it
(defvar package-menu-exclude-packages '("org-jira"))

(defun package-menu--remove-excluded-packages (orig)
  (let ((included (-filter
                   (lambda (entry)
                     (let ((name (symbol-name (package-desc-name (car entry)))))
                       (not (member name package-menu-exclude-packages))))
                   tabulated-list-entries)))
    (setq-local tabulated-list-entries included)
    (funcall orig)))

(advice-add 'package-menu--find-upgrades :around #'package-menu--remove-excluded-packages)


;; Enter key follows links (= C-c C-o)
(setq org-return-follows-link t)

;; Don't remove links after inserting
(setq org-keep-stored-link-after-insertion t)


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-use-fast-todo-selection t)

;; This allows changing todo states with S-left and S-right skipping all of the normal processing
;; when entering or leaving a todo state.
;; This cycles through the todo states but skips setting timestamps and entering notes which
;; is very convenient when all you want to do is fix up the status of an entry.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-default-notes-file (concat org-directory "refile.org"))

(setq org-clock-idle-time 15)  ; idle after 15 minutes

(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(require 'org-protocol)
;; org-capture chrome plugin: https://chrome.google.com/webstore/detail/org-capture/kkkjlfejijcjgjllecmnejhogpbcigdc?hl=en

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      `(("t" "todo" entry (file ,(concat org-directory "refile.org"))
         "* TODO %?\n%U\n" :clock-in t :clock-resume t)
        ("T" "todo with link" entry (file ,(concat org-directory "refile.org"))
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("e" "email" entry (file ,(concat org-directory "refile.org"))
         "* TODO %? Email: %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish nil)
        ("r" "respond" entry (file ,(concat org-directory "refile.org"))
         "* TODO Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("n" "note" entry (file ,(concat org-directory "refile.org"))
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("w" "org-protocol" entry (file ,(concat org-directory "refile.org"))
         "* TODO Review %c\n%U\n" :immediate-finish t)
        ("m" "Meeting" entry (file ,(concat org-directory "refile.org"))
         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
        ("P" "Phone call" entry (file ,(concat org-directory "refile.org"))
         "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
        ("p" "Protocol" entry (file ,(concat org-directory "refile.org"))
         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
        ("L" "Protocol Link" entry (file ,(concat org-directory "refile.org"))
         "* %?\nCaptured On: %U\n[[%:link][%:description]]\n")
        ("h" "Habit" entry (file ,(concat org-directory "refile.org"))
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))


;; undone TODO entries will block switching the parent to DONE
(setq org-enforce-todo-dependencies t)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks nil)

;; Only show the current clocked time in mode line (not all)
(setq org-clock-mode-line-total 'current)

;; Clocktable (C-c C-x C-r) defaults
(setq org-clock-clocktable-default-properties '(:block thismonth :scope file-with-archives))

;; Clocktable (reporting: r) in the agenda
(setq org-clocktable-defaults
      '(:maxlevel 3 :lang "en" :scope file-with-archives
                  :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 nil
                  :tags nil :emphasize nil :link t :narrow 70! :indent t :formula nil :timestamp nil
                  :level nil :tcolumns nil :formatter nil))

;; Never show 'days' in clocksum (e.g. in report clocktable)
;; format string used when creating CLOCKSUM lines and when generating a
;; time duration (avoid showing days)
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact nil :narrow 80)))

;; Set default column view headings: Task Effort Clock_Summary
;;(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

;; Set default column view headings: Task Total-Time Time-Stamp
(setq org-columns-default-format "%75ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

;; global Effort estimate values
;; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state clock)))

;; Tags with fast selection keys
(setq org-tag-alist (quote (("PERSONAL" . ?p)
                            ("WORK" . ?w)
                            ("ATOMX" . ?a)
                            ("E5" . ?e)
                            ("HOGASO" . ?h)
                            ("ORG" . ?o)
                            ("NOTE" . ?n)
                            ("crypt" . ?c)
                            ("FLAGGED" . ??))))

;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)


(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")


;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; C-RET, C-S-RET insert new heading after current task content
(setq org-insert-heading-respect-content nil)

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)


;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist-file "~/.emacs.d/personal/org-clock-save.el")
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; Use sticky agenda's so they persist
;;(setq org-agenda-sticky t)

;; Show a little bit more when using sparse-trees
(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))

(require 'org-crypt)
;; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
(setq org-crypt-key "C1C8D63F884EF9C9")
;; don't ask to disable auto-save
(setq org-crypt-disable-auto-save nil)


;; don't show * / = etc
(setq org-hide-emphasis-markers t)

;; leave highlights in sparse tree after edit. C-c C-c removes highlights
(setq org-remove-highlights-with-change nil)

;; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

;; disable whitespace-mode in org-mode
(add-hook 'org-mode-hook (lambda () (whitespace-mode -1)))

;; use utf-8 characters instead of `*` as bullet points
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Automatic line-wrapping in org-mode
(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))


(require 'org-pomodoro)

;; called with py3status in ~/.config/i3status/config with emacsclient --eval
(defun dakra/org-pomodoro-i3-bar-time ()
  "Display remaining pomodoro time in i3 status bar."
  (if (org-pomodoro-active-p)
      (format "Pomodoro: %d minutes - %s" (/ org-pomodoro-countdown 60) org-clock-heading)
    (if (org-clock-is-active)
        (org-no-properties (org-clock-get-clock-string))
      "No active pomodoro or tasks")))

;;(setq org-pomodoro-tick-hook 'dakra/org-pomodoro-i3-bar-time)
;;(setq org-pomodoro-finished-hook 'dakra/org-pomodoro-i3-bar-time)
;;(setq org-pomodoro-killed-hook 'dakra/org-pomodoro-i3-bar-time)


;; Automatically copy orgit link to last commit after commit
(add-hook 'git-commit-setup-hook
          (lambda ()
            (add-hook 'with-editor-post-finish-hook
                      (lambda ()
                        (let* ((repo (abbreviate-file-name default-directory))
                               (rev (magit-git-string "rev-parse" "HEAD"))
                               (link (format "orgit-rev:%s::%s" repo rev))
                               (desc (format "%s (magit-rev %s)" repo rev)))
                          (push (list link desc) org-stored-links)))
                      t t)))


;;; Create org TODO from github issue
;; FIXME:
;; get gh user/project from org filename/header? or properties?
;; replace ^M chars with \n
;; always insert TODO even when on NEXT header
;; ...
(require 'gh)
(defun dakra/org-insert-gh-issue (issue-number)
  (interactive "NGithub issue number:")
  (let (api raw-issue issue issue-title issue-body start-point)
    (setf api (gh-issues-api "api" :sync nil :cache nil :num-retries 1))
    (setf raw-issue (gh-issues-issue-get api "atomx" "api" issue-number))
    (setf issue (oref raw-issue :data))
    (setf issue-title (oref issue :title))
    (setf issue-body (oref issue :body))
    (org-insert-todo-heading-respect-content)
    (org-insert-link nil
                     (format "https://github.com/atomx/api/issues/%s" issue-number)
                     (format "#%s: %s" issue-number issue-title))
    (when bh/insert-inactive-timestamp
      (next-line)
      (move-end-of-line 1))
    (insert "\n")
    (setq start-point (point))
    (insert (format "%s" issue-body))
    (shell-command-on-region start-point (point) "pandoc -f markdown_github -t org" :replace t)
    (org-indent-refresh-maybe (point) (mark) nil)))

;;; automatically create github issues from org-mode
;; You have to set 'GH-PROJECT' property
(defun gh-issue-new-browse (project title body)
  (browse-url (concat "https://github.com/"
                      project
                      "/issues/new?title="
                      (url-hexify-string title)
                      "&body="
                      (url-hexify-string body))))

(defun gh-issue-create ()
  (interactive)
  (gh-issue-new-browse (org-entry-get (point) "GH-PROJECT" t)
                       (org-get-heading)
                       (org-export-as 'gfm t)))


;; code-verbatim with backticks
;; Set in preload.el
;; see: https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg00715.html


;;; org babel config

(add-to-list 'org-structure-template-alist '("sp" "#+BEGIN_SRC python\n?\n#+END_SRC"))
(add-to-list 'org-structure-template-alist '("si" "#+BEGIN_SRC ipython\n?\n#+END_SRC"))

;; Open babel src in other frame
;;(setq org-src-window-setup 'other-frame)

(setq org-confirm-babel-evaluate nil)  ; don't prompt me to confirm everytime I want to evaluate a block

;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(setq org-src-fontify-natively t)  ; syntax highlighting for source code blocks


;; Show multiple inline figures and results in one cell for ob-ipython.
;; http://kitchingroup.cheme.cmu.edu/blog/2017/01/29/ob-ipython-and-inline-figures-in-org-mode/
;; results must be in a drawer. So set a header like:
;; #+BEGIN_SRC ipython :session :results output drawer
(defun ob-ipython-inline-image (b64-string)
  "Write the b64-string to a temporary file.
Returns an org-link to the file."
  (let* ((tfile (make-temp-file "ob-ipython-" nil ".png"))
         (link (format "[[file:%s]]" tfile)))
    (ob-ipython--write-base64-string tfile b64-string)
    link))

(defun org-babel-execute:ipython (body params)
  "Execute a block of IPython code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((file (cdr (assoc :file params)))
         (session (cdr (assoc :session params)))
         (result-type (cdr (assoc :result-type params))))
    (org-babel-ipython-initiate-session session params)
    (-when-let (ret (ob-ipython--eval
                     (ob-ipython--execute-request
                      (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                                     params (org-babel-variable-assignments:python params))
                      (ob-ipython--normalize-session session))))
      (let ((result (cdr (assoc :result ret)))
            (output (cdr (assoc :output ret))))
        (if (eq result-type 'output)
            (concat
             output
             (format "%s"
                     (mapconcat 'identity
                                (loop for res in result
                                      if (eq 'image/png (car res))
                                      collect (ob-ipython-inline-image (cdr res)))
                                "\n")))
          (ob-ipython--create-stdout-buffer output)
          (cond ((and file (string= (f-ext file) "png"))
                 (->> result (assoc 'image/png) cdr (ob-ipython--write-base64-string file)))
                ((and file (string= (f-ext file) "svg"))
                 (->> result (assoc 'image/svg+xml) cdr (ob-ipython--write-string-to-file file)))
                (file (error "%s is currently an unsupported file extension." (f-ext file)))
                (t (->> result (assoc 'text/plain) cdr))))))))

;; copy org text as rich text
(defun org-formatted-copy ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "xclip -selection clipboard -t 'text/html' -i"))
      (kill-buffer buf))))
;;(global-set-key (kbd "C-c e") 'org-formatted-copy)

;; Tab should do indent in code blocks
(setq org-src-tab-acts-natively t)

;; Don't remove (or add) any extra whitespace
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

;;; Some helper function to manage org-babel sessions

(defun src-block-in-session-p (&optional name)
  "Return if src-block is in a session of NAME.
NAME may be nil for unnamed sessions."
  (let* ((info (org-babel-get-src-block-info))
         (lang (nth 0 info))
         (body (nth 1 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params))))

    (cond
     ;; unnamed session, both name and session are nil
     ((and (null session)
           (null name))
      t)
     ;; Matching name and session
     ((and
       (stringp name)
       (stringp session)
       (string= name session))
      t)
     ;; no match
     (t nil))))

(defun org-babel-restart-session-to-point (&optional arg)
  "Restart session up to the src-block in the current point.
Goes to beginning of buffer and executes each code block with
`org-babel-execute-src-block' that has the same language and
session as the current block. ARG has same meaning as in
`org-babel-execute-src-block'."
  (interactive "P")
  (unless (org-in-src-block-p)
    (error "You must be in a src-block to run this command"))
  (let* ((current-point (point-marker))
         (info (org-babel-get-src-block-info))
         (lang (nth 0 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        ;; goto start of block
        (goto-char (match-beginning 0))
        (let* ((this-info (org-babel-get-src-block-info))
               (this-lang (nth 0 this-info))
               (this-params (nth 2 this-info))
               (this-session (cdr (assoc :session this-params))))
          (when
              (and
               (< (point) (marker-position current-point))
               (string= lang this-lang)
               (src-block-in-session-p session))
            (org-babel-execute-src-block arg)))
        ;; move forward so we can find the next block
        (forward-line)))))

(defun org-babel-kill-session ()
  "Kill session for current code block."
  (interactive)
  (unless (org-in-src-block-p)
    (error "You must be in a src-block to run this command"))
  (save-window-excursion
    (org-babel-switch-to-session)
    (kill-buffer)))

(defun org-babel-remove-result-buffer ()
  "Remove results from every code block in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-babel-src-block-regexp nil t)
      (org-babel-remove-result))))


;; this adds a "new language" in babel that gets exported as js in html
;; https://www.reddit.com/r/orgmode/comments/5bi6ku/tip_for_exporting_javascript_source_block_to/
(add-to-list 'org-src-lang-modes '("inline-js" . javascript))
(defvar org-babel-default-header-args:inline-js
  '((:results . "html")
    (:exports . "results")))
(defun org-babel-execute:inline-js (body _params)
  (format "<script type=\"text/javascript\">\n%s\n</script>" body))

;; dot == graphviz-dot
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

;; Add 'conf-mode' to org-babel
(add-to-list 'org-src-lang-modes '("conf" . conf))

;; add all languages to org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (R . t)
   (asymptote)
   (awk)
   (calc)
   (clojure)
   (comint)
   (css)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (fortran)
   (gnuplot . t)
   (haskell)
   (io)
   (java)
   (js)
   (latex)
   (ledger . t)
   (lilypond)
   (lisp)
   (lua)
   (matlab)
   (maxima)
   (mscgen)
   (ocaml)
   (octave . t)
   (org . t)
   (perl)
   (picolisp)
   (plantuml)
   (python . t)
   (ipython . t)
   (restclient . t)
   (ref)
   (ruby)
   (sass)
   (scala)
   (scheme)
   (screen)
   (shell . t)
   (shen)
   (sql . t)
   (sqlite)))

;;; dakra-org.el ends here
