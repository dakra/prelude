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
   ))

;; Install newest org and org-plus-contrib packages
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-directory "~/org/")

(setq org-agenda-files '("~/org"))

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-use-fast-todo-selection t)

(setq org-default-notes-file (concat org-directory "refile.org"))

(setq org-clock-idle-time 10)  ; idle after 10 minutes

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

(require 'org-protocol)
;; org-capture chrome plugin: https://chrome.google.com/webstore/detail/org-capture/kkkjlfejijcjgjllecmnejhogpbcigdc?hl=en

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      `(("t" "todo" entry (file ,(concat org-directory "refile.org"))
         "* TODO %?\n%U\n" :clock-in t :clock-resume t)
        ("T" "todo with link" entry (file ,(concat org-directory "refile.org"))
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("r" "respond" entry (file ,(concat org-directory "refile.org"))
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
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
         "* %? [[%:link][%:description]] \nCaptured On: %U")
        ("h" "Habit" entry (file ,(concat org-directory "refile.org"))
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

;; Capturing task often is <1min and results in empty drawer.
;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))
(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


;; use utf-8 characters instead of `*` as bullet points
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


(setq org-confirm-babel-evaluate nil)  ; don't prompt me to confirm everytime I want to evaluate a block

;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(setq org-src-fontify-natively t)  ; syntax highlighting for source code blocks

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

(require 'org-pomodoro)

(defun dakra/org-pomodoro-i3-bar-time ()
  "Display remaining pomodoro time in i3 status bar."
  (interactive)
  (with-temp-file "~/.emacs.d/.pomodoro"
    (if (org-pomodoro-active-p)
        (insert (format "Pomodoro: %d minutes" (/ org-pomodoro-countdown 60)))
      (insert "No active pomodoro"))))

(setq org-pomodoro-tick-hook 'dakra/org-pomodoro-i3-bar-time)

(setq org-pomodoro-finished-hook 'dakra/org-pomodoro-i3-bar-time)
(setq org-pomodoro-killed-hook 'dakra/org-pomodoro-i3-bar-time)



;; Tab should do indent in code blocks
(setq org-src-tab-acts-natively t)


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


;; dot == graphviz-dot
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

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
   (sh . t)
   (shen)
   (sql . t)
   (sqlite)))

;;; dakra-org.el ends here
