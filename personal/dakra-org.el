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
   org-bullets
   org-pomodoro
   ))

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "gtd.org"))

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      `(("t" "todo" entry (file+headline ,(concat org-directory "gtd.org") "Inbox")
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("r" "respond" entry (file+headline ,(concat org-directory "gtd.org") "Inbox")
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("n" "note" entry (file+headline ,(concat org-directory "gtd.org") "Inbox")
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("w" "org-protocol" entry (file+headline ,(concat org-directory "gtd.org") "Inbox")
         "* TODO Review %c\n%U\n" :immediate-finish t)
        ("m" "Meeting" entry (file+headline ,(concat org-directory "gtd.org") "Inbox")
         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
        ("P" "Phone call" entry (file+headline ,(concat org-directory "gtd.org") "Inbox")
         "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
        ("p" "Protocol" entry (file+headline ,(concat org-directory "gtd.org") "Inbox")
         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
        ("L" "Protocol Link" entry (file+headline ,(concat org-directory "gtd.org") "Inbox")
         "* %? [[%:link][%:description]] \nCaptured On: %U")
        ("h" "Habit" entry (file+headline ,(concat org-directory "gtd.org") "Inbox")
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

;; use utf-8 characters instead of `*` as bullet points
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-protocol)
;; org-capture chrome plugin: https://chrome.google.com/webstore/detail/org-capture/kkkjlfejijcjgjllecmnejhogpbcigdc?hl=en
(setq org-capture-templates `(
                              ("p" "Protocol" entry (file+headline ,(concat org-directory "gtd.org") "Inbox")
                               "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                              ("L" "Protocol Link" entry (file+headline ,(concat org-directory "gtd.org") "Inbox")
                               "* %? [[%:link][%:description]] \nCaptured On: %U")
                              ))


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
   (sql)
   (sqlite)))

;;; dakra-org.el ends here
