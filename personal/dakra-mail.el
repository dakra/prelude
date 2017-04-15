;;; package --- dakra-mail

;;; Commentary:
;;; mu4e email config

;;; Code:

(require 'prelude-packages nil 'noerror)
(prelude-require-packages
 '(helm-mu))

;; mu package (includes mu4e) must be installed in the system
(require 'mu4e)

;; for org capture
(require 'org-mu4e)

;; Store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)

;; Open mu4e with the 'Mail' key (if your keyboard has one)
(global-set-key (kbd "<XF86Mail>") 'mu4e)

;; set mu4e as default mail client
(setq  mail-user-agent 'mu4e-user-agent)

(defalias 'org-mail 'org-mu4e-compose-org-mode)

;; when mail is sent, automatically convert org body to HTML
(setq org-mu4e-convert-to-html t)

;; FIXME: only set this during mu4e usage
(setq org-export-with-toc nil)  ; turn off table of contents


;; Show additional user-agent header
(setq mu4e-view-fields
      '(:from :to :cc :subject :flags :date :maildir :user-agent :mailing-list :tags :attachments :signature :decryption))

;; Auto sign mails
(add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign-pgpmime)
;; Encrypt mails by calling (mml-secure-message-encrypt-pgpmime)


;; default
(setq mu4e-maildir "~/Maildir")

(setq mu4e-drafts-folder "/private/Drafts")
(setq mu4e-sent-folder   "/private/Sent")
(setq mu4e-trash-folder  "/private/Trash")

;; Dynamically refile
;; See: https://www.djcbsoftware.nl/code/mu/mu4e/Smart-refiling.html#Smart-refiling
(defun dakra-mu4e-private-refile (msg)
  (cond
   ;; refile all messages from Uber to the 'uber' folder
   ((mu4e-message-contact-field-matches msg :from "@uber\\.com")
    "/private/uber")
   ;; important to have a catch-all at the end!
   (t  "/private/Archive")))

(setq mu4e-refile-folder 'dakra-mu4e-private-refile)

;; don't show duplicate mails when searching
(setq mu4e-headers-skip-duplicates t)

;; show email address as well and not only the name
(setq mu4e-view-show-addresses t)

;; Don't show related messages by default.
;; Activate with 'W' on demand
(setq mu4e-headers-include-related nil)

;; use helm-mu for search
;; FIXME helm search broken
;; default search only inbox, archive or sent mail
;;(setq helm-mu-default-search-string "(maildir:/private/Inbox OR maildir:/private/Archive OR maildir:/private/Sent)")
;;(define-key mu4e-main-mode-map "s" 'helm-mu)
;;(define-key mu4e-headers-mode-map "s" 'helm-mu)
;;(define-key mu4e-view-mode-map "s" 'helm-mu)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '(("/private/Inbox"      . ?i)
        ("/private/Sent"       . ?s)
        ("/private/Trash"      . ?t)
        ("/private/Drafts"     . ?d)
        ("/private/Archive"   . ?a)))

;; Don't ask to quit
(setq mu4e-confirm-quit nil)

;; Don't spam the minibuffer with 'Indexing...' messages
(setq mu4e-hide-index-messages t)

;; gmail delete == move mail to trash folder
(fset 'my-move-to-trash "mt")
(define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
(define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)
;; Overwrite normal 'D' keybinding
(define-key mu4e-headers-mode-map (kbd "D") 'my-move-to-trash)
(define-key mu4e-view-mode-map (kbd "D") 'my-move-to-trash)

;; Mark all as read with 'M'
(define-key mu4e-headers-mode-map (kbd "M") 'mu4e-headers-mark-all-unread-read)

;; Add some mailing lists
(add-to-list 'mu4e~mailing-lists '("intern.lists.entropia.de" . "Entropia"))
(add-to-list 'mu4e~mailing-lists '("intern.lists.ccc.de" . "CCC"))
(add-to-list 'mu4e~mailing-lists '("pylons-discuss.googlegroups.com" . "Pyramid"))
(add-to-list 'mu4e~mailing-lists '("pylons-devel.googlegroups.com" . "Pyramid"))

(setq mu4e-bookmarks `(("maildir:/private/Inbox OR maildir:/gmail/inbox OR maildir:/atomx/inbox OR maildir:/hogaso/inbox OR maildir:/e5/Inbox" "All inboxes" ?i)
                       ("flag:flagged" "Flagged messages" ?f)
                       (,(concat "flag:unread AND "
                                 "NOT flag:trashed AND "
                                 "NOT flag:seen AND "
                                 "NOT maildir:/atomx/spam AND "
                                 "NOT maildir:/atomx/trash AND "
                                 "NOT maildir:/gmail/spam AND "
                                 "NOT maildir:/gmail/trash")
                        "Unread messages" ?a)
                       ("list:magit@googlegroups.com OR list:mu-discuss@googlegroups.com" "Elisp" ?e)
                       ("list:pylons-discuss@googlegroups.com OR list:pylons-devel@googlegroups.com OR list:sqlalchemy@googlegroups.com" "Python" ?p)
                       ("list:intern.lists.ccc.de" "CCC Intern" ?c)
                       ("list:intern.lists.entropia.de" "Entropia Intern" ?k)
                       ("list:uwsgi.lists.unbit.it" "uwsgi" ?u)))

;; (add-hook 'mu4e-mark-execute-pre-hook
;;           (lambda (mark msg)
;;             (cond ((member mark '(refile trash)) (mu4e-action-retag-message msg "-\\Inbox"))
;;                   ((equal mark 'flag) (mu4e-action-retag-message msg "\\Starred"))
;;                   ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

;; allow for updating mail using 'U' in the main view:
;; (only update inboxes)
(setq mu4e-get-mail-command "mbsync private gmail-inbox atomx-inbox hogaso-inbox e5-inbox")
;; for update all:
;;(setq mu4e-get-mail-command "mbsync -a")

;; update database every ten minutes
(setq  mu4e-update-interval (* 60 10))

;; We do a full index (that verify integrity) with a systemd job
;; Go fast inside emacs
(setq mu4e-index-cleanup nil      ;; don't do a full cleanup check
      mu4e-index-lazy-check t)    ;; don't consider up-to-date dirs

;;; Use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;;; Save attachment (this can also be a function)
(setq mu4e-attachment-dir "~/Downloads")

;; Attach files from dired (C-c RET C-a)
(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; I want my format=flowed thank you very much
;; mu4e sets up visual-line-mode and also fill (M-q) to do the right thing
;; each paragraph is a single long line; at sending, emacs will add the
;; special line continuation characters.
(setq mu4e-compose-format-flowed nil)

;; Dont open new frame for composing mails
(setq mu4e-compose-in-new-frame nil)

;; display html messages
(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)
(add-hook 'mu4e-view-mode-hook
          (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))
(setq shr-color-visible-luminance-min 80)

;; Don't reply to self
(setq mu4e-user-mail-address-list
      '("daniel@kraus.my" "daniel.kraus@gmail.com" "dakra@tr0ll.net" "daniel@tr0ll.net" "d@niel-kraus.de"
        "arlo@kraus.my"
        "dakra-cepheus@tr0ll.net"
        "daniel@atomx.com"
        "daniel@hogaso.com"
        "daniel.kraus@ebenefuenf.de"))
(setq mu4e-compose-dont-reply-to-self t)

;; Extract name from email for yasnippet template
;; http://pragmaticemacs.com/emacs/email-templates-in-mu4e-with-yasnippet/
(defun bjm/mu4e-get-names-for-yasnippet ()
  "Return comma separated string of names for an email"
  (interactive)
  (let ((email-name "") str email-string email-list email-name2 tmpname)
    (save-excursion
      (goto-char (point-min))
      ;; first line in email could be some hidden line containing NO to field
      (setq str (buffer-substring-no-properties (point-min) (point-max))))
    ;; take name from TO field - match series of names
    (when (string-match "^To: \"?\\(.+\\)" str)
      (setq email-string (match-string 1 str)))
    ;;split to list by comma
    (setq email-list (split-string email-string " *, *"))
    ;;loop over emails
    (dolist (tmpstr email-list)
      ;;get first word of email string
      (setq tmpname (car (split-string tmpstr " ")))
      ;;remove whitespace or ""
      (setq tmpname (replace-regexp-in-string "[ \"]" "" tmpname))
      ;;join to string
      (setq email-name
            (concat email-name ", " tmpname)))
    ;;remove initial comma
    (setq email-name (replace-regexp-in-string "^, " "" email-name))

    ;;see if we want to use the name in the FROM field
    ;;get name in FROM field if available, but only if there is only
    ;;one name in TO field
    (if (< (length email-list) 2)
        (when (string-match "^\\([^ ,\n]+\\).+writes:$" str)
          (progn (setq email-name2 (match-string 1 str))
                 ;;prefer name in FROM field if TO field has "@"
                 (when (string-match "@" email-name)
                   (setq email-name email-name2))
                 )))
    email-name))

;; Always display plain text messages.
(setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)

(setq mu4e-msg2pdf "/usr/bin/msg2pdf")  ; to display html messages as pdf

;; View mail in browser with "a V"
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions
             '("xViewXWidget" . mu4e-action-view-with-xwidget) t)
;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)

(defun mu4e-message-maildir-matches (msg rx)
  "Match message MSG with regex RX based on maildir."
  (when rx
    (if (listp rx)
        ;; if rx is a list, try each one for a match
        (or (mu4e-message-maildir-matches msg (car rx))
            (mu4e-message-maildir-matches msg (cdr rx)))
      ;; not a list, check rx
      (string-match rx (mu4e-message-field msg :maildir)))))


(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "private"
           :enter-func (lambda () (mu4e-message "Switch to the Private context"))
           ;; leave-func not defined
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-maildir-matches msg "^/private")))
           :vars '(  ( user-mail-address  . "daniel@kraus.my"  )
                     ( mu4e-maildir-shortcuts . (("/private/Inbox"      . ?i)
                                                 ("/private/Sent"       . ?s)
                                                 ("/private/Trash"      . ?t)
                                                 ("/private/Drafts"     . ?d)
                                                 ("/private/Archive"   . ?a)))
                     ( mu4e-drafts-folder . "/private/Drafts" )
                     ( mu4e-sent-folder   . "/private/Sent" )
                     ( mu4e-trash-folder  . "/private/Trash" )
                     ( mu4e-refile-folder . dakra-mu4e-private-refile)
                     ( user-full-name     . "Daniel Kraus" )
                     ( mu4e-compose-signature .
                                              (concat
                                               "regards,\n"
                                               "  Daniel\n"))))
         ,(make-mu4e-context
           :name "gmail"
           :enter-func (lambda () (mu4e-message "Switch to the gmail context"))
           ;; leave-func not defined
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-maildir-matches msg "^/gmail")))
           :vars '(  ( user-mail-address  . "daniel.kraus@gmail.com"  )
                     ( mu4e-maildir-shortcuts . (("/gmail/inbox"      . ?i)
                                                 ("/gmail/sent_mail"  . ?s)
                                                 ("/gmail/trash"      . ?t)
                                                 ("/gmail/drafts"     . ?d)
                                                 ("/gmail/all_mail"   . ?a)))
                     ( mu4e-drafts-folder . "/gmail/drafts" )
                     ( mu4e-sent-folder   . "/gmail/sent_mail" )
                     ( mu4e-trash-folder  . "/gmail/trash" )
                     ( mu4e-refile-folder . "/gmail/all_mail" )
                     ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                     ( mu4e-sent-messages-behavior  . delete)
                     ( user-full-name     . "Daniel Kraus" )
                     ( mu4e-compose-signature .
                                              (concat
                                               "regards,\n"
                                               "  Daniel\n"))))
         ,(make-mu4e-context
           :name "atomx"
           :enter-func (lambda () (mu4e-message "Switch to the Atomx context"))
           ;; leave-fun not defined
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-maildir-matches msg "^/atomx")))
           :vars '(  ( user-mail-address  . "daniel@atomx.com" )
                     ( mu4e-maildir-shortcuts . (("/atomx/inbox"      . ?i)
                                                 ("/atomx/sent_mail"  . ?s)
                                                 ("/atomx/trash"      . ?t)
                                                 ("/atomx/drafts"     . ?d)
                                                 ("/atomx/all_mail"   . ?a)))
                     ( mu4e-drafts-folder . "/atomx/drafts" )
                     ( mu4e-sent-folder   . "/atomx/sent_mail" )
                     ( mu4e-trash-folder  . "/atomx/trash" )
                     ( mu4e-refile-folder . "/atomx/all_mail" )
                     ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                     ( mu4e-sent-messages-behavior  . delete)
                     ( user-full-name     . "Daniel Kraus" )
                     ( mu4e-compose-signature . (concat
                                                 "Daniel Kraus\n"
                                                 "Atomx | https://atomx.com\n"))))

         ,(make-mu4e-context
           :name "e5"
           :enter-func (lambda () (mu4e-message "Switch to the e5 context"))
           ;; leave-fun not defined
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-maildir-matches msg "^/e5")))
           :vars '(  ( user-mail-address  . "daniel.kraus@ebenefuenf.de" )
                     ( mu4e-maildir-shortcuts . (("/e5/Inbox"      . ?i)
                                                 ("/e5/Sent"  . ?s)
                                                 ("/e5/Trash"      . ?t)
                                                 ("/e5/Drafts"     . ?d)
                                                 ("/e5/Archive"    . ?a)))
                     ( mu4e-drafts-folder . "/e5/Drafts" )
                     ( mu4e-sent-folder   . "/e5/Sent" )
                     ( mu4e-trash-folder  . "/e5/Trash" )
                     ( mu4e-refile-folder . "/e5/Archive" )
                     ( user-full-name     . "Daniel Kraus" )
                     ( mu4e-compose-signature . (concat
                                                 "Daniel Kraus\n"
                                                 "https://ebenefuenf.de\n"
                                                 "ebene fünf GmbH\n"
                                                 "Amselweg 6\n"
                                                 "96173 Oberhaid/Staffelbach\n"
                                                 "---\n"
                                                 "Sitz der Gesellschaft: Staffelbach\n"
                                                 "Amtsgericht Bamberg - HRB 6233\n"
                                                 "USt-IdNr. DE 263246988\n"))))

         ,(make-mu4e-context
           :name "hogaso"
           :enter-func (lambda () (mu4e-message "Switch to the Hogaso context"))
           ;; leave-fun not defined
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-maildir-matches msg "^/hogaso")))
           :vars '(  ( user-mail-address  . "daniel@hogaso.com" )
                     ( mu4e-maildir-shortcuts . (("/hogaso/inbox"      . ?i)
                                                 ("/hogaso/sent_mail"  . ?s)
                                                 ("/hogaso/trash"      . ?t)
                                                 ("/hogaso/drafts"     . ?d)
                                                 ("/hogaso/all_mail"   . ?a)))
                     ( mu4e-drafts-folder . "/hogaso/drafts" )
                     ( mu4e-sent-folder   . "/hogaso/sent_mail" )
                     ( mu4e-trash-folder  . "/hogaso/trash" )
                     ( mu4e-refile-folder . "/hogaso/all_mail" )
                     ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                     ( mu4e-sent-messages-behavior  . delete)
                     ( user-full-name     . "Daniel Kraus" )
                     ( mu4e-compose-signature . (concat
                                                 "Daniel Kraus\n"
                                                 "Hogaso | https://hogaso.com\n"))))))

;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
(setq mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask
'(setq mu4e-compose-context-policy nil)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; something about ourselves
;; (setq
;;  user-mail-address "daniel@kraus.my"
;;  user-full-name  "Daniel Kraus"
;;  mu4e-compose-signature
;;  (concat
;;   "regards,\n"
;;   "  Daniel\n"))

;; sending mail (with msmtp)
(setq send-mail-function 'sendmail-send-it
      sendmail-program "~/bin/msmtp-enqueue.sh"
      mail-specify-envelope-from t
      message-sendmail-f-is-evil nil
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)


;; If there's 'attach' 'file' 'pdf' in the message warn when sending w/o attachment
(defun mbork/message-attachment-present-p ()
  "Return t if an attachment is found in the current message."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (search-forward "<#part" nil t) t))))

(defcustom mbork/message-attachment-intent-re
  (regexp-opt '("attach"
		"file"
                "pdf"))
  "A regex which - if found in the message, and if there is no
attachment - should launch the no-attachment warning.")

(defcustom mbork/message-attachment-reminder
  "Are you sure you want to send this message without any attachment? "
  "The default question asked when trying to send a message
containing `mbork/message-attachment-intent-re' without an
actual attachment.")

(defun mbork/message-warn-if-no-attachments ()
  "Ask the user if s?he wants to send the message even though
there are no attachments."
  (when (and (save-excursion
	       (save-restriction
		 (widen)
		 (goto-char (point-min))
		 (re-search-forward mbork/message-attachment-intent-re nil t)))
	     (not (mbork/message-attachment-present-p)))
    (unless (y-or-n-p mbork/message-attachment-reminder)
      (keyboard-quit))))

(add-hook 'message-send-hook #'mbork/message-warn-if-no-attachments)
