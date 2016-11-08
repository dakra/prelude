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

;; Open mu4e with the 'Mail' key (if your keyboard has one)
(global-set-key (kbd "<XF86Mail>") 'mu4e)

;; set mu4e as default mail client
(setq  mail-user-agent 'mu4e-user-agent)

(defalias 'org-mail 'org-mu4e-compose-org-mode)

;; when mail is sent, automatically convert org body to HTML
(setq org-mu4e-convert-to-html t)

;; FIXME: only set this during mu4e usage
(setq org-export-with-toc nil)  ; turn off table of contents


;; default
(setq mu4e-maildir "~/.mail")

(setq mu4e-drafts-folder "/gmail/drafts")
(setq mu4e-sent-folder   "/gmail/sent_mail")
(setq mu4e-trash-folder  "/gmail/trash")
(setq mu4e-refile-folder "/gmail/all_mail")

;; default search only inbox or sent mail
(setq helm-mu-default-search-string "(maildir:/gmail/inbox OR maildir:/gmail/sent_mail)")

;; don't show duplicate mails when searching
(setq mu4e-headers-skip-duplicates t)

;; show email address as well and not only the name
(setq mu4e-view-show-addresses t)

;; use helm-mu for search
;; FIXME helm search broken
;;(define-key mu4e-main-mode-map "s" 'helm-mu)
;;(define-key mu4e-headers-mode-map "s" 'helm-mu)
;;(define-key mu4e-view-mode-map "s" 'helm-mu)

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '(("/gmail/inbox"      . ?i)
        ("/atomx/inbox"      . ?a)
        ("/e5/inbox"         . ?e)
        ("/hogaso/inbox"     . ?h)
        ("/gmail/sent_mail"  . ?s)
        ("/gmail/trash"      . ?t)
        ("/gmail/all_mail"   . ?A)))

(fset 'my-move-to-trash "mt")
(define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
(define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)

;; gmail delete == move mail to trash folder
(fset 'my-move-to-trash "mt")
(define-key mu4e-headers-mode-map (kbd "D") 'my-move-to-trash)
(define-key mu4e-view-mode-map (kbd "D") 'my-move-to-trash)

(setq mu4e-bookmarks `(("maildir:/gmail/inbox OR maildir:/atomx/inbox OR maildir:/hogaso/inbox OR maildir:/e5/inbox" "All inboxes" ?i)
                       ("flag:flagged" "Flagged messages" ?f)))
;; (setq mu4e-bookmarks `(("\\\\Inbox" "Inbox" ?i)
;;                        ("flag:flagged" "Flagged messages" ?f)
;;                        (,(concat "flag:unread AND "
;;                                  "NOT flag:trashed AND "
;;                                  "NOT maildir:/spam AND "
;;                                  "NOT maildir:/trash")
;;                         "Unread messages" ?u)))

;; (add-hook 'mu4e-mark-execute-pre-hook
;;           (lambda (mark msg)
;;             (cond ((member mark '(refile trash)) (mu4e-action-retag-message msg "-\\Inbox"))
;;                   ((equal mark 'flag) (mu4e-action-retag-message msg "\\Starred"))
;;                   ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

;; allow for updating mail using 'U' in the main view:
;; (only update inboxes)
(setq mu4e-get-mail-command "mbsync gmail-inbox atomx-inbox hogaso-inbox e5-inbox")
;; for update all:
;;(setq mu4e-get-mail-command "mbsync -a")

;; update database every ten minutes
(setq  mu4e-update-interval (* 60 10))


;;; Use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;;; Save attachment (this can also be a function)
(setq mu4e-attachment-dir "~/Downloads")


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
      '("daniel.kraus@gmail.com" "dakra@tr0ll.net"
        "daniel@atomx.com" "daniel@hogaso.com" "daniel.kraus@ebenefuenf.de"))
(setq mu4e-compose-dont-reply-to-self t)

;; Always display plain text messages.
(setq mu4e-view-html-plaintext-ratio-heuristic 30)

(setq mu4e-msg2pdf "/usr/bin/msg2pdf")  ; to display html messages as pdf

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
                           (mu4e-message-maildir-matches msg "^/gmail")))
           :vars '(  ( user-mail-address  . "daniel.kraus@gmail.com"  )
                     ( smtpmail-smtp-user . "daniel.kraus@gmail.com" )
                     ( smtpmail-smtp-server . "smtp.gmail.com" )
                     ( smtpmail-auth-credentials . '(("smtp.gmail.com" 587 "daniel.kraus@gmail.com" nil)) )
                     ( mu4e-maildir-shortcuts . (("/gmail/inbox"      . ?i)
                                                 ("/e5/inbox"         . ?e)
                                                 ("/atomx/inbox"      . ?a)
                                                 ("/hogaso/inbox"     . ?h)
                                                 ("/gmail/sent_mail"  . ?s)
                                                 ("/gmail/trash"      . ?t)
                                                 ("/gmail/all_mail"   . ?A)))
                     ( mu4e-drafts-folder . "/gmail/drafts" )
                     ( mu4e-sent-folder   . "/gmail/sent_mail" )
                     ( mu4e-trash-folder  . "/gmail/trash" )
                     ( mu4e-refile-folder . "/gmail/all_mail" )
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
                     ( smtpmail-smtp-user . "daniel@atomx.com" )
                     ( smtpmail-smtp-server . "smtp.gmail.com" )
                     ( smtpmail-auth-credentials . '(("smtp.gmail.com" 587 "daniel@atomx.com" nil)) )
                     ( mu4e-maildir-shortcuts . (("/atomx/inbox"      . ?a)
                                                 ("/e5/inbox"         . ?e)
                                                 ("/gmail/inbox"      . ?i)
                                                 ("/hogaso/inbox"     . ?h)
                                                 ("/atomx/sent_mail"  . ?s)
                                                 ("/atomx/trash"      . ?t)
                                                 ("/atomx/all_mail"   . ?a)))
                     ( mu4e-drafts-folder . "/atomx/drafts" )
                     ( mu4e-sent-folder   . "/atomx/sent_mail" )
                     ( mu4e-trash-folder  . "/atomx/trash" )
                     ( mu4e-refile-folder . "/atomx/all_mail" )
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
                     ( smtpmail-smtp-user . "daniel.kraus@ebenefuenf.de" )
                     ( smtpmail-smtp-server . "smtp.fastmail.com" )
                     ( smtpmail-auth-credentials . '(("smtp.fastmail.com" 587 "daniel.kraus@ebenefuenf.de" nil)) )
                     ( mu4e-maildir-shortcuts . (("/atomx/inbox"   . ?a)
                                                 ("/e5/inbox"      . ?e)
                                                 ("/gmail/inbox"   . ?i)
                                                 ("/hogaso/inbox"  . ?h)
                                                 ("/e5/sent_mail"  . ?s)
                                                 ("/e5/trash"      . ?t)
                                                 ("/e5/archive"    . ?a)))
                     ( mu4e-drafts-folder . "/e5/drafts" )
                     ( mu4e-sent-folder   . "/e5/sent_mail" )
                     ( mu4e-trash-folder  . "/e5/trash" )
                     ( mu4e-refile-folder . "/e5/archive" )
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
                     ( smtpmail-smtp-user . "daniel@hogaso.com" )
                     ( smtpmail-smtp-server . "smtp.gmail.com" )
                     ( smtpmail-auth-credentials . '(("smtp.gmail.com" 587 "daniel@hogaso.com" nil)) )
                     ( mu4e-maildir-shortcuts . (("/atomx/inbox"      . ?a)
                                                 ("/e5/inbox"         . ?e)
                                                 ("/gmail/inbox"      . ?i)
                                                 ("/hogaso/inbox"     . ?h)
                                                 ("/hogaso/sent_mail"  . ?s)
                                                 ("/hogaso/trash"      . ?t)
                                                 ("/hogaso/all_mail"   . ?a)))
                     ( mu4e-drafts-folder . "/hogaso/drafts" )
                     ( mu4e-sent-folder   . "/hogaso/sent_mail" )
                     ( mu4e-trash-folder  . "/hogaso/trash" )
                     ( mu4e-refile-folder . "/hogaso/all_mail" )
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

;; something about ourselves
;; (setq
;;  user-mail-address "daniel.kraus@gmail.com"
;;  user-full-name  "Daniel Kraus"
;;  mu4e-compose-signature
;;  (concat
;;   "regards,\n"
;;   "  Daniel\n"))

;; sending mail
(require 'smtpmail)
;;; Allow queuing mails
(setq smtpmail-queue-mail  nil  ;; start in non-queuing mode
      smtpmail-queue-dir   "~/.mail/queue/")
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-user "daniel.kraus@gmail.com"
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "daniel.kraus@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)


;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)


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
