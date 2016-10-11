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

;; default
(setq mu4e-maildir "~/.mail/gmail/")

(setq mu4e-drafts-folder "/drafts")
(setq mu4e-sent-folder   "/sent_mail")
(setq mu4e-trash-folder  "/trash")
(setq mu4e-refile-folder "/all_mail")

;; default search only inbox or sent mail
(setq helm-mu-default-search-string "(maildir:/inbox OR maildir:/sent_mail)")

;; don't show duplicate mails when searching
(setq mu4e-headers-skip-duplicates t)

;; use helm-mu for search
(define-key mu4e-main-mode-map "s" 'helm-mu)
(define-key mu4e-headers-mode-map "s" 'helm-mu)
(define-key mu4e-view-mode-map "s" 'helm-mu)

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
      '( ("/inbox"      . ?i)
         ("/sent_mail"  . ?s)
         ("/trash"      . ?t)
         ("/all_mail"   . ?a)))

(fset 'my-move-to-trash "mt")
(define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
(define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)

;; gmail delete == move mail to trash folder
(fset 'my-move-to-trash "mt")
(define-key mu4e-headers-mode-map (kbd "D") 'my-move-to-trash)
(define-key mu4e-view-mode-map (kbd "D") 'my-move-to-trash)

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
(setq mu4e-get-mail-command "mbsync gmail")


;; display html messages
(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)
(add-hook 'mu4e-view-mode-hook
          (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))
(setq shr-color-visible-luminance-min 80)


(setq mu4e-msg2pdf "/usr/bin/msg2pdf")  ; to display html messages as pdf

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)


;; something about ourselves
(setq
 user-mail-address "daniel.kraus@gmail.com"
 user-full-name  "Daniel Kraus"
 mu4e-compose-signature
 (concat
  "regards,\n"
  "  Daniel\n"))

;; sending mail
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "daniel.kraus@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)


;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
