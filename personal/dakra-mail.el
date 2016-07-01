;;; package --- dakra-mail

;;; Commentary:
;;; mu4e email config

;;; Code:

(require 'prelude-packages nil 'noerror)
(prelude-require-packages
 '(helm-mu))

;; mu package (includes mu4e) must be installed in the system
(require 'mu4e)


;; default
(setq mu4e-maildir "~/.mail/gmail/")

(setq mu4e-drafts-folder "/[Gmail]/.Drafts")
(setq mu4e-sent-folder   "/[Gmail]/.Sent Mail")
(setq mu4e-trash-folder  "/[Gmail]/.Trash")

;; default search only inbox or sent mail
(setq helm-mu-default-search-string "(maildir:/Inbox OR maildir:/[Gmail]/.Sent Mail)")

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
      '( ("/Inbox"               . ?i)
         ("/[Gmail]/.Sent Mail"   . ?s)
         ("/[Gmail]/.Trash"       . ?t)
         ("/[Gmail]/.All Mail"    . ?a)))

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

;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; something about ourselves
(setq
 user-mail-address "daniel.kraus@gmail.com"
 user-full-name  "Daniel Kraus"
 mu4e-compose-signature
 (concat
  "regards,\n"
  "  Daniel\n"))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

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
