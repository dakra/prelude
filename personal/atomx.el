;;; package --- Atomx package
;;
;;; Commentary:
;; Update atomx auth token for Emacs restclient.
;;
;;; Code:

(defun atomx-replace-restclient-auth-token (new-auth-token)
  "Replace the `:auth-token` value in the current buffer with NEW-AUTH-TOKEN."
  (set-buffer atomx-restclient-api-buffer)
  (let (current-point del-start-point del-end-point)
    (setq current-point (point))
    (goto-char (point-min))
    (re-search-forward "^:auth-token")
    (skip-chars-forward " =")
    (setq del-start-point (point))
    (setq del-end-point (line-end-position))
    (delete-region del-start-point del-end-point)
    (insert new-auth-token)
    (goto-char current-point)
    ))

(defun atomx-update-restclient-auth-token ()
  "Update the current buffer with a new api auth-token."
  (interactive)
  ;; load the password
  (require 'dakra-passwords "~/.emacs.d/personal/dakra-passwords.el.gpg")
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")
           ("User-Agent" . "Emacs Login")))
        url-request-data
        current-point
        del-start-point
        auth-token
        api-buffer
        api-endpoint
        api-url
        api-password
        )
    (setq current-point (point))
    (goto-char (point-min))
    (re-search-forward "^:api")
    (skip-chars-forward " =")
    (setq api-endpoint (buffer-substring (point) (line-end-position)))
    (setq api-password (cdr (assoc api-endpoint dakra-atomx-passwords)))
    (setq url-request-data (concat "{\"email\":\"daniel@atomx.com\",\"password\":\"" api-password "\"}"))
    (setq api-url (concat api-endpoint "/login"))
    (setq atomx-restclient-api-buffer (current-buffer))
    (goto-char current-point)
    (url-retrieve api-url
                  (lambda (status)
                    (search-forward "\"auth_token\":\"")
                    (setq del-start-point (point))
                    (skip-chars-forward "^\"")
                    (setq auth-token (buffer-substring del-start-point (point)))
                    ;;(kill-buffer (current-buffer))
                    ;;(set-buffer api-buffer)
                    (atomx-replace-restclient-auth-token auth-token)
                    ))))

(provide 'atomx)
;;; atomx.el ends here
