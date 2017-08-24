;; User different elpa dirs for emacs stable and unstable
;;(setq package-user-dir (format "%selpa_%s/"
;;  user-emacs-directory emacs-major-version)) ; default = ~/.emacs.d/elpa/

;;(setq prelude-theme 'sanityinc-tomorrow-bright)
(setq prelude-theme nil)

(add-to-list 'load-path "~/.emacs.d/personal/external")

;;(setq prelude-theme 'moe-dark)
;;(setq prelude-theme 'doom-one)
;; brighter minibuffer when active
;;(add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
;;(setq doom-one-brighter-comments t
;;      doom-one-brighter-modeline t)



(setq org-list-allow-alphabetical t)

(setq org-emphasis-alist '(("*" bold)
                           ("/" italic)
                           ("_" underline)
                           ("=" org-verbatim verbatim)
                           ("`" org-code verbatim)
                           ("~" org-code verbatim)
                           ("+" (:strike-through t))))

(require 'recentf)
(add-to-list 'recentf-keep 'file-remote-p)

;; Use outshine prefix for outline-minor-mode
(defvar outline-minor-mode-prefix "\M-#")

;; save multiple cursers under /personal
(setq mc/list-file "~/.emacs.d/personal/.mc-lists.el")
