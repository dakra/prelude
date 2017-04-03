;;(setq prelude-theme 'sanityinc-tomorrow-bright)
(setq prelude-theme nil)

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

;; multi cursor
(setq mc/list-file "~/.emacs.d/personal/.mc-lists.el")
