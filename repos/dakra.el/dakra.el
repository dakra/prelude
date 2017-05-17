;;; dakra.el --- Minor mode and functions for personal use
;;
;; Copyright © 2017 Daniel Kraus
;;
;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/dot-emacs
;; Version: 0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Minor mode and functions for personal use

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(defvar dakra-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'crux-open-with)
    (define-key map (kbd "C-c g") 'dakra-google)
    (define-key map (kbd "C-c G") 'dakra-github)
    (define-key map (kbd "C-c y") 'dakra-youtube)
    (define-key map (kbd "C-c U") 'dakra-duckduckgo)
    ;; mimic popular IDEs binding, note that it doesn't work in a terminal session
    (define-key map (kbd "C-a") 'crux-move-beginning-of-line)
    (define-key map [(shift return)] 'crux-smart-open-line)
    (define-key map (kbd "M-o") 'crux-smart-open-line)
    (define-key map [(control shift return)] 'crux-smart-open-line-above)
    (define-key map [(control shift up)]  'move-text-up)
    (define-key map [(control shift down)]  'move-text-down)
    (define-key map [(meta shift up)]  'move-text-up)
    (define-key map [(meta shift down)]  'move-text-down)
    (define-key map (kbd "C-c n") 'crux-cleanup-buffer-or-region)
    (define-key map (kbd "C-c f")  'crux-recentf-ido-find-file)
    (define-key map (kbd "C-M-z") 'crux-indent-defun)
    (define-key map (kbd "C-c u") 'crux-view-url)
    (define-key map (kbd "C-c e") 'crux-eval-and-replace)
    (define-key map (kbd "C-c s") 'crux-swap-windows)
    (define-key map (kbd "C-c D") 'crux-delete-file-and-buffer)
    (define-key map (kbd "C-c d") 'crux-duplicate-current-line-or-region)
    (define-key map (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
    (define-key map (kbd "C-c r") 'crux-rename-buffer-and-file)
    (define-key map (kbd "C-c t") 'crux-visit-term-buffer)
    (define-key map (kbd "C-c k") 'crux-kill-other-buffers)
    (define-key map (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)
    (define-key map (kbd "C-c I") 'crux-find-user-init-file)
    (define-key map (kbd "C-c S") 'crux-find-shell-init-file)
    (define-key map (kbd "C-c i") 'imenu-anywhere)
    ;; extra prefix for projectile
    (define-key map (kbd "s-p") 'projectile-command-map)
    ;; make some use of the Super key
    (define-key map (kbd "s-g") 'god-local-mode)
    (define-key map (kbd "s-r") 'crux-recentf-ido-find-file)
    (define-key map (kbd "s-j") 'crux-top-join-line)
    (define-key map (kbd "s-k") 'crux-kill-whole-line)
    (define-key map (kbd "s-m m") 'magit-status)
    (define-key map (kbd "s-m l") 'magit-log)
    (define-key map (kbd "s-m f") 'magit-log-buffer-file)
    (define-key map (kbd "s-m b") 'magit-blame)
    (define-key map (kbd "s-o") 'crux-smart-open-line-above)

    map)
  "Keymap for dakra-mode.")



;; define minor mode
(define-minor-mode dakra-mode
  "Minor mode for my personal keymap.

\\{dakra-mode-map}"
  :keymap dakra-mode-map)

(define-globalized-minor-mode dakra-global-mode dakra-mode dakra-on)

(defun dakra-on ()
  "Turn on `dakra-mode'."
  (dakra-mode +1))

(defun dakra-off ()
  "Turn off `dakra-mode'."
  (dakra-mode -1))

(defun dakra-toggle-browser ()
  (interactive)
  (if (eq browse-url-browser-function 'eww-browse-url)
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "firefox")
    (setq browse-url-browser-function 'eww-browse-url)))

;;;###autoload
(defun dakra-kill-this-buffer ()
  "Like (kill-this-buffer) but independent of the menu bar."
  (interactive)
  (kill-buffer (current-buffer)))

(defmacro dakra-define-up/downcase-dwim (case)
  (let ((func (intern (concat "dakra-" case "-dwim")))
        (doc (format "Like `%s-dwim' but %s from beginning when no region is active." case case))
        (case-region (intern (concat case "-region")))
        (case-word (intern (concat case "-word"))))
    `(defun ,func (arg)
       ,doc
       (interactive "*p")
       (save-excursion
         (if (use-region-p)
             (,case-region (region-beginning) (region-end))
           (beginning-of-thing 'symbol)
           (,case-word arg)))
       )))
(dakra-define-up/downcase-dwim "upcase")
(dakra-define-up/downcase-dwim "downcase")
(dakra-define-up/downcase-dwim "capitalize")

(defun dakra-other-window-or-frame (count)
  "Call `other-window' if more than one window is visible, `other-frame' otherwise."
  (interactive)
  (if (one-window-p)
      (other-frame count)
    (other-window count)))

(defun dakra-next-window-or-frame ()
  "Focus next window or frame."
  (dakra-other-window-or-frame 1))

(defun dakra-previous-window-or-frame ()
  "Focus previous window or frame."
  (dakra-other-window-or-frame -1))

(provide 'dakra)
;;; dakra.el ends here
