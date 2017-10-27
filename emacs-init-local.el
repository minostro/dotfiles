;;; init-local.el --- My Personal Local Configurations
;;
;;; Code:

;; window numbering starts here
(window-numbering-mode)
;; window numbering ends here

;; copy/paste starts here
;; shamelessly copy from http://iancmacdonald.com/macos/emacs/tmux/2017/01/15/macOS-tmux-emacs-copy-past.html
(defun copy-from-osx ()
  "Use OSX clipboard to paste."
  (shell-command-to-string "reattach-to-user-namespace pbpaste"))

(defun paste-to-osx (text &optional push)
  "Add kill ring entries (TEXT) to OSX clipboard.  PUSH."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "reattach-to-user-namespace" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)
;; copy/paste ends here

;; multiple-cursors starts here
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
;; multiple-cursors ends here

;; org-gcal starts here
(require 'init-gcal)
;; org-gcal ends here

;; org-mode starts here
(setq org-src-fontify-natively t)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-agenda-files (list "~/org/gcal.org"))
;; org-mode ends here

(provide 'init-local)
;;; init-local.el ends here
