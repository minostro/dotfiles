;;; init-local.el --- My Personal Local Configurations
;;
;;; Code:

;; window numbering starts here
;;(window-numbering-mode)
;; window numbering ends here

;; delete section starts here
(delete-selection-mode 1) ;;delete selected region with backspace key
;; delete section ends here

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
;;(require 'init-gcal)
;; org-gcal ends here

;; org-mode starts here
(setq org-src-fontify-natively t)

;;; Org Bullets settings
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;; Agenda settings
(setq org-agenda-files (list
                        "~/org/my-life.org"
                        "~/org/triage.org"))

;;; Refile settings
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

;;; To-do settings
(setq org-todo-keywords
      (quote ((sequence "TODO(t/!)" "NEXT(n/!)" "PROGRESS(p/!)" "WAITING(w@/!)" "ON HOLD(h@/!)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "PROJECT(j)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face)
              ("PROGRESS" :inherit font-lock-string-face)
              ("WAITING" :inherit font-lock-negation-char-face))))

(setq org-archive-location "~/org/archive.org::")
;;; Capturing settings
(setq org-capture-templates
      (quote
       (("t" "todo" entry
         (file+headline "~/org/triage.org" "TODOS")
         "* TODO %?
:PROPERTIES:
:CAPTURED_AT: %U
:END:
" :clock-resume t)
        ("c" "code todo" entry
         (file+headline "~/org/triage.org" "TODOS")
         "* TODO %?
:PROPERTIES:
:REFERENCE: [[file://%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))][code at: %f]]
:CAPTURED_AT: %U
:CODE:
#+BEGIN_SRC ruby
%i
#+END_SRC
:END:
" :clock-resume t)
        ("n" "note" entry
         (file+headline "~/org/my-life.org" "NOTES")
         "* %? :NOTE:
:PROPERTIES:
:CAPTURED AT: %U
:END:
" :clock-resume t))))
;; org-mode ends here

;; set-faces starts here
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#2d2d2d" :foreground "#cccccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Monaco")))))
;; set-faces ends here

;; java starts here
(require 'eclim)
(setq eclimd-autostart nil)
(add-hook 'java-mode-hook (lambda () (eclim-mode t)))

;; Dirs configuration
(custom-set-variables
 '(eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse"))
 '(eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim"))

;; Displaying compilation errors in the echo area
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; Configuring company mode
(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)
;;(company-emacs-eclim-ignore-case t)
;; java ends here

;; Adding utf-8 alias
(define-coding-system-alias 'UTF-8 'utf-8)

;; Enabling flyspell mode
(flyspell-mode t)

;; Enabling yafolding mode
(require 'yafolding)
(add-hook 'json-mode (lambda () ((yafolding-mode t))))
(global-set-key (kbd "C-c j t") 'yafolding-toggle-element)
(global-set-key (kbd "C-c j s") 'yafolding-show-element)
(global-set-key (kbd "C-c j a t") 'yafolding-toggle-all)
(global-set-key (kbd "C-c j a s") 'yafolding-show-all)

(provide 'init-local)
;;; init-local.el ends here
