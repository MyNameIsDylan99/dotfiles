(setq doom-theme 'doom-one)

(setq display-line-numbers-type t)
(setq-default truncate-lines t) ; in allen Modi keine Zeilen umbrechen

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(after! org
(setq org-directory "~/notes/")
(setq org-agenda-files
      '("~/notes/"
        "~/notes/Life/GTD/Projects/"
        "~/pCloudDrive/Portfolio/"
        "~/notes/daily/"))
(setq org-log-done 'time))

;; Schriftgrößen: skaliert Text sauber proportional zur UI
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 36))
(setq doom-variable-pitch-font (font-spec :family "Noto Sans" :size 38))
(setq doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 48)) ;; z.B. für Präsentationsmodus

(after! doom-modeline
  (setq doom-modeline-height 45)) ;; Default ist 25

;;Exit insert mode by pressing j and then j quickly
(use-package! key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))

(after! lsp-mode
  (setq
        lsp-enable-snippet nil     ;; optional
        lsp-enable-symbol-highlighting t
        lsp-signature-auto-activate t))

(add-hook 'csharp-mode-hook #'lsp!)

(setq org-roam-directory "~/notes/")

(use-package! org-roam
  :init
  (setq org-roam-v2-ack t) ;; wichtig!
  :custom
  (org-roam-directory (file-truename "~/notes/"))
  :config
  (org-roam-db-autosync-enable))


(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %?" :target
         (file+head "%<%Y-%m-%d>.org"
                    "#+title: %<%Y-%m-%d>\n"))))
(map! :leader
      :prefix ("n r" . "org-roam") ;; n = notes, r = roam
      :desc "Find node" "f" #'org-roam-node-find
      :desc "Insert node" "i" #'org-roam-node-insert
      :desc "Show graph" "g" #'org-roam-graph
      :desc "Capture to node" "c" #'org-roam-capture
      :desc "Today (dailies)" "t" #'org-roam-dailies-capture-today
      :desc "Yesterday" "y" #'org-roam-dailies-capture-yesterday
      :desc "Tomorrow" "m" #'org-roam-dailies-capture-tomorrow
      :desc "Find daily" "d" #'org-roam-dailies-goto-date)

;;Smooth scrolling
(pixel-scroll-precision-mode 1)
(defun my-smooth-scroll (lines)
  "Smoothly scroll by LINES (positive or negative), keeping the cursor in view."
  (let ((step (if (< lines 0) -1 1))
        (remaining (abs lines)))
    (dotimes (_ remaining)
      (scroll-up-line step)
      (ignore-errors
        (forward-line step)) ;; bewegt den Cursor mit
      (sit-for 0.01))))

(map! :n "C-d"
      (lambda () (interactive)
        (my-smooth-scroll (/ (window-body-height) 2)))
      :n "C-u"
      (lambda () (interactive)
        (my-smooth-scroll (- (/ (window-body-height) 2)))))

(use-package! beacon
  :config
  (beacon-mode 1))

;; Beispiel für Org Mode Heading Fonts
(set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
(set-face-attribute 'org-level-2 nil :height 1.3 :weight 'bold)
(set-face-attribute 'org-level-3 nil :height 1.2 :weight 'bold)
(set-face-attribute 'org-level-4 nil :height 1.1 :weight 'bold)

;;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;;(require 'mu4e)
;;(require 'smtpmail)
;;(setq user-mail-address "fresshnes@gmail.com"
;;      user-full-name "Dylan Frosini"
;;      mu4e-get-mail-command "mbsync -c ~/.config/mu4e/mbysyncrc -a"
;;      mu4e-update-interval 300
;;      message-signature
;;      (concat
;;       "Dylan Frosini\n")
;;      message-send-mail-function 'smtpmail-send-it
;;      starttls-use-gnuls t
;;      smtpmail-starttls-credentials '(("smtp.land1.com" 587 nil nil))
;;      smtpmail-auth-credentials '(("smtp.land1.com" 587 "fresshnes@gmail.com" nil))
;;      smtpmail-default-smtp-server "smtp.land1.com"
;;      smtpmail-smtp-service 587
;;      mu4e-sent-folder "/Sent"
;;      mu4e-drafts-folder "/Drafts"
;;      mu4e-trash-folder "/Trash"
;;      mu4e-refile-folder "/All Mail"
;;      mu4e-maildir-shortcuts
;;      '(("/gmail/INBOX"     . ?i)
;;        ("/gmail/Sent"      . ?s)
;;        ("/gmail/All Mail"  . ?a)
;;        ("/gmail/Trash"     . ?t)))
