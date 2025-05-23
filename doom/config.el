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

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'smtpmail)
(setq user-mail-address "dylan.frosini@outlook.com"
      user-full-name "Dylan Frosini"
      mu4e-get-mail-command "mbsync -c ~/.config/mu4e/mbysyncrc -a"
      mu4e-update-interval 300
      message-signature
      (concat
       "Dylan Frosini\n")
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnuls t
      smtpmail-starttls-credentials '(("smtp.land1.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.land1.com" 587 "dylan.frosini@outlook.com" nil))
      smtpmail-default-smtp-server "smtp.land1.com"
      smtpmail-smtp-service 587
      mu4e-sent-folder "/Sent"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/All Mail"
      mu4e-maildir-shortcuts
      '(("/Outlook/Inbox"     . ?i)
        ("/Outlook/Sent"      . ?s)
        ("/Outlook/All Mail"  . ?a)
        ("/Outlook/Trash"     . ?t)))
