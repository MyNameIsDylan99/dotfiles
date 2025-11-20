;; -*- lexical-binding: t; -*-
(setq doom-theme 'doom-dark+)

(setq display-line-numbers-type t)
(setq-default truncate-lines t) ; in allen Modi keine Zeilen umbrechen


(defun my/org-find-files(dirs)
  "Sammelt alle .org Dateien in den angegebenen DIRS (rekursiv)."
  (apply #'append
         (mapcar (lambda (dir)
                   (directory-files-recursively dir "\\.org$"))
                 dirs)))

(after! org
  (setq org-agenda-files
        (my/org-find-files '("~/notes/"
                             "~/pCloudDrive/Portfolio/"
                             "~/Git/Bewerben/")))
  (setq org-log-done 'time))

;; Schriftgrößen: skaliert Text sauber proportional zur UI
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 26))
(setq doom-variable-pitch-font (font-spec :family "Noto Sans" :size 30))
(setq doom-big-font (font-spec :family "FiraCode Nerd Font, Medium" :size 28)) ;; z.B. für Präsentationsmodus

(after! doom-modeline
  (setq doom-modeline-height 45)) ;; Default ist 25

;;Exit insert mode by pressing j and then j quickly
(use-package! key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))

(use-package! lsp-mode
  :commands lsp lsp-deferred)

(after! lsp-mode
  (setq
   ;;lsp-enable-snippet nil     ;; optional
   lsp-enable-symbol-highlighting t
   lsp-signature-auto-activate t))

(setq treesit-font-lock-level 4)

(use-package! csharp-ts-mode
  :after lsp-mode
  :mode ("\.cs$")
  :hook (csharp-ts-mode . lsp-deferred))

(after! csharp-ts-mode
  (setq-local treesit-font-lock-settings
              (append csharp-ts-mode--font-lock-settings
                      (treesit-font-lock-rules
                       :language 'c-sharp
                       :feature 'type
                       '((variable_declarator
                          (invocation_expression
                           function: (member_access_expression
                                      expression: (identifier) @font-lock-type-face)))))))
  (treesit-major-mode-setup)
  (font-lock-flush))

(setq org-roam-directory "~/notes/")

(use-package! org-roam
  :init
  (setq org-roam-v2-ack t) ;; wichtig!
  :custom
  (org-roam-directory (file-truename "~/notes/"))
  :config
  (org-roam-db-autosync-enable))

;; Standard-Template für Dailies
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n#+author: Dylan\n\n* Bewerben\n\n* WishOfHers\n\n* Emacs\n\n* Fiverr\n\n* Brainrot\n\n* Rollstuhlsimulator\n\n* Pause"))))

;; (setq org-roam-dailies-capture-templates
;;       '(("d" "default" entry "* %?" :target
;;          (file+head "%<%Y-%m-%d>.org"
;;                     "#+title: %<%Y-%m-%d>\n"))))
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
;; (defun my-smooth-scroll (lines)
;;   "Smoothly scroll by LINES (positive or negative), keeping the cursor in view."
;;   (let ((step (if (< lines 0) -1 1))
;;         (remaining (abs lines)))
;;     (dotimes (_ remaining)
;;       (scroll-up-line step)
;;       (ignore-errors
;;         (forward-line step)) ;; bewegt den Cursor mit
;;       (sit-for 0.01))))

;; (map! :n "C-d"
;;       (lambda () (interactive)
;;         (my-smooth-scroll (/ (window-body-height) 2)))
;;       :n "C-u"
;;       (lambda () (interactive)
;;         (my-smooth-scroll (- (/ (window-body-height) 2)))))

(pixel-scroll-precision-mode 1)
(use-package! beacon
  :config
  (beacon-mode 1))

(after! beacon
  (defun my/beacon-blink-after-scroll (&rest _)
    (beacon-blink))

  (advice-add 'evil-scroll-up :after #'my/beacon-blink-after-scroll)
  (advice-add 'evil-scroll-down :after #'my/beacon-blink-after-scroll))

;; Beispiel für Org Mode Heading Fonts
(set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
(set-face-attribute 'org-level-2 nil :height 1.3 :weight 'bold)
(set-face-attribute 'org-level-3 nil :height 1.2 :weight 'bold)
(set-face-attribute 'org-level-4 nil :height 1.1 :weight 'bold)

(use-package! treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package! astro-ts-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-ts-mode))

  (add-hook 'astro-ts-mode-hook #'lsp! 'append)

  :config
  (let ((astro-recipe (make-treesit-auto-recipe
                       :lang 'astro
                       :ts-mode 'astro-ts-mode
                       :url "https://github.com/virchau13/tree-sitter-astro"
                       :revision "master"
                       :source-dir "src")))

    (add-to-list 'treesit-auto-recipe-list astro-recipe)))

(add-to-list 'exec-path (expand-file-name "~/.dotnet/tools"))
(setenv "PATH" (concat (expand-file-name "~/.dotnet/tools") ":" (getenv "PATH")))

(after! apheleia
  (set-formatter! 'prettier-astro
    '("npx" "prettier" "--parser=astro"
      "--config" "/home/dylan/.config/prettier/.prettierrc.mjs"
      (apheleia-formatters-indent "--use-tabs" "--tab-width" 'astro-ts-mode-indent-offset))
    :modes '(astro-ts-mode))

  (setf (alist-get 'astro-mode apheleia-mode-alist)
        'prettier-astro)
  (setf (alist-get 'bash-ts-mode apheleia-mode-alist)
        'shfmt)
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt"))
  (setf (alist-get 'sh-mode apheleia-mode-alist)
        'shfmt)

  ;; Formatter mit csharpier definieren
  ;; (setf (alist-get 'csharpier apheleia-formatters)
  ;;       '("clang-format" "--assume-filename=foo.cs"))

  (setf (alist-get 'csharpier apheleia-formatters)
        '("csharpier" "format" filepath "--write-stdout"))
  ;; C#-Mode dem Formatter zuordnen
  (setf (alist-get 'csharp-ts-mode apheleia-mode-alist)
        'csharpier)

  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" filepath))
  ;; C++-Mode dem Formatter zuordnen
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist)
        'clang-format)

  ;; Optional: Apheleia global aktivieren
  (apheleia-global-mode +1)
  (setq apheleia-log-formatter-errors t)

  )

(use-package! lsp-tailwindcss
  :when (modulep! +lsp)
  :init
  (setq! lsp-tailwindcss-add-on-mode t)
  :config
  (add-to-list 'lsp-tailwindcss-major-modes 'astro-ts-mode))

;; MDX Support
(add-to-list 'auto-mode-alist '("\\.\\(mdx\\)$" . markdown-mode))
(when (modulep! +lsp)
  (add-hook 'markdown-mode-local-vars-hook #'lsp! 'append))

;; Doom Dashboard
(setq fancy-splash-image
      (expand-file-name "splashscreens/doom-emacs-cats-small.png" doom-user-dir))

(defun my-doom-dashboard-cat-banner ()
  (let* ((banner '("                      /^--^\\     /^--^\\     /^--^\\"
                   "                      \\____/     \\____/     \\____/"
                   "                     /      \\   /      \\   /      \\"
                   "KAT                 |        | |        | |        |"
                   "                     \\__  __/   \\__  __/   \\__  __/"
                   "|^|^|^|^|^|^|^|^|^|^|^|^\\ \\^|^|^|^/ /^|^|^|^|^\\ \\^|^|^|^|^|^|^|^|^|^|^|^|"
                   "| | | | | | | | | | | | |\\ \\| | |/ /| | | | | | \\ \\ | | | | | | | | | | |"
                   "########################/ /######\\ \\###########/ /#######################"
                   "| | | | | | | | | | | | \\/| | | | \\/| | | | | |\\/ | | | | | | | | | | | |"
                   "|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'my-doom-dashboard-cat-banner)

(after! yasnippet
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets/" doom-user-dir)))

(defun my/vterm-dired-here ()
  "Öffne dired im aktuellen Arbeitsverzeichnis des vterm-Buffers."
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (let ((dir (vterm--get-directory (current-buffer))))
        (if (and dir (stringp dir))
            (dired dir)
          (message "Verzeichnis konnte nicht ermittelt werden. Shell-Tracking aktiv?")))
    (message "Dies ist kein vterm-Buffer.")))

(use-package! emms
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players))

(defun dylan/emms-add-directory-with-default ()
  "Frage nach einem Verzeichnis und schlage ~/pCloudDrive/Music vor."
  (interactive)
  (let ((dir (read-directory-name "Verzeichnis zur Playlist hinzufügen: " "~/pCloudDrive/Music")))
    (emms-add-directory-tree dir)))

(map! :leader
      (:prefix ("e" . "emms")
       :desc "EMMS starten" "e" #'emms
       :desc "Verzeichnis auswählen (pCloud default)" "d" #'dylan/emms-add-directory-with-default))

(map! :leader
      (:prefix ("n" . "navigation")
       :desc "Remove highlights" "h" #'evil-ex-nohighlight))

;;eradio
(use-package! eradio
  :init
  (setq eradio-player '("mpv" "--no-video" "--no-terminal"))
  :config
  (setq eradio-channels '(("def con - soma fm" . "https://somafm.com/defcon256.pls")          ;; electronica with defcon-speaker bumpers
                          ("cyberia - lainon"  . "https://lainon.life/radio/cyberia.ogg.m3u") ;; cyberpunk-esque electronica
                          ("Synphaera"     . "https://somafm.com/synphaera256.pls")  ;; Electronic, ambient, minimal
                          ("Music on Lush"     . "https://somafm.com/lush.pls")  ;; Pretty voice, mellow electronica-tinged music
                          ("Deep Space One"     . "https://somafm.com/deepspaceone.pls")  ;; Deep ambient, electronic, experimental space music
                          ("Radio 6"     . "http://as-hls-ww-live.akamaized.net/pool_81827798/live/ww/bbc_6music/bbc_6music.isml/bbc_6music-audio=96000.norewind.m3u8")  ;; Deep ambient, electronic, experimental space music
                          ("Radio Energy Zürich"     . "https://energyzuerich.ice.infomaniak.ch/energyzuerich-high.mp3?ua=energy%2Bwebsite%2Bdesktop")  ;; Deep ambient, electronic, experimental space music
                          ("SRF 3"     . "https://livestreaming-node-3.srg-ssr.ch/srgssr/srf3/aac/96")  ;; Deep ambient, electronic, experimental space music
                          ("Vaporwaves"     . "https://somafm.com/vaporwaves.pls")))  ;; Waves of synthesizers, drum machines samplers and saxophones
  )

(map! :leader (:prefix ("r" . "eradio") :desc "Play a radio channel" "p" 'eradio-play))
(map! :leader (:prefix ("r" . "eradio") :desc "Stop the radio player" "s" 'eradio-stop))
(map! :leader (:prefix ("r" . "eradio") :desc "Toggle the radio player" "t" 'eradio-toggle))

(after! which-key
  (map! :leader
        "<f2>" #'which-key-C-h-dispatch
        "ö" #'which-key-show-next-page-cycle
        "ä" #'which-key-show-previous-page-cycle)
  )
