(package! key-chord)
(package! org-roam)
(package! beacon)

(package! treesit-auto)
(package! astro-ts-mode)
(package! format-all)
(package! sudo-edit)
(package! eradio)

(when (modulep! +lsp)
  (package! lsp-tailwindcss
    :recipe (:host github :repo "merrickluo/lsp-tailwindcss")))
