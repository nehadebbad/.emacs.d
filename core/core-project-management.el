(use-package counsel-projectile
  :defer t
  :config
  (counsel-projectile-on)
  (message "counsel-projectile loaded")
  :bind (("C-c p SPC" . counsel-projectile)))

(use-package neotree
  :defer t
  :init
  (defhydra hydra-neotree (:hint nil
                                   :pre  (neotree-dir (projectile-project-root))
                                   :post neotree-hide
                                   :color pink)
    ("r" neotree-rename-node "rename")
    ("w" neotree-copy-node "copy")
    ("n" neotree-create-node "new")
    ("d" neotree-delete-node "delete")
    ("i" neotree-previous-line)
    ("k" neotree-next-line)
    ("h" neotree-hidden-file-toggle "hidden-files")
    ("s" neotree-stretch-toggle "stretch")
    ("f" neotree-quick-look "quick-look")
    ("e" neotree-enter "select")
    ("q" nil "quit"))
  (global-set-key (kbd "C-c n") 'hydra-neotree/body)
  :config
  (use-package all-the-icons)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(provide 'core-project-management)
