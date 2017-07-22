(use-package adaptive-wrap
  :config
  (progn
    (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)))

(use-package hide-comnt
  :commands hide/show-comments-toggle
  :init (my/set-leader-keys "ch" 'hide/show-comments-toggle))

(use-package highlight-indentation
  :defer t
  :init
  (progn
    (my|add-toggle highlight-indentation
                          :mode highlight-indentation-mode
                          :documentation "Highlight indentation levels."
                          :evil-leader "thi")
    (my|add-toggle highlight-indentation-current-column
                          :mode highlight-indentation-current-column-mode
                          :documentation "Highlight indentation level at point."
                          :evil-leader "thc")))

(use-package highlight-numbers
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'highlight-numbers-mode)
    (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1)))))

(use-package highlight-parentheses
  :defer t
  :init
  (progn
    (when (member dotspacemacs-highlight-delimiters '(all current))
      (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
    (setq hl-paren-delay 0.2)
    (my/set-leader-keys "thp" 'highlight-parentheses-mode)
    (setq hl-paren-colors '("Springgreen3"
                            "IndianRed1"
                            "IndianRed3"
                            "IndianRed4")))
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))

(use-package hl-anything
  :init
  (progn
    (hl-highlight-mode)
    (setq-default hl-highlight-save-file
                  (concat spacemacs-cache-directory ".hl-save"))
    (my/set-leader-keys
     "hc"  'hl-unhighlight-all-local
     "hC"  'hl-unhighlight-all-global
     "hh"  'hl-highlight-thingatpt-local
     "hH"  'hl-highlight-thingatpt-global
     "hn"  'hl-find-next-thing
     "hN"  'hl-find-prev-thing
     "hr"  'hl-restore-highlights
     "hs"  'hl-save-highlights)))

(use-package rainbow-delimiters
  :defer t
  :init
  (progn
    (my/set-leader-keys "tCd" 'rainbow-delimiters-mode)
    (when (member dotspacemacs-highlight-delimiters '(any all))
      (my/add-to-hooks 'rainbow-delimiters-mode '(prog-mode-hook)))))

(use-package volatile-highlights
  :config
  (progn
    ;; additional extensions
    ;; evil
    (vhl/define-extension 'evil
                          'evil-move
                          'evil-paste-after
                          'evil-paste-before
                          'evil-paste-pop)
    (with-eval-after-load 'evil
      (vhl/install-extension 'evil)
      (vhl/load-extension 'evil))
    ;; undo-tree
    (vhl/define-extension 'undo-tree
                          'undo-tree-move
                          'undo-tree-yank)
    (with-eval-after-load 'undo-tree
      (vhl/install-extension 'undo-tree)
      (vhl/load-extension 'undo-tree))
    (volatile-highlights-mode)))

(provide 'core-editing-visual)
