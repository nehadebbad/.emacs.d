(setq helm-prevent-escaping-from-minibuffer t
        helm-bookmark-show-location t
        helm-display-header-line nil
        helm-split-window-in-side-p t
        helm-always-two-windows t
        helm-echo-input-in-header-line t
        helm-imenu-execute-action-at-once-if-one nil
        helm-org-format-outline-path t
        helm-display-function 'spacemacs//display-helm-window)
  (with-eval-after-load 'helm
    (spacemacs|hide-lighter helm-mode)
    (when (and dotspacemacs-helm-resize
               (or (eq dotspacemacs-helm-position 'bottom)
                   (eq dotspacemacs-helm-position 'top)))
      (setq helm-autoresize-min-height 10)
      (helm-autoresize-mode 1))
    ;; setup hooks
    (add-hook 'helm-minibuffer-set-up-hook
              'spacemacs//helm-hide-minibuffer-maybe)
    (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)
    (spacemacs/add-to-hook 'helm-after-initialize-hook
                           '(spacemacs//prevent-minibuffer-escape
                             spacemacs//hide-cursor-in-helm-buffer))
    (add-hook 'helm-cleanup-hook #'spacemacs//unprevent-minibuffer-escape)
    (add-hook 'helm-find-files-before-init-hook
              'spacemacs//set-dotted-directory)
    (add-hook 'spacemacs-editing-style-hook 'spacemacs//helm-hjkl-navigation)
    ;; setup advices
    ;; fuzzy matching for all the sourcess
    (unless (eq dotspacemacs-helm-use-fuzzy 'source)
      (advice-add 'helm-make-source :around #'spacemacs//helm-make-source)
