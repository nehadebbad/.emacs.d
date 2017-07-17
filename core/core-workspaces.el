(use-package persp-mode
  :config
  (with-eval-after-load "persp-mode-autoloads"
      (setq wg-morph-on nil) ;; switch off animation
      (setq persp-autokill-buffer-on-remove 'kill-weak)
      (setq persp-keymap-prefix "C-c w")
      (add-hook 'after-init-hook #'(lambda () (persp-mode 1)))))

(provide 'core-workspaces)
